-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Language.SO.Pack where

import Language.SO.Code
import Language.SO.ToHeader(commals)
import Language.Slim.Data(Size, SizeNative(SizeNative))
import Data.List ( sortBy, intersperse )
import Language.SO.Args
import Language.Slim.Serialize
import Text.PrettyPrint hiding (Mode)
import Debug.Trace(trace)
import Prelude hiding ( (<>) )
dbg ff = (show ff) `trace` ff

type IsExtParams = Bool
type FarfLevel = String
type FarfMessage = String
type FarfArg = String

countF :: [SerealArg] -> Function
countF sereal = Function
         ""
         "static __inline void"
         "_count"
         (map AS $ "int _numIn[1]" : "int _numROut[1]" : "int _numInH[1]" : "int _numROutH[1]" : (map (("_ATTRIBUTE_UNUSED " ++). render) $ declareRefsl sereal ))
         (  [ er $ text "_numIn[0] += " <> int (numSecondaryBufs In sereal) <> semi
            , er $ text "_numROut[0] += " <> int (numSecondaryBufs ROut sereal) <> semi
            , er $ text "_numInH[0] += " <> int (numHandles In sereal) <> semi
            , er $ text "_numROutH[0] += " <> int (numHandles ROut sereal) <> semi]
      ++    (concatMap countBufs sereal))

countBufs :: SerealArg -> [Code]
countBufs (ComplexArg arg (ComplexStruct _) _ args) = [E $ countBuf ]
   where
      countBuf = countF args `CE` ((tes ["_numIn", "_numROut", "_numInH", "_numROutH"]) ++ (callOffsets (ptr arg) args))
countBufs (ComplexArg arg (ComplexSeq szal _ _) _ args) =
   seqLoop [] [] [] arg szal $ \ _ _ nat ->  [E $ countF args `CE` ((tes ["_numIn", "_numROut", "_numInH", "_numROutH"]) ++ (callOffsets nat args)) ]
countBufs _ = []

ifcrout sereal aa
   | (hasComplexROut sereal) = aa
   | otherwise = []

skelCopyIR :: Origin -> SerealArg -> [Code]
skelCopyIR _ sa
   | False == saIsIR sa = []
skelCopyIR _ (ScalarArg _ _ (Prim In _ _) _) = []
skelCopyIR _ (ScalarArg arg tt pp@(Prim ROut _ _) _) = copy pp arg tt
skelCopyIR _ (BufferArg _ (SequenceBuf _ _ _ _) (Buf In _)) = []
skelCopyIR og (BufferArg arg (SequenceBuf sz _ _ _) buf@(Buf ROut vv)) =
         [ dpv og buf ]
   ++    assert ((seqlenval $ arg) `igteq` (seqlenval $ iarg arg))
   ++    [ er $ text "_MEMMOVEIF" `call` [seqptrval arg, seqptrvall $ iarg $ arg, (text "(size_t)" <> parens (seqlenval $ iarg arg)) `mult` size sz] <> semi ]
skelCopyIR og (ComplexArg _ _ _ _) = error "inrout complex arguments not supported"


skelUnpack :: Origin -> SerealArg -> IsExtParams -> [Code]
skelUnpack _ (ScalarArg arg tt pp@(Prim In _ _) _) _ = unpack pp arg tt
skelUnpack _ (ScalarArg _ _ (Prim ROut _ _) _) _ = []
skelUnpack og (BufferArg arg (SequenceBuf sz True _ _) buf@(Buf In _)) _ =
         [ dpv og buf ]
   ++    assert ((nLen buf `divd` size sz) `igteq` (seqlenval arg))
   ++    [ er $ seqptrval arg `eq` pv buf <> semi ]
   ++    assert (parens ((seqlenval arg) `gt` int 0) `annd` (parens (((seqptrval arg) <> (brackets ((seqlenval arg)  `minus` (int 1))) `eqq` int 0))))

skelUnpack og (BufferArg arg (SequenceBuf sz _ _ _) buf) _ =
         [ dpv og buf ]
   ++    assert ((nLen buf `divd` size sz) `igteq` (seqlenval arg))
   ++    [ er $ seqptrval arg `eq` pv buf <> semi ]
skelUnpack og (ComplexArg arg (ComplexStruct _) prims args) iep =
         [ D $ CD (allocator "" "")
         , D $ CD $ ppraIn og
         , D $ CD $ ppraROut og
         , D $ CD $ ppraHIn og
         , D $ CD $ ppraHROut og ]
   ++    (try $ skelUnpackF og args iep `CE` (ctxStruct prims arg ++ (callOffsets (ptr arg) args)))
skelUnpack og (ComplexArg arg (ComplexSeq sz _ _) prims args) iep =
   let   pr md = filter ((==) md  . primMode) prims
   in    [ D $ CD (allocator "" "")
         , D $ CD $ ppraIn og
         , D $ CD $ ppraROut og
         , D $ CD $ ppraHIn og
         , D $ CD $ ppraHROut og ]
   ++    assertSize (seqlenval arg) (pr In)
   ++    assertSize (seqlenval arg) (pr ROut)
   ++    alloc ((text "(size_t)" <> parens (seqlenval arg)) `mult` sizeNative sz) (aligNative sz)  (seqptrval arg)
   ++    (seqLoop og (pr In) (pr ROut) arg sz $ \ prin prout nat ->
            (try $ skelUnpackF og args iep `CE` (ctxPrim prin prout arg ++ (callOffsets nat args))))
skelUnpack og oo@(ObjArg arg obj@(Obj In _)) _ =
         [ er $ text "_COPY" `call` [ptr arg, int 0, ph obj, int 0, text "sizeof(remote_handle64)"] <> semi ]
skelUnpack og (ObjArg _ (Obj ROut _)) _ = []
skelUnpack og oo@(DmahandleArg arg _ obj@(Obj _ _)) iep =
         [ sdmah og obj iep
         , er $ text "_COPY" `call` [ptr arg <> text "Fd", int 0, phfd og obj, int 0, text "sizeof(uint32_t)"] <> semi
         , er $ text "_COPY" `call` [ptr arg <> text "Offset", int 0, phoffset og obj, int 0, text "sizeof(uint32_t)"] <> semi ]
     ++  assert (text "adsp_mmap_fd_getinfo" `call` [ text "*" <> ptr arg <> text "Fd", ptr arg <> text "Len"] `eqq` (int 0))
     ++  [ inch og obj ]

skelUnpackCopy :: Origin -> SerealArg -> [Code]
skelUnpackCopy og oo@(DmahandleArg arg _ obj@(Obj _ _)) =
         [  er $ text "_COPY" `call` [ptr arg <> text "Fd", int 0, phfd og obj, int 0, text "sizeof(uint32_t)"] <> semi
         ,  er $ text "_COPY" `call` [ptr arg <> text "Offset", int 0, phoffset og obj, int 0, text "sizeof(uint32_t)"] <> semi ]
     ++  assert (text "adsp_mmap_fd_getinfo" `call` [ text "*" <> ptr arg <> text "Fd", ptr arg <> text "Len"] `eqq` (int 0))
     ++  [ inch og obj ]
skelUnpackCopy _ _ = []

assertSize :: Doc -> [Primary] -> [Code]
assertSize len [(Prim md bufix sz)] = assert (((nLen (Buf md bufix)) `divd` size sz) `igteq` (len))
assertSize _ _ = []

seqLoop :: Origin -> [Primary] -> [Primary] -> Arg -> SizeNative -> (Doc -> Doc -> Doc -> [Code]) -> [Code]
seqLoop _ [] [] arg nsz expr =
   let   nt = text "_seq_nat" <> int (aN arg)
   in    [ dr $ text "char*" <+> nt <+> text " = 0" <> semi
         , ds $ "int _ii = 0;"
         , E $ for [text "_ii = 0"
                   ,nt `eq` text "(char*)" <> (seqptrval arg)]
                   [text "_ii < (int)" <> (seqlenval arg)]
                   [text "++_ii"
                   ,nt `eq` (nt `plus` sizeNative nsz)]
                   (expr (int 0) (int 0) nt) ]


seqLoop og [(Prim In bufin isz)] [] arg nsz expr =
   let   sp md = text "_seq_prim" <> (text $ show md) <> int (aN arg)
         nt = text "_seq_nat" <> int (aN arg)
   in    [ dr $ text "char*" <+> sp In <+> text " = 0" <> semi
         , dr $ text "char*" <+> nt <+> text " = 0" <> semi
         , ds $ "int _ii = 0;"
         , E $ for [text "_ii = 0"
                   ,sp In `eq` text "(char*)" <> (pv (Buf In bufin))
                   ,nt `eq` text "(char*)" <> (seqptrval arg)]
                   [text "_ii < (int)" <> (seqlenval arg)]
                   [text "++_ii"
                   ,sp In `eq` (sp In  `plus` size isz)
                   ,nt `eq`(nt `plus` sizeNative nsz)]
                   (expr (sp In) (int 0) nt)]

seqLoop og [] [(Prim ROut bufrout osz)] arg nsz expr =
   let   sp md = text "_seq_prim" <> (text $ show md) <> int (aN arg)
         nt = text "_seq_nat" <> int (aN arg)
   in    [ dr $ text "char*" <+> sp ROut <+> text " = 0" <> semi
         , dr $ text "char*" <+> nt <+> text " = 0" <> semi
         , ds $ "int _ii = 0;"
         , E $ for [text "_ii = 0"
                   ,sp ROut `eq` text "(char*)" <> (pvPost (Buf ROut bufrout))
                   ,nt `eq` text "(char*)" <> (seqptrval arg)]
                   [text "_ii < (int)" <> (seqlenval arg)]
                   [text "++_ii"
                   ,sp ROut `eq` (sp ROut  `plus` size osz)
                   ,nt `eq` (nt `plus` sizeNative nsz)]
                   (expr (int 0) (sp ROut) nt)]

seqLoop og [(Prim In bufin isz)] [(Prim ROut bufrout osz)] arg nsz expr =
   let   sp md = text "_seq_prim" <> (text $ show md) <> int (aN arg)
         nt = text "_seq_nat" <> int (aN arg)
   in    [ dr $ text "char*" <+> sp In <+> text " = 0" <> semi
         , dr $ text "char*" <+> sp ROut <+> text " = 0" <> semi
         , dr $ text "char*" <+> nt <+> text " = 0" <> semi
         , ds $ "int _ii = 0;"
         , E $ for [text "_ii = 0"
                   ,sp In `eq` text "(char*)" <> (pv (Buf In bufin))
                   ,sp ROut `eq` text "(char*)" <> (pv (Buf ROut bufrout))
                   ,nt `eq` text "(char*)" <> (seqptrval arg)]
                   [text "_ii < (int)" <> (seqlenval arg)]
                   [text "++_ii"
                   ,sp In `eq` (sp In  `plus` size isz)
                   ,sp ROut `eq` (sp ROut  `plus` size osz)
                   ,nt `eq` (nt `plus` sizeNative nsz)]
                   (expr (sp In) (sp ROut) nt)]


seqLoop _ _ _ _ _ _ = error "internal error: seqLoop bad primary"

for :: [Doc] -> [Doc] -> [Doc] -> [Code] -> Expr
for inits compares incs block =
   let
         hsemis [] = empty
         hsemis ls = (hcat $ intersperse (text ";") ls)
   in    BE (render $ text "for" <> parens (hsemis [commals inits, commals compares, commals incs])) block

skelUnpackF :: Origin -> [SerealArg] -> IsExtParams -> Function
skelUnpackF og sereal iep
     | (not $ hasDmahandle sereal) = Function
         ""
         "static __inline int"
         "_skel_unpack"
         (ctxTypes og ++ (map (AS .("_ATTRIBUTE_UNUSED " ++). render) $ declareRefsl sereal))
         (
            [D $ SD $ "int _nErr = 0;"]
      ++    [ er $ text "remote_arg* _praInStart = _praIn;" ]
      ++    [ er $ text "remote_arg** _ppraInStart = _ppraIn;" ]
      ++    [ er $ text "remote_arg* _praROutStart = _praROut;" ]
      ++    [ er $ text "remote_arg** _ppraROutStart = _ppraROut;" ]
      ++    [ er $ text "_ppraIn = &_praIn;" ]
      ++    [ er $ text "_ppraROut = &_praROut;" ]
      ++    concat (zipWith (skelUnpack og) sereal (repeat iep))
      ++    [ er $ text "_ppraInStart[0] += (_praIn - _praInStart) + " <> int (numSecondaryBufs In sereal) <> semi ]
      ++    [ er $ text "_ppraROutStart[0] += (_praROut - _praROutStart) +" <> int (numSecondaryBufs ROut sereal) <> semi ]
      ++    [ F $ "return _nErr;" ] )
skelUnpackF og sereal _ = Function
         ""
         "static __inline int"
         "_skel_unpack"
         (ctxTypes og ++  (map (AS .("_ATTRIBUTE_UNUSED "++). render) $ declareRefsl sereal ))
         (
            [D $ SD $ "int _nErr = 0;"]
      ++    (concatMap (skelUnpackCopy og) sereal)
      ++    [ F $ "return _nErr;" ] )

skelPack :: Origin -> SerealArg -> [Code]
skelPack _ (ScalarArg arg tt pp@(Prim ROut _ _) _) = pack pp arg tt
skelPack og (ComplexArg arg@(Arg ROut _ _) (ComplexStruct _) prims args) =
         [ D $ CD $ ppraROutPost og ]
    ++   (try $ skelPackF og args `CE` (ctxStructPost prims arg ++ (callOffsets (ptr arg) args)))
skelPack og (ComplexArg arg (ComplexSeq sz _ _) prims args) =
   let   pr md = filter ((==) md  . primMode) prims
   in    [D $ CD $ ppraROutPost og]
   ++    (seqLoop og [] (pr ROut) arg sz $ \ _ prout nat ->
            (try $ skelPackF og args `CE` ((ctxPost prout arg) ++ (callOffsets nat args))))
skelPack og oo@(ObjArg arg obj@(Obj ROut _)) =
         [ er $ text "_COPY" `call` [ph obj, int 0, ptr arg, int 0, text "sizeof(remote_handle64)"] <> semi ]

skelPack _ _ = []

skelPackF :: Origin -> [SerealArg] -> Function
skelPackF og sereal = Function
         ""
         "static __inline int"
         "_skel_pack"
         (ctxTypesPost og ++  (map (AS .("_ATTRIBUTE_UNUSED "++).render) $ declareRefsl sereal ))
         (
            [D $ SD $ "int _nErr = 0;"]
      ++    [ er $ text "remote_arg* _praROutPostStart = _praROutPost;" ]
      ++    [ er $ text "remote_arg** _ppraROutPostStart = _ppraROutPost;" ]
      ++    [ er $ text "_ppraROutPost = &_praROutPost;" ]
      ++    (concatMap (skelPack og) sereal)
      ++    [ er $ text "_ppraROutPostStart[0] += (_praROutPost - _praROutPostStart) +" <> int (numSecondaryBufs ROut sereal) <> semi ]
      ++    [ F $ "return _nErr;" ] )

stubPack :: Origin -> SerealArg -> [Code]
stubPack _ (ScalarArg arg tt pp@(Prim In _ _) _) = pack pp arg tt
stubPack _ (ScalarArg _ _ (Prim ROut _ _) _) = []
stubPack og (BufferArg arg (SequenceBuf sz _ _ _) buf) =
         [ dpv og buf
         , er $ pv buf `eq` seqptrvall arg <> semi
         , er $ nLen buf `eq` (size sz `mult` (text "(size_t)" <> parens (seqlenval arg))) <> semi ]
stubPack og (ComplexArg arg (ComplexStruct _) prims args) =
         [ D $ CD (allocator "" "")
         , D $ CD $ ppraIn og
         , D $ CD $ ppraROut og
         , D $ CD $ ppraHIn og
         , D $ CD $ ppraHROut og ]
   ++    (try $ (stubPackF og args) `CE` (ctxStruct prims arg ++ (callOffsets (ptr arg) args)))
stubPack og (ComplexArg arg (ComplexSeq sz _ _) prims args) =
   let   pr md = filter ((==) md  . primMode) prims
   in    [ D $ CD $ ppraIn og
         , D $ CD $ ppraROut og
         , D $ CD $ ppraHIn og
         , D $ CD $ ppraHROut og ]
   ++    (concatMap (allocSeq arg) prims)
   ++    (seqLoop og (pr In) (pr ROut) arg sz $ \ prin prout nat ->
           (try $ stubPackF og args `CE` (ctxPrim prin prout arg ++ (callOffsets nat args))))
stubPack og oo@(ObjArg arg obj@(Obj In ix)) =
         [ dph og obj
         , er $ text "_COPY" `call` [ph obj, int 0, ptr arg, int 0, text "sizeof(remote_handle64)"] <> semi ]
stubPack og (ObjArg _ (Obj ROut _)) = []
stubPack og oo@(DmahandleArg arg _ obj@(Obj _ _)) =
         [ dmah og obj
         , er $ text "_COPY" `call` [phfd og obj, int 0, ptr arg <> text "Fd", int 0, text "sizeof(uint32_t)"] <> semi
         , er $ text "_COPY" `call` [phoffset og obj, int 0, ptr arg <> text "Offset", int 0, text "sizeof(uint32_t)"] <> semi ]
     ++  assert (text "__QAIC_REMOTE(remote_register_dma_handle)" `call` [ text "*" <> ptr arg <> text "Fd", text "*" <> ptr arg <> text "Len"] `eqq` (int 0))
     ++  [ inch og obj ]

allocSeq :: Arg -> Primary -> [Code]
allocSeq arg (Prim md bufin isz) =
   let   buf = (Buf md bufin)
   in    (alloc ((text "(size_t)" <> parens(seqlenval arg)) `mult` size isz) (alig isz)  (pv buf))
   ++    [er $ nLen buf `eq` (size isz `mult`  (text "(size_t)" <> parens (seqlenval arg))) <> semi ]

stubPackF :: Origin -> [SerealArg] -> Function
stubPackF og sereal
   | (not $ hasDmahandle sereal) = Function
         ""
         "static __inline int"
         "_stub_pack"
         (ctxTypes og ++ (map (AS .("_ATTRIBUTE_UNUSED "++). render) $ declareRefsl sereal ))
         (
            [D $ SD $ "int _nErr = 0;"]
      ++    [ er $ text "remote_arg* _praInStart = _praIn;" ]
      ++    [ er $ text "remote_arg** _ppraInStart = _ppraIn;" ]
      ++    [ er $ text "remote_arg* _praROutStart = _praROut;" ]
      ++    [ er $ text "remote_arg** _ppraROutStart = _ppraROut;" ]
      ++    [ er $ text "_ppraIn = &_praIn;" ]
      ++    [ er $ text "_ppraROut = &_praROut;" ]
      ++    (concatMap (stubPack og) sereal)
      ++    [ er $ text "_ppraInStart[0] += (_praIn - _praInStart) + " <> int (numSecondaryBufs In sereal) <> semi ]
      ++    [ er $ text "_ppraROutStart[0] += (_praROut - _praROutStart) +" <> int (numSecondaryBufs ROut sereal) <> semi ]
      ++    [ F $ "return _nErr;" ] )
stubPackF og sereal = Function
         ""
         "static __inline int"
         "_stub_pack"
         (ctxTypes og ++  (map (AS .("_ATTRIBUTE_UNUSED "++). render) $ declareRefsl sereal ))
         (
            [D $ SD $ "int _nErr = 0;"]
      ++    (concatMap (stubPack og) sereal)
      ++    [ F $ "return _nErr;" ] )

type Origin = [SerealArg]

stubUnpack :: Origin -> SerealArg -> [Code]
stubUnpack _ (ScalarArg arg tt pp@(Prim ROut _ _) _) = unpack pp arg tt
stubUnpack _ (BufferArg arg (SequenceBuf _ True _ _) (Buf ROut _)) =
      iff ((seqlenval arg) `gt` int 0) ((seqptrval arg) <> (brackets ((seqlenval arg)  `minus` (int 1))) `eq` int 0 <> semi)
stubUnpack og (ComplexArg arg@(Arg ROut _ _) (ComplexStruct _) prims args) =
            [ D $ CD (allocator "" "")
            , D $ CD $ ppraROutPost og]
      ++    (try $ stubUnpackF og args `CE` (ctxStructPost prims arg ++ (callOffsets (ptr arg) args)))
stubUnpack og (ComplexArg arg (ComplexSeq sz _ _) prims args) =
   let   pr md = filter ((==) md  . primMode) prims
   in    [ D $ CD (allocator "" "")
         , D $ CD $ ppraROutPost og]
   ++    (seqLoop og []  (pr ROut) arg sz $ \ _ prout nat ->
            (try $ stubUnpackF og args `CE` ((ctxPost prout arg) ++ (callOffsets nat args))))

stubUnpack og oo@(ObjArg arg obj@(Obj ROut _)) =
         [ dph og obj
         , er $ text "_COPY" `call` [ptr arg, int 0, ph obj, int 0, text "sizeof(remote_handle64)"] <> semi ]

stubUnpack _ _ = []


stubUnpackF :: Origin -> [SerealArg] -> Function
stubUnpackF og sereal = Function
         ""
         "static __inline int"
         "_stub_unpack"
         (ctxTypesPost og ++  (map (AS .("_ATTRIBUTE_UNUSED "++). render) $ declareRefsl sereal  ))
         (
            [D $ SD $ "int _nErr = 0;"]
      ++    [ er $ text "remote_arg* _praROutPostStart = _praROutPost;" ]
      ++    [ er $ text "remote_arg** _ppraROutPostStart = _ppraROutPost;" ]
      ++    [ er $ text "_ppraROutPost = &_praROutPost;" ]
      ++    (concatMap (stubUnpack og) sereal)
      ++    [ er $ text "_ppraROutPostStart[0] += (_praROutPost - _praROutPostStart) +" <> int (numSecondaryBufs ROut sereal) <> semi ]
      ++    [ F $ "return _nErr;" ] )

copy :: Primary -> Arg -> ScalarType -> [Code]
copy pp arg (Register _ al) =
         [ er $ text "_COPYIF" `call` [ptr arg, int 0, ptr $ iarg arg, int 0, iint al] <> semi ]
copy pp arg (Scalar sz) =
         [ er $ text "_COPYIF" `call` [ptr arg, int 0, ptr $ iarg arg, int 0, size sz] <> semi ]
copy pp arg (Len _) = []

unpack :: Primary -> Arg -> ScalarType -> [Code]
unpack pp arg (Register _ al) =
         [ er $ text "_COPY" `call` [ptr arg, int 0, prim pp, pO pp arg, iint al] <> semi ]
unpack pp arg (Scalar sz) =
         [ er $ text "_COPY" `call` [ptr arg, int 0, prim pp, pO pp arg, size sz] <> semi ]
unpack pp arg (Len _) =
         [ er $ text "_COPY" `call` [seqlenref arg, int 0, prim pp, pO pp arg, int 4] <> semi ]

pack :: Primary -> Arg -> ScalarType -> [Code]
pack pp arg (Register _ al) =
         [ er $ text "_COPY" `call` [prim pp, pO pp arg, ptr arg, int 0, iint al] <> semi ]
pack pp arg (Scalar sz) =
         [ er $ text "_COPY" `call` [prim pp, pO pp arg, ptr arg, int 0, size sz] <> semi ]
pack pp arg@(Arg In _ _) (Len (Just al)) =
         [ er $ (seqlenval arg) `eq` lparen <> (typeFromAlS False 4) <> rparen <> (int 1 `plus` ((std_strlen al) `call` [seqptrval arg])) <> semi
         , er $ text "_COPY" `call` [prim pp, pO pp arg, seqlenref arg, int 0, int 4 ] <> semi ]
pack pp arg (Len _) =
         [ er $ text "_COPY" `call` [prim pp, pO pp arg, seqlenref arg, int 0, int 4 ] <> semi ]

pO :: Primary -> Arg -> Doc
pO (Prim In _ _) arg = int $ argPrimIn arg
pO (Prim ROut _ _) arg = int $ argPrimROut arg

argPrimIn :: Arg -> Int
argPrimIn arg = oIn $ xO $ aI arg

argPrimROut :: Arg -> Int
argPrimROut arg =  oROut $ xO $ aI arg

ptr :: Arg -> Doc
ptr (Arg In ix _)   = parm "_in"   (xN ix)
ptr (Arg ROut ix _) = parm "_rout" (xN ix)

iarg :: Arg -> Arg
iarg (Arg ROut ix True)   = Arg In ix True
iarg (Arg In ix True) = Arg ROut ix True
iarg _ = error "internal error: arg is not inrout"

seqptrval :: Arg -> Doc
seqptrval (Arg In ix _)   = val $ parm "_in" (xN ix)
seqptrval (Arg ROut ix _) = val $ parm "_rout" (xN ix)

seqptrvall :: Arg -> Doc
seqptrvall (Arg In ix _)   = val $ parm "(void*) _in" (xN ix)
seqptrvall (Arg ROut ix _) = val $ parm "_rout" (xN ix)

seqlenval :: Arg -> Doc
seqlenval (Arg In ix _)   = val $ parm "_in" (xN ix) <> text "Len"
seqlenval (Arg ROut ix _) = val $ parm "_rout" (xN ix) <> text "Len"

seqlenref :: Arg -> Doc
seqlenref (Arg In ix _)   = parm "_in" (xN ix) <> text "Len"
seqlenref (Arg ROut ix _) = parm "_rout" (xN ix) <> text "Len"

pv :: Buf -> Doc
pv (Buf In ix) = text "_praIn[" <> int ix <> text "].buf.pv"
pv (Buf ROut ix) = text "_praROut[" <> int ix <> text "].buf.pv"

pvPost :: Buf -> Doc
pvPost (Buf ROut ix) = text "_praROutPost[" <> int ix <> text "].buf.pv"
pvPost _ = error "internal error: pv post should only be rout"

dpv :: Origin -> Buf -> Code
dpv og (Buf In _) = D $ CD $ praIn og
dpv og (Buf ROut _) = D $ CD $ praROut og

dph :: Origin -> Obj -> Code
dph og (Obj In _) = D $ CD $ praRHandleIn og
dph og (Obj ROut _) = D $ CD $ praRHandleROut og

inch :: Origin -> Obj -> Code
inch og (Obj In ix)
   | (hasComplex og) = er $ text "_ppraHIn[0] = _ppraHIn[0] + 1 + " <> int ix `minus` int ix <> semi
   | otherwise = er $ text ""
inch og (Obj ROut ix)
   | (hasComplexROut og) = er $ text "_ppraHROut[0] = _ppraHROut[0] + 1 + " <> int ix `minus` int ix <> semi
   | otherwise = er $ text ""

dmah :: Origin -> Obj -> Code
dmah og (Obj In _) = D $ CD $ praHandleIn og
dmah og (Obj ROut _) = D $ CD $ praHandleROut og

sdmah :: Origin -> Obj -> IsExtParams -> Code
sdmah og (Obj In _) iep = D $ CD $ spraHandleIn og iep
sdmah og (Obj ROut _) iep = D $ CD $ spraHandleROut og iep

ph :: Obj -> Doc
ph (Obj In ix) = text "&(_praRHandleIn[" <> int ix <> text "].h64)"
ph (Obj ROut ix) = text "&(_praRHandleROut[" <> int ix <> text "].h64)"

phfd :: Origin -> Obj -> Doc
phfd og (Obj In ix)
   | (hasComplex og) = text "&(_praHIn[0].dma.fd)"
   | otherwise = text "&(_praHandleIn[" <> int ix <> text "].dma.fd)"
phfd og (Obj ROut ix)
   | (hasComplex og) = text "&(_praHROut[0].dma.fd)"
   | otherwise = text "&(_praHandleROut[" <> int ix <> text "].dma.fd)"

phoffset :: Origin -> Obj -> Doc
phoffset og (Obj In ix)
   | (hasComplex og) = text "&(_praHIn[0].dma.offset)"
   | otherwise = text "&(_praHandleIn[" <> int ix <> text "].dma.offset)"
phoffset og (Obj ROut ix)
   | (hasComplex og) = text "&(_praHROut[0].dma.offset)"
   | otherwise = text "&(_praHandleROut[" <> int ix <> text "].dma.offset)"

prim :: Primary -> Doc
prim (Prim In _ _) = text "_primIn"
prim (Prim ROut _ _) = text "_primROut"

nLen :: Buf -> Doc
nLen (Buf In ix) = text "_praIn[" <> int ix <> text "].buf.nLen"
nLen (Buf ROut ix) = text "_praROut[" <> int ix <> text "].buf.nLen"

nErr :: Doc
nErr = text "_nErr"

iint :: Integral a => a -> Doc
iint = int . fromIntegral

iff :: Doc -> Doc -> [Code]
iff cond body = [ er $ text "if" `call` [cond]
                , er $ text  "{"
                , er $ nest 3 body
                , er $ text  "}"
                ]


addFarf :: FarfLevel -> FarfMessage -> FarfArg -> Int -> String
addFarf level msg arg indentation =
   let qaicFarf = if (length arg /= 0)
                  then (text ("_QAIC_FARF("++level++", \""++msg++"\", "++arg++");"))
                  else (text ("_QAIC_FARF("++level++", \""++msg++"\");"))
   in render (nest indentation qaicFarf)

tryF :: String -> Function
tryF tryFunc = Function "" "" tryFunc [] []

try :: Expr -> [Code]
try expr =
   [ D $ SD $ "int _nErr = 0;"
   , E $ (tryF "_TRY") `CE` [SE "_nErr", expr]
   , F $ "_QAIC_CATCH(_nErr) {}"]

tryFarf :: Expr -> String -> String -> [Code]
tryFarf expr message parameters=
   [ D $ SD $ "int _nErr = 0;"
   , E $ (tryF "_TRY_FARF") `CE` [SE "_nErr", expr]
   , F $ "_CATCH_FARF(_nErr) {"
   , F $ (addFarf "RUNTIME_ERROR" message parameters 3)
   , F $ "}"]

alloc :: Doc -> Doc -> Doc -> [Code]
alloc len al buf =
   [ D $ CD (allocator "" "")
   , er $ text "_QAIC_ALLOCATE" `call` [nErr, text "_al", len, al, buf] <> semi
   ]


allocpra :: Doc -> Doc -> Doc -> String -> String -> [Code]
allocpra len al buf message parameters =
   [ D $ CD (allocator message parameters)
   , er $ text "_QAIC_ALLOCATE" `call` [nErr, text "_al", len, al, buf] <> semi
   , er $ text "_QAIC_ASSERT" `call` [nErr, text "_pra"] <> semi
   ]

allocator :: String -> String -> Contextual
allocator message parameters =
   let catch_farf = if (message /= "" && parameters /= "" )
                    then [F $ "_CATCH_FARF(_nErr) {"
                        , F $ (addFarf "RUNTIME_ERROR" message parameters 3)
                        , F $ "}"]
                    else []
   in (Contextual
   ([ D $ SD $ "_allocator _al[1] = {{0}};"
   , es $ "_allocator_init(_al, 0, 0);"
   , F $ "_QAIC_CATCH(_nErr) {}"]
   ++ catch_farf
   ++ [ F $ "_allocator_deinit(_al);"]))

praIn :: Origin -> Contextual
praIn og = Contextual $
      [ D $ SD $ "remote_arg* _praIn = 0;"
      , er $ text "_praIn" `eq` (text "_pra" `plus` (int $ numPrimBufs In og)) <> semi]

praHIn :: Origin -> Contextual
praHIn og
   | (hasComplexROut og) = Contextual $
      [ D $ SD $ "remote_arg* _praHIn = 0;" ]
      ++ iff (text "_praHIn == 0") (text "_praHIn" `eq` (text "_praROut" `plus` (text "_numROut[0]") `plus` (int $ numPrimBufs ROut og)) <> semi)
praHIn og
   | (hasComplex og) = Contextual $
      [ D $ SD $ "remote_arg* _praHIn = 0;"
      , D $ CD $ ppraIn og ]
      ++ iff (text "_praHIn == 0") (text "_praHIn" `eq` (text "_praIn" `plus` (text "_numIn[0]") `plus` (int $ numPrimBufs ROut og)) <> semi)
praHIn og = Contextual $
      [ D $ SD $ "remote_arg* _praHIn = 0;" ]
      ++ iff (text "_praHIn == 0") (text "_praHIn" `eq` (text "_praROut" `plus` (int $ numPrimBufs ROut og)) <> semi)

spraHIn :: Origin -> IsExtParams -> Contextual
spraHIn og iep
   | (hasComplex og) = Contextual $
         [ D $ SD $ "remote_arg* _praHIn = 0;"
         , dr $ if (iep==True) then text "_praHIn = _pra + REMOTE_SCALARS_INBUFS_64(_sc_64) + REMOTE_SCALARS_OUTBUFS_64(_sc_64);"
                else text "_praHIn = _pra + REMOTE_SCALARS_INBUFS(_sc) + REMOTE_SCALARS_OUTBUFS(_sc);" ]
   | otherwise = Contextual $ []

praEnd :: IsExtParams -> Contextual
praEnd iep = Contextual
   [ D $ SD $ "remote_arg* _praEnd = 0;"
   , er $ if (iep==True)
          then text "_praEnd" `eq` (text "_pra" `plus` text "REMOTE_SCALARS_INBUFS_64(_sc_64)" `plus` text "REMOTE_SCALARS_OUTBUFS_64(_sc_64) + REMOTE_SCALARS_INHANDLES_64(_sc_64) + REMOTE_SCALARS_OUTHANDLES_64(_sc_64)") <> semi
          else text "_praEnd" `eq` (text "_pra" `plus` text "REMOTE_SCALARS_INBUFS(_sc)" `plus` text "REMOTE_SCALARS_OUTBUFS(_sc) + REMOTE_SCALARS_INHANDLES(_sc) + REMOTE_SCALARS_OUTHANDLES(_sc)") <> semi
   ]

praRHandleIn :: Origin -> Contextual
praRHandleIn og = Contextual $
      [ D $ SD $ "remote_arg* _praRHandleIn = 0;"
      , er $ text "_praRHandleIn" `eq` (text "_pra" `plus` (numBufs og)) <> semi]

praRHandleROut :: Origin -> Contextual
praRHandleROut _ = Contextual $ []

praHandleIn :: Origin -> Contextual
praHandleIn og
   | (not $ hasComplex og) = Contextual $
      [ D $ SD $ "remote_arg* _praHandleIn = 0;"
      , er $ text "_praHandleIn" `eq` (text "_pra" `plus` (numBufs og)) <> semi]
praHandleIn og = Contextual $
      [ D $ CD $ ppraHIn og ]

spraHandleIn :: Origin -> IsExtParams -> Contextual
spraHandleIn og iep
   | (not $ hasComplex og) = Contextual $
      [ D $ SD $ "remote_arg* _praHandleIn = 0;"
      , er $ if (iep==True)
             then text "_praHandleIn" `eq` (text "_pra + REMOTE_SCALARS_INBUFS_64(_sc_64) + REMOTE_SCALARS_OUTBUFS_64(_sc_64)") <> semi
             else text "_praHandleIn" `eq` (text "_pra + REMOTE_SCALARS_INBUFS(_sc) + REMOTE_SCALARS_OUTBUFS(_sc)") <> semi]
spraHandleIn og iep = Contextual $
      [ D $ CD $ sppraHIn og iep]

praHandleROut :: Origin -> Contextual
praHandleROut og
   | (not $ hasComplex og) = Contextual $
      [ D $ SD $ "remote_arg* _praHandleROut = 0;"
      , er $ text "_praHandleROut" `eq` (text "_pra" `plus` (numBufs og) `plus` (numInHandles og)) <> semi ]
praHandleROut og = Contextual $
      [ D $ CD $ ppraHROut og ]

spraHandleROut :: Origin -> IsExtParams -> Contextual
spraHandleROut og iep
   | (not $ hasComplex og) = Contextual $
      [ D $ SD $ "remote_arg* _praHandleROut = 0;"
      , er $ if (iep==True)
             then text "_praHandleROut" `eq` (text "_pra + REMOTE_SCALARS_INBUFS_64(_sc_64) + REMOTE_SCALARS_OUTBUFS_64(_sc_64) + REMOTE_SCALARS_INHANDLES_64(_sc_64);")
             else text "_praHandleROut" `eq` (text "_pra + REMOTE_SCALARS_INBUFS(_sc) + REMOTE_SCALARS_OUTBUFS(_sc) + REMOTE_SCALARS_INHANDLES(_sc);") ]
spraHandleROut og iep = Contextual $
      [ D $ CD $ sppraHROut og iep]

numBufs :: Origin -> Doc
numBufs og = (int $ numAllStaticBufs In og) `plus` (int $ numAllStaticBufs ROut og)

numAllHandles :: Origin -> Doc
numAllHandles og = (int $ numRemoteHandles In og) `plus` (int $ numRemoteHandles ROut og) `plus` (int $ numHandles In og) `plus` (int $ numHandles ROut og)

numInHandles :: Origin -> Doc
numInHandles og = (int $ numHandles In og)

numROutHandles :: Origin -> Doc
numROutHandles og = (int $ numHandles ROut og)

ppraIn :: Origin -> Contextual
ppraIn og = Contextual
   [ D $ CD $ praIn og
   , D $ SD $ "remote_arg** _ppraIn = &_praIn;"
   ]

ppraHIn :: Origin -> Contextual
ppraHIn og = Contextual
   [ D $ CD $ praHIn og
   , D $ SD $ "remote_arg** _ppraHIn = &_praHIn;"
   ]

sppraHIn :: Origin -> IsExtParams -> Contextual
sppraHIn og iep = Contextual
   [ D $ CD $ spraHIn og iep
   , D $ SD $ "remote_arg** _ppraHIn = &_praHIn;"
   ]

sppraHROut :: Origin -> IsExtParams -> Contextual
sppraHROut og iep = Contextual
   [ D $ CD $ spraHROut og iep
   , D $ SD $ "remote_arg** _ppraHROut = &_praHROut;"
   ]

praROut :: Origin -> Contextual
praROut og = Contextual $
      [ D $ CD $ praIn og
      , D $ SD $ "remote_arg* _praROut = 0;"
      , er $ text "_praROut" `eq` (text "_praIn + _numIn[0]" `plus` (int $ numPrimBufs ROut og)) <> semi]

spraHROut :: Origin -> IsExtParams -> Contextual
spraHROut og iep
   | (hasComplex og)  = Contextual $
      [ D $ CD $ spraHIn og iep
      , D $ SD $ "remote_arg* _praHROut = 0;"
      , dr $ if (iep==True)
             then text "_praHROut = _praHIn + REMOTE_SCALARS_INHANDLES_64(_sc_64);"
             else text "_praHROut = _praHIn + REMOTE_SCALARS_INHANDLES(_sc);"]
spraHROut _ _ = Contextual $ []

praHROut :: Origin -> Contextual
praHROut og
   | (not $ hasComplexROut og) = Contextual $
      [ D $ CD $ praHIn og
      , D $ SD $ "remote_arg* _praHROut = 0;" ]
      ++ iff (text "_praHROut == 0") (text "_praHROut = _praHIn + _numInH[0]" `plus` (int $ numHandles In og) <> semi)
praHROut og = Contextual $
      [ D $ CD $ praHIn og
      , D $ SD $ "remote_arg* _praHROut = 0;" ]
      ++ iff (text "_praHROut == 0") (text "_praHROut = _praHIn + _numInH[0]" `plus` (int $ numHandles In og) <> semi)

ppraROut :: Origin -> Contextual
ppraROut og = Contextual
   [ D $ CD $ praROut og
   , D $ SD $ "remote_arg** _ppraROut = &_praROut;"
   ]

ppraHROut :: Origin -> Contextual
ppraHROut og = Contextual
   [ D $ CD $ praHROut og
   , D $ SD $ "remote_arg** _ppraHROut = &_praHROut;"
   ]

praROutPost :: Origin -> Contextual
praROutPost og = Contextual
   [ D $ CD $ praROut og
   , D $ SD $ "remote_arg* _praROutPost = 0;"
   , es $ "_praROutPost = _praROut;"
   ]

ppraROutPost :: Origin -> Contextual
ppraROutPost og = Contextual
   [ D $ CD $ praROutPost og
   , D $ SD $ "remote_arg** _ppraROutPost = &_praROutPost;"
   ]

sizeNative :: SizeNative -> Doc
sizeNative (SizeNative s32 s64) = ifPtr32 (size s32) (size s64)

aligNative :: SizeNative -> Doc
aligNative (SizeNative s32 s64) = ifPtr32 (alig s32) (alig s64)

size :: Size -> Doc
size (sz,_) = int (fromIntegral sz)

alig :: Size -> Doc
alig (_,al) = int (fromIntegral al)

assert :: Doc -> [Code]
assert cond = [ er $ text "_QAIC_ASSERT" `call` [nErr, cond] <> semi
              , F $ "_QAIC_CATCH(_nErr) {}" ]

std_strlen :: (Eq a, Num a) => a -> Doc
std_strlen 1 = text "strlen"
std_strlen 2 = text "_std_wstrlen"
std_strlen _ = error "internal error: invalid string alignment"

ctxStruct :: [Primary] -> Arg -> [Expr]
ctxStruct [_] arg = ctxPrim (text "(char*)_primIn" `plus` (int $ argPrimIn arg)) (int 0) arg
ctxStruct _ arg = ctxPrim (text "(char*)_primIn" `plus` (int $ argPrimIn arg))
                          (text "(char*)_primROut" `plus` (int $ argPrimROut arg))
                          arg
ctxStructPost :: [Primary] -> Arg -> [Expr]
ctxStructPost [_] arg = ctxPost (int 0) arg
ctxStructPost _ arg   = ctxPost (text "(char*)_primROut" `plus` (int $ argPrimROut arg)) arg


ctxPrim :: Doc -> Doc -> Arg -> [Expr]
ctxPrim primin primrout arg =
   [ SE $ "_al"
   , SE $ render $ text "_praIn" `plus` (int $ argInBuf arg)
   , SE $ render $ text "_ppraIn"
   , SE $ render $ text "_praROut" `plus` (int $ argROutBuf arg)
   , SE $ render $ text "_ppraROut"
   , SE $ render $ text "_praHIn"
   , SE $ render $ text "_ppraHIn"
   , SE $ render $ text "_praHROut"
   , SE $ render $ text "_ppraHROut"
   , SE $ render $ primin
   , SE $ render $ primrout
   ]

ctxPost :: Doc -> Arg -> [Expr]
ctxPost primrout arg =
   [ SE $ render $ text "_praROutPost" `plus` (int $ argROutBuf arg)
   , SE $ render $ text "_ppraROutPost"
   , SE $ render $ primrout
   ]

ctxTypes :: Origin -> [FuncArgType]
ctxTypes og = [ AV "_ATTRIBUTE_UNUSED _allocator* _al" (allocator "" "")
              , AV "_ATTRIBUTE_UNUSED remote_arg* _praIn" $ praIn og
              , AV "_ATTRIBUTE_UNUSED remote_arg* _ppraIn[1]" $ ppraIn og
              , AV "_ATTRIBUTE_UNUSED remote_arg* _praROut" $ praROut og
              , AV "_ATTRIBUTE_UNUSED remote_arg* _ppraROut[1]" $ ppraROut og
              , AV "_ATTRIBUTE_UNUSED remote_arg* _praHIn" $ praHIn og
              , AV "_ATTRIBUTE_UNUSED remote_arg* _ppraHIn[1]" $ ppraHIn og
              , AV "_ATTRIBUTE_UNUSED remote_arg* _praHROut" $ praHROut og
              , AV "_ATTRIBUTE_UNUSED remote_arg* _ppraHROut[1]" $ ppraHROut og
              , AS $ "_ATTRIBUTE_UNUSED void* _primIn"
              , AS $ "_ATTRIBUTE_UNUSED void* _primROut"]

ctxTypesPost :: Origin -> [FuncArgType]
ctxTypesPost og = [ AV "_ATTRIBUTE_UNUSED remote_arg* _praROutPost" $ praROutPost og
                  , AV "_ATTRIBUTE_UNUSED remote_arg* _ppraROutPost[1]" $ ppraROutPost og
                  , AS $ "_ATTRIBUTE_UNUSED void* _primROut"]


callOffsets :: Doc -> [SerealArg] -> [Expr]
callOffsets arg args = map (SE . render . offsetptr arg) $ sortBy saOrder args

offsetptr :: Doc -> SerealArg -> Doc
offsetptr arg sa =
   let
         (SizeNative s32 s64) = szNative $ xO $ aI $ saArg sa
         (o32,o64) = saNativeO sa
   in    ifPtr32 (offsetptr' arg sa o32 s32) (offsetptr' arg sa o64 s64)

offsetptr' :: Doc -> SerealArg -> Int -> Size -> Doc
offsetptr' arg sa offset sznat =
   let
         typ = typeFromAlS False (snd sznat)
         pos =  int $ offset `div` (fromIntegral $ snd sznat)
   in    offsetToRefl sa $ (parens (parens (parens (typ <> text "*") <> arg) <> brackets pos))


pvPrimIn :: [Char]
pvPrimIn = "_pra[0].buf.pv"

nLenPrimIn :: [Char]
nLenPrimIn = "_pra[0].buf.nLen"

pvPrimROut :: Origin -> [Char]
pvPrimROut og = render $ praROutBuf og <> text ".pv"

nLenPrimROut :: Origin -> [Char]
nLenPrimROut og = render $ praROutBuf og <> text ".nLen"

praROutBuf :: Origin -> Doc
praROutBuf og = text "_pra" <> brackets (text "_numIn[0]" `plus` (int (numPrimBufs In og))) <> text ".buf"


-- Removing as part of QAIC Source Code Cleanup

-- assertPrim :: Mode -> [SerealArg] -> [Code]
-- assertPrim md sereal
--    | (numPrimBufs md sereal) > 0 = assert $ (text "_prim" <> (text (show md))) `lt` text "_praEnd"
-- assertPrim _ _ = []

-- assertSecondary :: Mode -> [SerealArg] -> [Code]
-- assertSecondary md sereal
--    | (numSecondaryBufs md sereal) > 0 = assert $ (text "_pra" <> (text (show md))) `lt` text "_praEnd"
-- assertSecondary _ _ = []

-- skelUnpackAll :: Origin -> [SerealArg] -> [Code]
-- skelUnpackAll og sereal =
--       assertSecondary In sereal
--    ++ assertSecondary ROut sereal
--    ++ (concatMap (skelUnpack og) sereal)

-- preHandle :: Origin -> Obj -> Code
-- preHandle og (Obj In _)
--    | (hasComplex og) =  er $ text "_ppraHIn = &_praHIn;"
--    | otherwise = er $ text ""
-- preHandle og (Obj ROut _)
--    | (hasComplexROut og) = er $ text "_ppraHROut = &_praHROut;"
--    | otherwise = er $ text ""