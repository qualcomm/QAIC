-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

module Language.SO.ToCStub where

import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Language.Idl.Data                    as Idl
import qualified Language.SO.ToHeader                 as H
import qualified Language.SO.CStub                    as CStub
import qualified Language.SO.Allocator                as Allocator
import qualified Language.SO.Wstring                  as Wstring
import qualified Language.SO.Slim                     as Slim
import qualified Language.Idl.ToPalette               as TP
--import Language.Idl.Data                              (ParameterMode(..))
import Language.SO.ToHeader                           ( commals, replace)
import Language.SO.Cfg
import Language.Slim.Doc(toC)
import Language.Slim.Ref
import Language.Slim.Data
import Language.Slim.Serialize
import Text.PrettyPrint
import Language.SO.Args
import Language.SO.Pack
import Language.SO.Code
import Prelude hiding ( (<>) )
type HasRemoteHandle = Bool


methodF :: Slim -> IsExtParams -> HasRemoteHandle -> Bool ->  HasMethodArg -> Method -> String -> Function
methodF sl iep hrh asc hma mm name =
   let
         pars = methodParameters sl mm
         sereal = serializeParameters sl hma pars name
         remote_handle | hrh = "remote_handle64 _handle"
                       | otherwise = "remote_handle _handle"
         asyncarg | asc = "fastrpc_async_descriptor_t* asyncDesc , uint32_t _mid"
                  | otherwise = "uint32_t _mid"
   in    Function
         ""
         "static __inline int"
         "_stub_method"
         (map AS $ remote_handle : asyncarg : (map render $ declareMethodRefsl $ sereal  ))
         (implementParams sl iep hrh asc (scalars hma mm) sereal)

implementParams :: Slim -> IsExtParams -> HasRemoteHandle -> Bool -> Scalars -> [SerealArg] -> [Code]
implementParams _ iep hrh asc sc sereal =
   let   invoke | hrh && asc = "__QAIC_REMOTE(remote_handle64_invoke_async)"
                | hrh && not asc = "__QAIC_REMOTE(remote_handle64_invoke)"
                | not hrh && asc = "__QAIC_REMOTE(remote_handle_invoke_async)"
                | otherwise = "__QAIC_REMOTE(remote_handle_invoke)"
         invoke' | hrh && asc = "__QAIC_REMOTE(remote_handle64_invoke_async_s64)"
                | hrh && not asc = "__QAIC_REMOTE(remote_handle64_invoke_s64)"
                | not hrh && asc = "__QAIC_REMOTE(remote_handle_invoke_async_s64)"
                | otherwise = "__QAIC_REMOTE(remote_handle_invoke_s64)"
         handle | asc = "_handle , asyncDesc"
                | otherwise = "_handle"
         message = "ERROR 0x%x: handle=0x%\""++(if hrh then "PRIx64" else "PRIx32")++"\", scalar=0x%x, method ID=%d: %s failed\\n"
         parameters = "_nErr , _handle, "++render (makeScalars sc sereal iep)++", _mid, __func__"
   in    (map drs $ (map (<> text " = {0}") $ declareMissingRefsl sereal))
   ++    (declarePra sereal iep message parameters)
   ++    ifcrout sereal [D $ CD $ ppraROutPost sereal]
   ++    (concatMap (stubPack sereal) sereal)
   ++    (checkHandlesIn iep sc sereal $(numHandles In sereal))
   ++    (checkHandlesROut iep sc sereal $(numHandles ROut sereal))
   ++    checkNumInNumROutLimit iep invoke invoke' handle sc sereal message parameters
   ++    (concatMap (stubUnpack sereal) sereal)
   ++    [ F $ "return _nErr;" ]

-- message: The failure message to be printed in case function called by tryFarf fails
-- parameters: list of variables to be printed in the error message, to be pased to FARF
checkNumInNumROutLimit :: IsExtParams -> String -> String -> String -> Scalars -> [SerealArg] -> String -> String -> [Code]
checkNumInNumROutLimit iep invoke invoke_s64 handle sc sereal message parameters = let tryExpr = if (iep == True)
                                                                                                 then (ser $ text invoke_s64 `call` [text handle , makeScalars sc sereal iep, text "_pra"])
                                                                                                 else (ser $ text invoke `call` [text handle , makeScalars sc sereal iep, text "_pra"])
                                                                                   in (tryFarf tryExpr message parameters)

checkHandlesIn:: IsExtParams -> Scalars -> [SerealArg] -> Int -> [Code]
checkHandlesIn iep _ args _
   | (hasComplex args) = if (iep == True)
                         then assert ((text "_numInH[0]" `plus` (int $ numHandles In args)) `lteq` text "255")
                         else assert ((text "_numInH[0]" `plus` (int $ numHandles In args)) `lteq` text "15")
checkHandlesIn _ _ _ 0 = []
checkHandlesIn iep _ args _ = if (iep == True)
                              then assert ((int $ numHandles In args) `lteq` text "255")
                              else assert ((int $ numHandles In args) `lteq` text "15")

checkHandlesROut:: IsExtParams -> Scalars -> [SerealArg] -> Int -> [Code]
checkHandlesROut iep _ args _
   | (hasComplex args) =  if (iep == True)
                          then assert ((text "_numROutH[0]" `plus` (int $ numHandles ROut args)) `lteq` text "255")
                          else assert ((text "_numROutH[0]" `plus` (int $ numHandles ROut args)) `lteq` text "15")
checkHandlesROut _ _ _ 0 = []
checkHandlesROut iep _ args _ = if (iep == True)
                                then assert ((int $ numHandles ROut args) `lteq` text "255")
                                else assert ((int $ numHandles ROut args) `lteq` text "15")

makeScalars :: Scalars -> [SerealArg] -> IsExtParams -> Doc
makeScalars sc args False
   | (hasComplex args) =
                text "REMOTE_SCALARS_MAKEX" <> parens (commals [ int 0
                                                               , text "_mid"
                                                               , (text "_numIn[0]" `plus` (int $ numPrimBufs In args))
                                                               , (text "_numROut[0]" `plus` (int $ numPrimBufs ROut args))
                                                               , (text "_numInH[0]" `plus` (int $ numHandles In args))
                                                               , (text "_numROutH[0]" `plus` (int $ numHandles ROut args))
                                                               ])

   | otherwise =
                text "REMOTE_SCALARS_MAKEX" <> parens (commals [ int 0
                                                               , text "_mid"
                                                               , (iint $ bufsIn sc)
                                                               , (iint $ bufsROut sc)
                                                               , (iint $ objsIn sc)
                                                               , (iint $ objsROut sc)
                                                               ])
makeScalars sc args True
   | (hasComplex args) =
                text "REMOTE_SCALARS_MAKEX_64" <> parens (commals [ int 0
                                                               , text "_mid"
                                                               , (text "_numIn[0]" `plus` (int $ numPrimBufs In args))
                                                               , (text "_numROut[0]" `plus` (int $ numPrimBufs ROut args))
                                                               , (text "_numInH[0]" `plus` (int $ numHandles In args))
                                                               , (text "_numROutH[0]" `plus` (int $ numHandles ROut args))
                                                               ])

   | otherwise =
                text "REMOTE_SCALARS_MAKEX_64" <> parens (commals [ int 0
                                                               , text "_mid"
                                                               , (iint $ bufsIn sc)
                                                               , (iint $ bufsROut sc)
                                                               , (iint $ objsIn sc)
                                                               , (iint $ objsROut sc)
                                                               ])

declarePra :: [SerealArg] -> IsExtParams -> String -> String -> [Code]
declarePra args iep message parameters =
      allocatePra args iep message parameters
   ++ declarePrim args (primaryBufs In args)
   ++ declarePrim args (primaryBufs ROut args)

allocatePra :: [SerealArg] -> IsExtParams -> String -> String -> [Code]
allocatePra args _ _ _
   |  (not $ hasComplex args) && (numRAs args == 0) =
         [ dr $ text "remote_arg* _pra = 0" <> semi]
allocatePra args _ _ _
   |  (not $ hasComplex args) && (numAllStaticBufs ROut args == 0) =
         [ dr $ text "remote_arg _pra" <> brackets (int $ numRAs args) <> text " = {0}" <> semi ]
allocatePra args _ _ _
   |  (not $ hasComplex args) =
         [ D $ SD $ "int _numIn[1] = {0};"
         , dr $ text "remote_arg _pra" <> brackets (int $ numRAs args) <> text " = {0}" <> semi
         , er $ text "_numIn[0]" `eq` int (numSecondaryBufs In args)  <> semi]
allocatePra args iep message parameters=
            [ D $ SD $ "remote_arg* _pra = 0;"
            , D $ SD $ "int _numIn[1] = {0};"
            , D $ SD $ "int _numROut[1] = {0};"
            , D $ SD $ "int _numInH[1] = {0};"
            , D $ SD $ "int _numROutH[1] = {0};"
            , er $  (nin `eq` (int $ numSecondaryBufs In args)) <> semi
            , er $  (nrout `eq` (int $ numSecondaryBufs ROut args)) <> semi
            , er $ text "_numInH[0] = 0" <> semi
            , er $ text "_numROutH[0] = 0" <> semi]
      ++    (concatMap countBufs args)
      ++    (bufferLimitError iep)
      ++    allocpra total (int 4) (text "_pra") message parameters
         where
               total = (nin `plus` nrout `plus` ninh `plus` nrouth `plus` (int $ numPrimBufs In args) `plus` (int $ numPrimBufs ROut args) `plus` (int $ numHandles In args) `plus` (int $ numHandles ROut args)) `mult` (text "sizeof(_pra[0])")
               nin = (text "_numIn[0]")
               nrout = (text "_numROut[0]")
               ninh = (text "_numInH[0]")
               nrouth = (text "_numROutH[0]")

bufferLimitError :: IsExtParams -> [Code]
bufferLimitError iep
   | (iep == False) = let inbufStr = (addFarf "RUNTIME_ERROR" "ERROR: Unsupported number of input buffers\\n" "" 4)
                          outbufStr = (addFarf "RUNTIME_ERROR" "ERROR: Unsupported number of output buffers\\n" "" 4)
                      in [E $ BE  "if(_numIn[0]>=255)" [es $ inbufStr, er $ (nest 4 (text "return AEE_EUNSUPPORTED;"))]
                         ,E $ BE  "if(_numROut[0]>=255)" [es $ outbufStr, er $ (nest 4 (text "return AEE_EUNSUPPORTED;"))]]
   | otherwise = []

numRAs :: [SerealArg] -> Int
numRAs args = (numAllStaticBufs In args)  + (numAllStaticBufs ROut args) + (numHandles In args) + (numHandles ROut args)

declarePrim :: Origin -> [Primary] -> [Code]
declarePrim _ [pp@(Prim In _ sz)] =
   [ dr $ typeFromAlS False (snd sz) <+> prim pp <> brackets (int $ arrayElems' sz) <> text "= {0}" <> semi
   , es $ pvPrimIn ++ " = (void*)_primIn;"
   , es $ nLenPrimIn ++ " = sizeof(_primIn);"]

declarePrim og [pp@(Prim ROut _ sz)] =
   [ dr $ typeFromAlS False (snd sz) <+> prim pp <> brackets (int $ arrayElems' sz) <> text "= {0}" <> semi
   , es $ pvPrimROut og ++ " = (void*)_primROut;"
   , es $ nLenPrimROut og ++ " = sizeof(_primROut);"]

declarePrim _ _ = []

toStub ::  Cfg -> [Pos.SourceName] -> Idl.Idl -> [(String, Doc)]
toStub cf files (Idl.Idl decs) =
   let
         decSourceName (Idl.TopLevelDeclaration fp _) = fp
         mine = filter (\ dd -> (decSourceName dd) `elem` files) decs
         haswstring = H.hasWString decs
         comments = vcat $ map H.fromTopComment mine
         addComment (ff,doc) = (ff, comments $+$ doc)
   in    map addComment $ concatMap (fromTop haswstring cf) mine

-- @haswstring: True, if wstring is in IDL's TopLevelDeclaration, False otherwise
-- @cf: Cfg data Type
-- @dec: "Declaration" parameter of IDL's "TopLevelDeclaration"
fromTop :: Bool -> Cfg -> Idl.TopLevelDeclaration -> [(String, Doc)]
fromTop haswstring cf (Idl.TopLevelDeclaration _ dec) = fromDec haswstring cf dec

fromDec :: Bool -> Cfg -> Idl.Declaration -> [(String, Doc)]
fromDec haswstring cf dec@(Idl.Declaration _ _ _ nm df) =
   fromInterface haswstring cf nm (toSlim cf ((H.defScopedName cf nm), dec)) df
fromDec _ _ _                      = []

fromInterface :: Bool -> Cfg -> Idl.Identifier -> Slim -> Idl.Definition -> [(String, Doc)]
fromInterface haswstring cf nm sl iface@(Idl.InterfaceDcl iep _ _ _ _ _) =
   let
         name = (render $ H.scopedName cf nm)
         slimFile = name ++ "_slim.h"
         slimDoc = (toC sl)
         interfaceFile = name ++ "_stub.c"
         rhms = map unMethodDec $ H.getRemoteHandleMethods cf iface
         nrhms = map unMethodDec $ H.getNonRemoteHandleMethods cf iface
         methodDecs = map unMethodDec $ H.getDerivedMethods cf iface

         isrh = not $ null rhms

         isAsyn = map isAsyncMethod $ H.getMethods cf iface
         asyn = True `elem` isAsyn

         async :: Doc
         async
               | asyn = text "#ifndef __ASYNC_STUB"
                       $+$ text "#define __ASYNC_STUB"
                       $+$ text "#endif"
               | otherwise = text ""
         methodRefs::[MethodRef]
         methodRefs = ifaceMethodRefs sl (scope $ H.withScope cf nm)
         nrhmrfs::[MethodRef]
         nrhmrfs = drop 2 methodRefs

         ifaceF :: Int -> MethodRef -> (Idl.Identifier, Idl.Definition) -> [Function]
         ifaceF = interfaceF (H.withScope cf nm) iep isrh sl name

         rhF :: Int -> MethodRef -> (Idl.Identifier, Idl.Definition) -> [Function]
         rhF = remoteHandleF (H.withScope cf nm) sl

         derivedF :: Int -> MethodRef -> (Idl.Identifier, Idl.Definition) -> [Function]
         derivedF ix mr def = derivedRemoteHandleF (H.withScope cf nm) sl ix mr def name iep

         funcs :: [[Function]]
         funcs = case (null rhms) of
            True -> zipWith3  ifaceF [0..] methodRefs methodDecs
            False -> (zipWith3 rhF [0..] methodRefs rhms)
                  ++ (zipWith3 derivedF [2..] nrhmrfs nrhms)
         stringh = text "#include <string.h>"
         docs = (H.toInclude $ moduleName cf)
            $+$ (if (H.hasString methodDecs) then stringh else empty)
            $+$ (if (haswstring) then text Wstring.source else empty) -- In a case where struct having wstring is defined in idl file and is not used as a parameter in idl methods, wstring sources are added to autogen stub file.
            $+$ text "#ifndef _WIN32"
            $+$ text "#include \"HAP_farf.h\"" -- Moving HAP_farf.h inclusion to stub.c file as if anybody includes both autogen header files and HAP_farf.h in their code and enables any of the FARF flags then there would be "macro redefinition error"
            $+$ text "#include <inttypes.h>"
            $+$ text "#endif //_WIN32 for HAP_farf"
            $+$ text Allocator.source
            $+$ text Slim.source
            $+$ (snd $ H.ifdef (slimFile, slimDoc))
            $+$ stub isrh name
            $+$ text "#ifdef __cplusplus"
            $+$ text "extern \"C\" {"
            $+$ text "#endif"
            $+$ compile (reverse $ concat funcs)
            $+$ text "#ifdef __cplusplus"
            $+$ text "}"
            $+$ text "#endif"
   in    [(interfaceFile, docs)]

fromInterface haswstring cf nm _ (Idl.ModuleDcl decs)       = concatMap (fromDec haswstring (H.withScope cf nm)) decs
fromInterface _ _ _ _ _                                   = []

isMethod :: Idl.Declaration -> Bool
isMethod (Idl.Declaration _ _ _ _ (Idl.OperationDcl _ _ _ _ _)) = True
isMethod _ = False

isAsyncMethod :: Idl.Declaration -> Bool
isAsyncMethod (Idl.Declaration _ _ _ _ (Idl.OperationDcl True _ _ _ _)) = True
isAsyncMethod _ = False

unMethodDec :: Idl.Declaration -> (Idl.Identifier,Idl.Definition)
unMethodDec (Idl.Declaration _ _ _ nm df@(Idl.OperationDcl {})) = (nm,df)
unMethodDec (Idl.Declaration sp _ _ _ _) = printError sp "internal: unexpected declaration, expected method"
unMethodDec _ = error "internal error: unexpected declaration, expected method"



interfaceF :: Cfg -> IsExtParams -> HasRemoteHandle -> Slim -> String -> Int -> MethodRef -> (Idl.Identifier, Idl.Definition) -> [Function]
interfaceF cf iep hrh sl name nn (MethodRef (Ref mix)) (nm,(Idl.OperationDcl True _ _ rv pars)) =
   let   handle = text "_" <> (H.fromScopedName (scope cf)) <> text "_handle()"
         async = text $ "asyncDesc"
         asyncarg =  AS $ render $ text "fastrpc_async_descriptor_t *"<+>async
         mm = (methods sl) !! mix
         hasMethodArgs = if (iep==False) then nn >= 31 else False
         methodargs = if (hasMethodArgs && iep==False) then [int 31, text "&_mid"] else [text "_mid"]
   in    [( Function
            "__QAIC_STUB"
            (render $ H.typeName cf rv)
            (render $ H.scopedName cf nm)
            (asyncarg : (map (AS . render) $ H.fromParams cf pars))
            [  drs $ text "uint32_t _mid = "<> (int nn)
            , drs $ text "remote_handle _handle = _" <> text name <> text "_handle()"
            , E $ "if (_handle != (remote_handle)-1)" `BE` [E $ returnE $ methodF sl iep hrh True hasMethodArgs mm name `CE` (map ser $ text "_handle" : async : methodargs ++ castArgs cf sl mm pars name)]
            , F $ "else {"
            , F $ (render (nest 4 (text ("return AEE_EINVHANDLE;"))))
            , F $  " }"]
         )]

interfaceF cf iep hrh sl name nn (MethodRef (Ref mix)) (nm,(Idl.OperationDcl False _ _ rv pars)) =
   let   handle = text "_" <> (H.fromScopedName (scope cf)) <> text "_handle()"
         mm = (methods sl) !! mix
         hasMethodArgs = if (iep==False) then nn >= 31 else False
         methodargs = if (hasMethodArgs && iep==False) then [int 31, text "&_mid"] else [text "_mid"]
   in    [( Function
            "__QAIC_STUB"
            (render $ H.typeName cf rv)
            (render $ H.scopedName cf nm)
            (map (AS . render) $ H.fromParams cf pars)
            [  drs $ text "uint32_t _mid = "<> (int nn)
            , drs $ text "remote_handle _handle = _" <> text name <> text "_handle()"
            , E $ "if (_handle != (remote_handle)-1)" `BE` [E $ returnE $ methodF sl iep hrh False hasMethodArgs mm name `CE` (map ser $ text "_handle" : methodargs ++ castArgs cf sl mm pars name)]
            , F $ "else {"
            , F $ (render (nest 4 (text ("return AEE_EINVHANDLE;"))))
            , F $  " }"]
         )]

interfaceF _ _ _ _ _ _ _ _  = []

remoteHandleF :: Cfg -> Slim -> Int -> MethodRef -> (Idl.Identifier, Idl.Definition) -> [Function]
remoteHandleF cf _ 0 _ (func@(Idl.Identifier "open"), (Idl.OperationDcl _ _ _ rv pars)) =
         [( Function
            "__QAIC_STUB"
            (render $ H.typeName cf rv)
            (render $ H.scopedName cf func)
            (map (AS . render) $ H.fromParams cf pars)
            [ E $ returnE $ ser $ text "__QAIC_REMOTE(remote_handle64_open)" `call` (concatMap (H.parmNames cf) pars)]
         )]

remoteHandleF cf _ 1 _ (func@(Idl.Identifier "close"), (Idl.OperationDcl _ _ _ rv pars)) =
         [( Function
            "__QAIC_STUB"
            (render $ H.typeName cf rv)
            (render $ H.scopedName cf func)
            (map (AS . render) $ H.fromParams cf pars)
            [E $ returnE $ ser $ text "__QAIC_REMOTE(remote_handle64_close)" `call` (concatMap (H.parmNames cf) pars)]
         )]
remoteHandleF  _ _ _ _ (_, (Idl.OperationDcl _ sp _ _ _))  = printErrorO sp "unexpected remote_handle method"
remoteHandleF  _ _ _ _ _  = error "internal error: unexpected declaration in interface"

derivedRemoteHandleF :: Cfg -> Slim -> Int -> MethodRef -> (Idl.Identifier,Idl.Definition) -> String -> IsExtParams -> [Function]
derivedRemoteHandleF cf sl nn (MethodRef (Ref mix)) (nm,(Idl.OperationDcl True _ _ rv (pars))) name iep =
   let   handle = text $ "_handle"
         async = text $ "asyncDesc"
         asyncarg =  AS $ render $ text "fastrpc_async_descriptor_t* "<+>async
         harg = AS $ render $ text "remote_handle64" <+> handle
         mm = (methods sl) !! mix
         hasMethodArgs = if (iep==False) then nn >= 31 else False
         methodargs = if (hasMethodArgs && iep==False) then [int 31, text "&_mid"] else [text "_mid"]
   in    [( Function
            "__QAIC_STUB"
            (render $ H.typeName cf rv)
            (render $ H.scopedName cf nm)
            (harg : asyncarg : (map (AS . render) $ H.fromParams cf pars))
            [ drs $ text "uint32_t _mid = "<> (int nn)
            , E $ returnE $ methodF sl iep True True hasMethodArgs mm name`CE` (map ser $ handle : async : methodargs ++ castArgs cf sl mm pars name)]
         )]
derivedRemoteHandleF cf sl nn (MethodRef (Ref mix)) (nm,(Idl.OperationDcl False _ _ rv (pars))) name iep =
   let   handle = text $ "_handle"
         harg = AS $ render $ text "remote_handle64" <+> handle
         mm = (methods sl) !! mix
         hasMethodArgs = if (iep==False) then nn >= 31 else False
         methodargs = if (hasMethodArgs && iep==False) then [int 31, text "&_mid"] else [text "_mid"]
   in    [( Function
            "__QAIC_STUB"
            (render $ H.typeName cf rv)
            (render $ H.scopedName cf nm)
            (harg : (map (AS . render) $ H.fromParams cf pars))
            [ drs $ text "uint32_t _mid = "<> (int nn)
            , E $ returnE $ methodF sl iep True False hasMethodArgs mm name`CE` (map ser $ handle : methodargs ++ castArgs cf sl mm pars name)]
         )]
derivedRemoteHandleF _ _ _ _ _ _ _ = error "internal error: unexpected declaration in interface"

printError :: Pos.SourcePos -> String -> a
printError sp str = error (TP.errorMessage sp str)

printErrorO :: (Maybe (Pos.SourcePos, a))  -> String -> b
printErrorO (Just (sp,_))  str = error (TP.errorMessage sp str)
printErrorO Nothing str = error $ "error:" ++ str

castArgs :: Cfg -> Slim -> Method -> [Idl.Parameter] -> String -> [Doc]
castArgs cf sl mm pars name =
   let
         args = methodArgsOnlyl $ (serializeParameters sl False  (methodParameters sl mm) name )
   in    zipWith valToRefl args (concatMap (H.parmInROutNames cf) pars)

ifaceMethodRefs :: Slim -> [String] -> [MethodRef]
ifaceMethodRefs sl nm =
   let isIface (Interface iface _ _ _ _ _) = iface == nm
       fromIface (Interface _ num (MethodRefArrayRef (Ref ix)) _ _ _) = take num (drop ix (methodArrays sl))
       fromIface _   = error "internal error: expected ref"
       single [a]    = a
       single _      = error "internal error: expected only one matching interface"
   in  fromIface $ single $ filter isIface (interfaces sl)

stub :: HasRemoteHandle -> String -> Doc
stub False iface = text $ replace "INTERFACE" iface $ CStub.source
stub True iface = text "\n"


-- Removing as part of QAIC Source Code Cleanup

-- stub True iface = vcat $ map (text . (replace "INTERFACE" iface)) $
--    [ "__QAIC_STUB_EXPORT int __QAIC_STUB(INTERFACE_skel_handle_invoke)(remote_handle64 _h, uint32_t _sc, remote_arg* _pra) __QAIC_STUB_ATTRIBUTE {"
--    , "   return __QAIC_REMOTE(remote_handle64_invoke)(_h, _sc, _pra);"
--    , "}"]


-- glu :: [a] -> [a] -> [a]
-- glu _ [] = []
-- glu aa rs = aa ++ rs

-- isObjHandleParam :: Cfg -> Idl.Parameter -> Bool
-- isObjHandleParam cf (Idl.Parameter _ Idl.ParameterIn tp) = (render $ H.typeName cf tp) == (render $ text "remote_handle")
-- isObjHandleParam _ _ = False

-- paramId :: Idl.Parameter -> String
-- paramId (Idl.Parameter (Idl.Identifier nm) _ _) = nm
