-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause

module Language.SO.ToCSkel where

import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Language.Idl.Data                    as Idl
import qualified Language.SO.ToHeader                 as H
import qualified Language.SO.Allocator                as Allocator
import qualified Language.SO.Wstring                  as Wstring
import qualified Language.SO.Slim                     as Slim
import Language.SO.ToHeader                           ( commals, argTypeList, lookupIface )
import Language.SO.ToCStub(ifaceMethodRefs, isMethod)
import Language.Slim.Serialize
import qualified Language.Slim.Serialize as S
import Language.Slim.Doc(toC)
import Text.PrettyPrint
import Language.Slim.Data
import Language.Slim.Ref
import Language.SO.Cfg
import Language.SO.Args
import Language.SO.Pack
import Language.SO.Code
import Compiler.Version as V
import Prelude hiding ( (<>) )
type HasRemoteHandle = Bool
type IsIdlVersionEnabled = Bool

methodF :: Slim -> IsIdlVersionEnabled -> IsExtParams -> HasRemoteHandle -> Bool -> HasMethodArg -> Method -> String -> Function
methodF sl isIDLVersionEnabled iep hrh asc hma mm nme=
   let
         pars = methodParameters sl mm
         witharg = serializeParameters sl hma pars  nme
         typenames = notMethodArg hma $ methodValTypes $ filter isNotInIR $ witharg
         pfn | hrh && asc = (render $ text "int (*_pfn)" <> parens (commals ((text "remote_handle64"):(text "fastrpc_async_descriptor_t* asyncDesc"):typenames)))
             | hrh && not asc = (render $ text "int (*_pfn)" <> parens (commals ((text "remote_handle64"):typenames)))
             | not hrh && asc = (render $ text "int (*_pfn)" <> parens (commals ((text "fastrpc_async_descriptor_t* asyncDesc"):typenames)))
             | otherwise = (render $ text "int (*_pfn)" <> parens (argTypeList typenames))
         args = if hrh
                then [pfn, "remote_handle64 _h", "uint32_t _sc", "remote_arg* _pra"]
                else [pfn, "uint32_t _sc", "remote_arg* _pra"]
         args_s64 = if hrh
                then [pfn, "remote_handle64 _h", "uint64_t _sc_64", "remote_arg* _pra"]
                else [pfn, "uint64_t _sc_64", "remote_arg* _pra"]
   in
      if iep
      then  Function
            ""
            "static __inline int"
            "_skel_method_s64"
            (map AS args_s64)
            (implementParams hrh asc hma isIDLVersionEnabled witharg iep (scalars hma mm))
      else  Function
            ""
            "static __inline int"
            "_skel_method"
            (map AS args)
            (implementParams hrh asc hma isIDLVersionEnabled witharg iep (scalars hma mm))


notMethodArg :: HasMethodArg -> [a] -> [a]
notMethodArg False sereal = sereal
notMethodArg True [] = error "internal error: notMethodArg"
notMethodArg True ls = tail ls

rhArg :: HasRemoteHandle -> [Doc] -> [Doc]
rhArg False ls = ls
rhArg True ls = (text "_h"):ls

asyncArg :: Bool -> [Doc] -> [Doc]
asyncArg True ls = text "NULL" : ls
asyncArg False ls = ls

isNotInIR :: SerealArg -> Bool
isNotInIR sa = not $ saIsIR sa && saIsIn sa

compareVersions :: Function
compareVersions = Function
                  ""
                  "static __inline int"
                  "_compare_versions"
                  [AS $ "char* stub_ver, char* skel_ver, int* result"]
                  [er $ text $ "unsigned long int major_stub = 0, minor_stub = 0, patch_stub = 0;",
                   er $ text $ "unsigned long int major_skel = 0, minor_skel = 0, patch_skel = 0;",
                   er $ text $ "char *saveptr1 = NULL;",
                   er $ text $ "char *token1 = NULL;",
                   er $ text $ "char *saveptr2 = NULL;",
                   er $ text $ "char *token2 = NULL;",
                   er $ text $ "int i=0;",
                   E $ ser $ (H.internalBlockCode (text "for (i=0, token1 = strtok_r(stub_ver, \".\", &saveptr1); i<3 && token1 != NULL; i++, token1 = strtok_r(NULL, \".\", &saveptr1))")
                   [text "unsigned long int tn = strtoul(token1, NULL,10);",
                    H.internalBlockCode (text "if( tn > 999)") [text $ "*result=-1;", text "return 0;"] 0,
                    H.internalBlockCode (text "else") [text "if(i==0) major_stub=tn;",
                                                     text "if(i==1) minor_stub=tn;",
                                                     text "if(i==2) patch_stub=tn;"] 0] 3),
                   E $ ser $ (H.internalBlockCode (text "for (i=0, token2 = strtok_r(skel_ver, \".\", &saveptr2); i<3 && token2 != NULL; i++, token2 = strtok_r(NULL, \".\", &saveptr2))")
                   [text "unsigned long int tn = strtoul(token2, NULL,10);",
                    H.internalBlockCode (text "if( tn > 999)") [text $ "*result=-1;", text "return 0;"] 0,
                    H.internalBlockCode (text "else") [text "if(i==0) major_skel=tn;",
                                                     text "if(i==1) minor_skel=tn;",
                                                     text "if(i==2) patch_skel=tn;"] 0] 3),
                   E $ ser $ (H.internalBlockCode (text "if(major_stub<major_skel)") [text "*result=1;", text "return 0;"] 3),
                   E $ ser $ (H.internalBlockCode (text "else if(major_stub==major_skel)")
                      [H.internalBlockCode (text "if( minor_stub < minor_skel )") [text "*result=1;", text "return 0;"] 0,
                       H.internalBlockCode (text "else if((minor_stub == minor_skel) && (patch_skel>=patch_stub))") [text "*result=1;", text "return 0;"] 0] 3),
                   er $ text $ "*result=-1;",
                   F $ "return 0;"
                   ]

stubSkelVersionCheck :: Function
stubSkelVersionCheck = Function
                       ""
                       "static __inline int"
                       "_stub_skel_version_check"
                       [AS $ "char*_in0, int* resVal"]
                       ([ er $ text "char* p = strstr(_in0, \"_idlver=\");"
                        , E $ ser $ H.internalBlockCode (text "if(!p)") [text "*resVal = -1;", text "return 0;"] 3
                        , er $ text "p+=8;"
                        , er $ text "int i=0,len=0, comVer=0,num_delimit=0, updtInxStub=0, updtInxSkel=0;"
                        , E $ ser $ (H.internalBlockCode (text "for(i=0;i<strlen(p);i++)") [
                        H.internalBlockCode (text "if(num_delimit>2)") [text "*resVal = -1;",text "return 0;"] 0,
                        H.internalBlockCode (text "if ((p[i]>='0' && p[i]<='9') || (p[i]=='.'))" ) [text "len++;", H.internalBlockCode (text "if(p[i]=='.')") [text "num_delimit++;"] 0] 0,
                        H.internalBlockCode (text "else if(p[i]=='&')") [text "break;"] 0,
                        H.internalBlockCode (text "else") [text "*resVal = -1;", text "return 0;"] 0] 3)
                        , er $ text "char* stubVer=(char*)MALLOC(len+1);"]
                        ++ assert (text "stubVer!=NULL")
                        ++ [ E $ ser $ (H.internalBlockCode (text "for(i=0;i<strlen(p);i++)") [
                           H.internalBlockCode (text "if((p[i]>=\'0\' && p[i]<=\'9\') || (p[i]==\'.\'))") [text "stubVer[updtInxStub]=p[i];", text "updtInxStub++;"] 0,
                           H.internalBlockCode (text "else if(p[i]==\'&\')") [text "break;"] 0] 3)
                           , er $ text "stubVer[len]=\'\\0\';"
                           , er $ text "char* skelVer=(char*)MALLOC(strlen(IDL_VERSION)+1);"]
                        ++ assert (text "skelVer!=NULL")
                        ++ [  E $ ser $ (H.internalBlockCode (text "for(i=0;i< strlen(IDL_VERSION);i++)") [ text "skelVer[updtInxSkel]=IDL_VERSION[i];", text "updtInxSkel++;"] 3)
                           , er $ text "skelVer[strlen(IDL_VERSION)]=\'\\0\';"]
                        ++ (try $ compareVersions `CE` [SE $ "stubVer", SE $ "skelVer", SE $ "&comVer"])
                        ++ [er $ text "*resVal = 0;"
                           ,E $ ser $ H.internalBlockCode (text "if (comVer==-1)") [text "*resVal = -1;"] 3
                           , er $ text "FREE(stubVer);"
                           , er $ text "FREE(skelVer);"
                           , F $ "return 0;"])

stubSkelMismatch :: IsIdlVersionEnabled -> [Code]
stubSkelMismatch isIDLVersionEnabled =
   if isIDLVersionEnabled
      then ([ er $ text "int resVal" <> semi]
           ++ (try $ stubSkelVersionCheck `CE` [SE $ "*_in0", SE $ "&resVal"])
           ++ [E $ ser $ (H.internalBlockCode (text "if(resVal==-1)") [text "return AEE_ESTUBSKELVERMISMATCH;"] 3)])
      else []

implementParams :: HasRemoteHandle -> Bool -> HasMethodArg -> IsIdlVersionEnabled -> [SerealArg] -> IsExtParams -> Scalars -> [Code]
implementParams hrh asc hma isIDLVersionEnabled witharg iep sc =
          [D $ CD $ praEnd iep]
    ++   (map drs $ (map (<> text " = {0}") $ declareRefsl witharg))
    ++   validateScalar sc witharg iep
    ++   assert ((text "_pra" `plus` ((numBufs witharg) `plus` (numAllHandles witharg))) `lteq` text "_praEnd")
    ++   (declarePrims witharg iep)
    ++   ifcrout witharg [D $ CD $ ppraROutPost witharg]
    ++   concat (zipWith (skelUnpack witharg) witharg (repeat iep))
    ++   (concatMap (skelCopyIR witharg) witharg)
    ++   (stubSkelMismatch isIDLVersionEnabled)
    ++   (try $ ser $ text "_pfn" <> parens (commals $ rhArg hrh $ asyncArg asc $ notMethodArg hma $ refsToMethodVals $ filter isNotInIR $ witharg))
    ++   (concatMap (skelPack witharg) witharg)
    ++   [ F $ "return _nErr;" ]

validateScalar :: Scalars -> [SerealArg] -> IsExtParams -> [Code]
validateScalar sc sereal iep
   | (hasComplex sereal) =
      let
         returnValue  = if iep
                        then (assert (text "REMOTE_SCALARS_INBUFS_64(_sc_64)>="<>(int $ numPrimBufs In sereal))
                              ++ assert(text "REMOTE_SCALARS_OUTBUFS_64(_sc_64)>="<>(int $ numPrimBufs ROut sereal)))
                        else (assert (text "REMOTE_SCALARS_INBUFS(_sc)>="<>(int $ numPrimBufs In sereal))
                              ++ assert(text "REMOTE_SCALARS_OUTBUFS(_sc)>="<>(int $ numPrimBufs ROut sereal)))
      in
         if iep
         then
            if (hasDmahandleInSerealArg sereal)
            then (returnValue ++ assert(text "REMOTE_SCALARS_INHANDLES_64(_sc_64)>="<>(int $ numHandles In sereal))
                  ++ assert(text "REMOTE_SCALARS_OUTHANDLES_64(_sc_64)>="<>(int $ numHandles ROut sereal)))
            else (returnValue ++ assert(text "REMOTE_SCALARS_INHANDLES_64(_sc_64)=="<>(int $ numHandles In sereal))
                  ++ assert(text "REMOTE_SCALARS_OUTHANDLES_64(_sc_64)=="<>(int $ numHandles ROut sereal)))
         else
            if (hasDmahandleInSerealArg sereal)
            then (returnValue ++ assert(text "REMOTE_SCALARS_INHANDLES(_sc)>="<>(int $ numHandles In sereal))
                  ++ assert(text "REMOTE_SCALARS_OUTHANDLES(_sc)>="<>(int $ numHandles ROut sereal)))
            else (returnValue ++ assert(text "REMOTE_SCALARS_INHANDLES(_sc)=="<>(int $ numHandles In sereal))
                  ++ assert(text "REMOTE_SCALARS_OUTHANDLES(_sc)=="<>(int $ numHandles ROut sereal)))

   | otherwise = if iep
                 then (assert (text "REMOTE_SCALARS_INBUFS_64(_sc_64)=="<>(iint $ bufsIn sc))
                      ++ assert(text "REMOTE_SCALARS_OUTBUFS_64(_sc_64)=="<>(iint $ bufsROut sc))
                      ++ assert(text "REMOTE_SCALARS_INHANDLES_64(_sc_64)=="<>(iint $ objsIn sc))
                      ++ assert(text "REMOTE_SCALARS_OUTHANDLES_64(_sc_64)=="<>(iint $ objsROut sc)))
                 else (assert (text "REMOTE_SCALARS_INBUFS(_sc)=="<>(iint $ bufsIn sc))
                      ++ assert(text "REMOTE_SCALARS_OUTBUFS(_sc)=="<>(iint $ bufsROut sc))
                      ++ assert(text "REMOTE_SCALARS_INHANDLES(_sc)=="<>(iint $ objsIn sc))
                      ++ assert(text "REMOTE_SCALARS_OUTHANDLES(_sc)=="<>(iint $ objsROut sc)))

declarePrims :: [SerealArg] -> IsExtParams -> [Code]
declarePrims args scalar_64 =
      declarePrim args (primaryBufs In args) scalar_64
   ++ declarePrim args (primaryBufs ROut args) scalar_64
   ++ declarePrimRHandles In (numRemoteHandles In args) scalar_64
   ++ declarePrimRHandles ROut (numRemoteHandles ROut args) scalar_64
   ++ declarePrimHandles args In (numHandles In args) scalar_64
   ++ declarePrimHandles args ROut (numHandles ROut args) scalar_64

declarePrimRHandles :: S.Mode -> Int -> IsExtParams -> [Code]
declarePrimRHandles _ 0 _ = []
declarePrimRHandles In _ False = [D $ SD "remote_arg* _praRHandleIn = _pra + REMOTE_SCALARS_INBUFS(_sc) +  REMOTE_SCALARS_OUTBUFS(_sc);"]
declarePrimRHandles ROut _ False = [D $ SD "remote_arg* _praRHandleROut = _pra + REMOTE_SCALARS_INBUFS(_sc) + REMOTE_SCALARS_OUTBUFS(_sc) + REMOTE_SCALARS_INHANDLES(_sc) ;"]
declarePrimRHandles In _ True = [D $ SD "remote_arg* _praRHandleIn = _pra + REMOTE_SCALARS_INBUFS_64(_sc_64) +  REMOTE_SCALARS_OUTBUFS_64(_sc_64);"]
declarePrimRHandles ROut _ True = [D $ SD "remote_arg* _praRHandleROut = _pra + REMOTE_SCALARS_INBUFS_64(_sc_64) + REMOTE_SCALARS_OUTBUFS_64(_sc_64) + REMOTE_SCALARS_INHANDLES_64(_sc_64) ;"]

numInH :: [SerealArg] -> S.Mode -> IsExtParams -> Contextual
numInH args _ False
   | (hasComplexROut args) = Contextual $ [ D $ SD $ "int _numInH[1] = {0};"
                                          , D $ SD $ "int _numROut[1] = {0};"
                                          , er $ text "_numInH[0] = REMOTE_SCALARS_INHANDLES(_sc);"
                                          , er $ text "_numROut[0] = REMOTE_SCALARS_OUTBUFS(_sc);" ]
   | (hasComplex args) = Contextual $ [ D $ SD $ "int _numInH[1] = {0};"
                                      , er $ text "_numInH[0] = REMOTE_SCALARS_INHANDLES(_sc);" ]
numInH args _ True
   | (hasComplexROut args) = Contextual $ [ D $ SD $ "int _numInH[1] = {0};"
                                          , D $ SD $ "int _numROut[1] = {0};"
                                          , er $ text "_numInH[0] = REMOTE_SCALARS_INHANDLES_64(_sc_64);"
                                          , er $ text "_numROut[0] = REMOTE_SCALARS_OUTBUFS_64(_sc_64);" ]
   | (hasComplex args) = Contextual $ [ D $ SD $ "int _numInH[1] = {0};"
                                      , er $ text "_numInH[0] = REMOTE_SCALARS_INHANDLES_64(_sc_64);" ]
numInH _ _ _= Contextual $ []

declarePrimHandles :: [SerealArg] -> S.Mode -> Int -> IsExtParams -> [Code]
declarePrimHandles args _ 0 scalar_64
   | (hasComplex args) = [ D $ CD $ numInH args In scalar_64 ]
declarePrimHandles _ _ 0 _ = []
declarePrimHandles args In _ scalar_64 = [ D $ CD $ numInH args In scalar_64 ]
declarePrimHandles args ROut _ scalar_64 = [ D $ CD $ numInH args ROut scalar_64 ]

declarePrim :: [SerealArg] -> [Primary] -> IsExtParams -> [Code]
declarePrim args [pp@(Prim In _ sz)] _
   |  (not $ hasComplex args) && (numAllStaticBufs ROut args == 0) =
      [ dr $ typeFromAlS False (snd sz) <> text "*" <+> prim pp <> text "= 0" <> semi ]
 ++   assert (text nLenPrimIn `gteq` (size sz ))
 ++   [ er $ text "_primIn" `eq` text pvPrimIn <> semi]

declarePrim args [pp@(Prim In _ sz)] scalar_64 =
      [ dr $ typeFromAlS False (snd sz) <> text "*" <+> prim pp <> text "= 0" <> semi
      , D $ SD $ "int _numIn[1] = {0};"
      , er $ if scalar_64 then text "_numIn[0]" `eq` (text "REMOTE_SCALARS_INBUFS_64(_sc_64)" `minus` (int $ numPrimBufs In args)) <> semi
             else text "_numIn[0]" `eq` (text "REMOTE_SCALARS_INBUFS(_sc)" `minus` (int $ numPrimBufs In args)) <> semi ]
 ++   assert (text nLenPrimIn `gteq` (size sz ))
 ++   [ er $ text "_primIn" `eq` text pvPrimIn <> semi]

declarePrim args [pp@(Prim ROut _ sz)] scalar_64 =
      [ dr $ typeFromAlS False (snd sz) <> text "*" <+> prim pp <> text "= 0" <> semi
      , D $ SD $ "int _numIn[1] = {0};"
      , er $ if scalar_64 then text "_numIn[0]" `eq` (text "REMOTE_SCALARS_INBUFS_64(_sc_64)" `minus` (int $ numPrimBufs In args)) <> semi
             else text "_numIn[0]" `eq` (text "REMOTE_SCALARS_INBUFS(_sc)" `minus` (int $ numPrimBufs In args)) <> semi ]
 ++   assert (text (nLenPrimROut args) `gteq` (size sz ))
 ++   [ er $ text "_primROut" `eq` text (pvPrimROut args) <> semi]

declarePrim _ _ _ = []

toSkel ::  Cfg -> [Pos.SourceName] -> Idl.Idl -> [(String, Doc)]
toSkel cf files (Idl.Idl decs) =
   let
         decSourceName (Idl.TopLevelDeclaration fp _) = fp
         mine = filter (\ dd -> (decSourceName dd) `elem` files) decs
         comments = vcat $ map H.fromTopComment mine
         addComment (ff,doc) = (ff, comments $+$ doc)
         isidlv = filter (/="") (map H.isIDLVersion' decs)
   in    map addComment $ concatMap (fromTop cf (H.checkVersionValidity isidlv)) mine

fromTop :: Cfg -> H.IsIdlVersionEnabled -> Idl.TopLevelDeclaration -> [(String, Doc)]
fromTop cf idlver (Idl.TopLevelDeclaration _ dec) = fromDec cf idlver dec

fromDec :: Cfg -> H.IsIdlVersionEnabled -> Idl.Declaration -> [(String, Doc)]
fromDec cf idlver dec@(Idl.Declaration _ _ _ nm df) =
   fromInterface cf idlver nm (toSlim cf ((H.defScopedName cf nm), dec)) df
fromDec _ _ _                         = []


zipWith4 :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds) =  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _      =  []


fromInterface :: Cfg -> H.IsIdlVersionEnabled -> Idl.Identifier -> Slim -> Idl.Definition -> [(String, Doc)]
fromInterface cf idlver nm sl iface@(Idl.InterfaceDcl iep _ _ _ parent mydecs) =
   let
         name = (render $ H.scopedName cf nm)
         slimFile = name ++ "_slim.h"
         slimDoc = (toC sl)
         interfaceFile = name ++ "_skel.c"
         alldecs :: [Idl.Declaration]
         alldecs = (concatMap (H.getDerivedMethods cf) (lookupIface cf parent)) ++ mydecs
         methodRefs = ifaceMethodRefs sl (scope $ H.withScope cf nm)
         isIDLVersion = if (length idlver >= 1) then True else False
         cases scalar_64 = zipWith4 (callFunc (H.withScope cf nm) sl hasRH isIDLVersion name) [0..] methodRefs (filter isMethod $ alldecs) (repeat scalar_64)
         (upto30,rest) = splitAt 31 (cases False)
         casecode = (concat upto30) ++ (case31 hasRH $ concat rest)
         casecode_s64 = cases True
         len = length(methodRefs)
         skel_hi = "switch(REMOTE_SCALARS_METHOD_64(_sc_64))"
         stringh = text "#include <string.h>"
         hasRH = not $ null $ H.getRemoteHandleMethods cf iface
         invokeName = if hasRH
                      then (text name <> text "_skel_handle_invoke")
                      else (text name <> text "_skel_invoke")
         invokeArgs = if hasRH
                      then ["remote_handle64 _h", "uint32_t _sc", "remote_arg* _pra"]
                      else ["uint32_t _sc", "remote_arg* _pra"]
         skel_invoke = if hasRH
                      then (if iep then name ++ "_skel_handle_invoke_s64" else name ++ "_skel_handle_invoke")
                      else (if iep then name ++ "_skel_invoke_s64" else name ++ "_skel_invoke")
         invokeName_s64 = if hasRH
                      then (text name <> text "_skel_handle_invoke_s64")
                      else (text name <> text "_skel_invoke_s64")
         invokeArgs_s64 = if hasRH
                      then ["remote_handle64 _h", "uint64_t _sc_64", "remote_arg* _pra"]
                      else ["uint64_t _sc_64", "remote_arg* _pra"]
         ver_name = "_ATTRIBUTE_VISIBILITY uint32_t "++ skel_invoke ++ "_qaic_version = " ++ V.str ++ ";"
         replace' = (H.replace "SKELNAME" (skelName cf)) . (H.replace "INTERFACE" name)
         intrfc_uri = replace' (H.interface_uri cf name iep idlver)
          -- we subtracted 2 from length of intrfc_uri as intrfc_uri has 2 extra '\' which won't be printed in skel.c
         intrfc_dec = let invokenm = if (iep == False) then (render invokeName) else (render invokeName_s64)
                      in if hasRH
                         then "_ATTRIBUTE_VISIBILITY char "++invokenm++"_uri["++(show ((length intrfc_uri)-2))++"+1]="++ intrfc_uri++";"
                         else ""
         docs = (H.toInclude $ moduleName cf)
            $+$ text ""
            $+$ (if (H.hasString alldecs) then stringh else empty)
            $+$ (if (H.hasWString alldecs) then text Wstring.source else empty)
            $+$ text "#ifndef _WIN32"
            $+$ text "#include \"HAP_farf.h\"" -- Moving HAP_farf.h inclusion to skel.c file as if anybody includes both autogen header files and HAP_farf.h in their code and enables any of the FARF flags then there would be "macro redefinition error"
            $+$ text "#endif //_WIN32 for HAP_farf"
            $+$ text Allocator.source
            $+$ text Slim.source
            $+$ (snd $ H.ifdef (slimFile, slimDoc))
            $+$ text "extern int adsp_mmap_fd_getinfo(int, uint32_t *);"
            $+$ text "#ifdef __cplusplus"
            $+$ text "extern \"C\" {"
            $+$ text "#endif"
            $+$ text ver_name
            $+$ text intrfc_dec
            $+$ (if (iep == False)
                then (compile [(
                  Function
                  "__QAIC_SKEL"
                  "int"
                  (render invokeName)
                  (map AS invokeArgs)
                  [ E $ BE "switch(REMOTE_SCALARS_METHOD(_sc))" casecode
                  , F  $ "return AEE_EUNSUPPORTED;"]
                  )] )
                else (compile [(
                  Function
                  "__QAIC_SKEL"
                  "int"
                  (render invokeName_s64)
                  (map AS invokeArgs_s64)
                  [ E $ BE skel_hi (concat $ casecode_s64)
                  , F  $ "return AEE_EUNSUPPORTED;"]
                  )] ))
            $+$ text "#ifdef __cplusplus"
            $+$ text "}"
            $+$ text "#endif"
   in    if (iep==True && len > 8192)
         then error(" Number of methods must be < 8192 if ext_params keyword is used in IDL")
         else [(interfaceFile, docs)]

fromInterface cf idlver nm _ (Idl.ModuleDcl decs)       = concatMap (fromDec (H.withScope cf nm) idlver) decs
fromInterface _ _ _  _ _                          = []

callFunc :: Cfg -> Slim -> HasRemoteHandle -> IsIdlVersionEnabled -> String -> Int -> MethodRef -> Idl.Declaration -> IsExtParams -> [Code]
callFunc cf sl hrh idlver nme ii mm  (Idl.Declaration _ _ _ nm df) scalar_64 = callFunc' cf sl hrh idlver nm ii mm df nme scalar_64
callFunc _ _ _ _ _ _ _ _ _                                  = []

callFunc' :: Cfg -> Slim -> HasRemoteHandle -> IsIdlVersionEnabled -> Idl.Identifier -> Int -> MethodRef -> Idl.Definition -> String -> IsExtParams -> [Code]
callFunc' cf sl hrh idlver nm nn (MethodRef (Ref ix)) (Idl.OperationDcl True _ _ _ _) nme scalar_64 =
   let   pfn = render $ text "__QAIC_IMPL" <> parens (H.scopedName cf nm)
         isIDLVersionEnabled = (idlver) && ((render (H.scopedName cf nm)) == (nme++"_open"))
         hrh' = hrh && nn > 1
         args = if hrh'
                then [pfn, "_h", "_sc", "_pra"]
                else [pfn, "_sc", "_pra"]
         args_s64 = if hrh'
                then [pfn, "_h", "_sc_64", "_pra"]
                else [pfn, "_sc_64", "_pra"]
         hma = if (scalar_64==False) then nn >= 31 else False
         funcE = if scalar_64
               then returnE $ methodF sl isIDLVersionEnabled scalar_64 hrh' True hma (methods sl !! ix) nme `CE` (map SE args_s64)
               else returnE $ methodF sl isIDLVersionEnabled scalar_64 hrh' True hma (methods sl !! ix) nme `CE` (map SE args)
   in    caseC nn funcE

callFunc' cf sl hrh idlver nm nn (MethodRef (Ref ix)) (Idl.OperationDcl False _ _ _ _) nme scalar_64 =
   let   pfn = render $ text "__QAIC_IMPL" <> parens (H.scopedName cf nm)
         isIDLVersionEnabled = (idlver) && ((render (H.scopedName cf nm)) == (nme++"_open"))
         hrh' = hrh && nn > 1
         args = if hrh'
                then [pfn, "_h", "_sc", "_pra"]
                else [pfn, "_sc", "_pra"]
         args_s64 = if hrh'
                then [pfn, "_h", "_sc_64", "_pra"]
                else [pfn, "_sc_64", "_pra"]
         hma = if (scalar_64==False) then nn >= 31 else False
         funcE = if scalar_64
               then returnE $ methodF sl isIDLVersionEnabled scalar_64 hrh' False hma (methods sl !! ix) nme `CE` (map SE args_s64)
               else returnE $ methodF sl isIDLVersionEnabled scalar_64 hrh' False hma (methods sl !! ix) nme `CE` (map SE args)
   in    caseC nn funcE

callFunc' _ _ _ _ _ _ _ _ _ _ = []

caseC :: Int -> Expr -> [Code]
caseC ii expr =
   [er $ text "case" <+> int ii <> text ":"
   ,E expr]

case31 :: HasRemoteHandle -> [Code] -> [Code]
case31 _ [] = []
case31 False exprs = [E $ BE  "case 31:"
   [er $ text "uint32_t* _mid;"
   ,er $ text "if(REMOTE_SCALARS_INBUFS(_sc) < 1 || _pra[0].buf.nLen < 4) { return AEE_EBADPARM; }"
   ,er $ text "_mid = (uint32_t*)_pra[0].buf.pv;"
   ,E $ returnE $ (case31F False exprs) `CE` (map SE ["*_mid", "_sc", "_pra"])
   ]]

case31 True exprs = [E $ BE  "case 31:"
   [er $ text "uint32_t* _mid;"
   ,er $ text "if(REMOTE_SCALARS_INBUFS(_sc) < 1 || _pra[0].buf.nLen < 4) { return AEE_EBADPARM; }"
   ,er $ text "_mid = (uint32_t*)_pra[0].buf.pv;"
   ,E $ returnE $ (case31F True exprs) `CE` (map SE ["_h, *_mid", "_sc", "_pra"])
   ]]


case31F :: HasRemoteHandle -> [Code] -> Function
case31F False exprs =
   Function
   ""
   "static __inline int"
   "_skel_invoke"
   (map AS ["uint32_t _mid, uint32_t _sc", "remote_arg* _pra"])
   [ E $ BE "switch(_mid)" exprs, F  $ "return AEE_EUNSUPPORTED;"]

case31F True exprs =
   Function
   ""
   "static __inline int"
   "_skel_handle_invoke"
   (map AS ["remote_handle64 _h, uint32_t _mid, uint32_t _sc", "remote_arg* _pra"])
   [ E $ BE "switch(_mid)" exprs, F  $ "return AEE_EUNSUPPORTED;"]


