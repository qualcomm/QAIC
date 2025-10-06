-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Language.Slim.ToSlim where

import qualified Data.Omg.Prim                        as OP
import qualified Language.Idl.ToPalette               as TP
import qualified Language.Palette.Data               as TK
import qualified Language.Idl.Data                    as ID
import qualified Language.Slim.Alignment              as SA
import qualified Language.Slim.Names                  as SN
import qualified Language.Slim.Ref                    as Ref
import qualified Language.Slim.TypeRefMap             as TD
import qualified Language.Slim.Data                   as Slim
import qualified Language.Slim.State                  as State
import qualified Control.Monad.State                  as State
import qualified Text.ParserCombinators.Parsec.Pos    as Pos
import qualified Data.List                            as List
import qualified Data.Omg.Literal                     as L

import Control.Monad(liftM, when)
import Data.Word(Word32, Word8)
import Data.Int(Int32)
import Control.Monad.State(runState)
import Data.Maybe(fromJust)
import Data.Generics(listify)
import Language.Slim.Data(sznAl,sznSz)

fromDecs ::  [Pos.SourceName] -> (TP.TypeDictionary , TP.ConstDictionary) -> (TD.TypeRefMap, TD.ConstWordMap) -> [(ID.ScopedName, ID.Declaration)] -> Slim.Slim
fromDecs files palds (trm, iidmap) decs =
   let
         decSourceName (ID.Declaration sp _ _ _ _) = Pos.sourceName sp
         decSourceName (ID.CommentBlock {}) = ""
         mydecs = filter (\ (_,dd) -> (decSourceName dd) `elem` files) decs

         (ifacess,_) = runState (mapM (interfaceFromDec palds) mydecs) (State.new palds trm iidmap)
         (iids,_) = runState (mapM interfaceIIDFromDec $ snd $ unzip mydecs) (State.new palds trm iidmap)
         slim = Slim.new { Slim.interfaces = concat ifacess
                         , Slim.moduleIIds = List.nub $ concat iids
                         }

   in    mkMSArrs $ mkMStrings $ mkStrings $ mkWord8 $ mkWord16 $ mkWord32 $ mkWord64 $ mkMarrays $ mkMethods $ mkParars $ mkParams $ mkTypears $ mkUnions $ mkStructs $ mkSeqTypes $ mkTypes slim
   where
         mkMethods slim =
            let   (methods',  slimMethods') = Ref.nubAndUpdate slim id
            in    slimMethods' { Slim.methods = methods' }
         mkMarrays slim =
            let   (marrays',  slimMArrays')  = Ref.compressAndUpdate slim id
            in    slimMArrays' { Slim.methodArrays = marrays' }
         mkParams slim =
            let   (params',   slimParams') = Ref.nubAndUpdate slim id
            in    slimParams' { Slim.parameters = params'}
         mkParars slim =
            let   (parars',   slimParars')  = Ref.compressAndUpdate slim id
            in    slimParars' { Slim.parameterArrays = parars'}
         mkTypes slim =
            let   (types',    slimTypes')   = Ref.nubAndUpdate  slim id
            in    slimTypes' { Slim.types = types' }
         mkTypears slim =
            let   (typars',   slimTypears')  = Ref.compressAndUpdate slim id
            in    slimTypears' { Slim.typeArrays = typars' }
         mkStructs slim =
            let   (structs',  slimStructs')  = Ref.nubAndUpdate slim id
            in    slimStructs' { Slim.structTypes = structs' }
         mkUnions slim =
            let   (unions',   slimUnions')   = Ref.nubAndUpdate slim id
            in    slimUnions' { Slim.unionTypes = unions' }
         mkSeqTypes slim =
            let   (seqtypes',   slimSeqTypes')   = Ref.nubAndUpdate slim id
            in    slimSeqTypes' { Slim.sequenceTypes = seqtypes' }
         mkWord64 slim =
            let   (word64s,  slim64')   = Ref.compressAndUpdate slim id
            in    slim64' { Slim.value64s = word64s }
         mkWord32 slim =
            let   (word32s,  slim32')   = Ref.compressAndUpdate slim id
            in    slim32' { Slim.value32s = word32s }
         mkWord16 slim =
            let   (word16s,  slim16')   = Ref.compressAndUpdate slim id
            in    slim16' { Slim.value16s = word16s }
         mkWord8 slim =
            let   (word8s,  slim8')   = Ref.compressAndUpdate slim id
            in    slim8' { Slim.value8s = word8s }
         mkStrings slim =
            let   (strings',  slimStrings')   = Ref.compressAndUpdate slim $ ((\ vv -> vv ++ "\0") :: String -> String)
            in    slimStrings' { Slim.strings = strings' }
         mkMStrings slim =
            let   (mstrings', slimMStrings')   = Ref.compressAndUpdate slim methodStringsToList
            in    slimMStrings' { Slim.methodStrings = mstrings' }
         mkMSArrs slim =
            let   (msarrays', slimMSArs')   = Ref.compressAndUpdate slim id
            in    slimMSArs' { Slim.methodStringsArrays = msarrays' }

methodStringsToList :: Slim.MethodStrings -> [Slim.StringRef]
methodStringsToList ms = listify isStringRef ms
   where
         isStringRef (Slim.StringRef _) = True

interfaceIIDFromDec ::  (ID.Declaration) -> State.StateM ([Slim.AEEIID])
interfaceIIDFromDec (ID.CommentBlock {})        = return []
interfaceIIDFromDec (ID.Declaration _ _ _ _ df) = interfaceIIDFromDef df

interfaceIIDFromDef :: ID.Definition -> State.StateM ([Slim.AEEIID])
interfaceIIDFromDef (ID.InterfaceDcl _ _ _ ifaceid _ decs) = do
   iid <- State.lookupIId ifaceid
   rest <- liftM concat $ mapM interfaceIIDFromDec decs
   return $ iid `mjoin` rest


interfaceIIDFromDef (ID.ModuleDcl decs) = liftM concat $ mapM interfaceIIDFromDec decs
interfaceIIDFromDef _                   = return []

mjoin :: Maybe a -> [a] -> [a]
mjoin Nothing rest = rest
mjoin (Just aa) rest = aa:rest

interfaceFromDec ::  (TP.TypeDictionary , TP.ConstDictionary) -> (ID.ScopedName, ID.Declaration) -> State.StateM ([Slim.Interface])
interfaceFromDec _ (_, ID.CommentBlock {})        = return []
interfaceFromDec palds (sn,ID.Declaration _ _ _ _ df) = interfaceFromDef palds sn df

interfaceFromDef :: (TP.TypeDictionary , TP.ConstDictionary) -> ID.ScopedName -> ID.Definition -> State.StateM ([Slim.Interface])
interfaceFromDef palds sn (ID.InterfaceDcl _ False False ifaceid diface decs) = do
   let
         (methods, _) = List.partition isOperationDcl decs

   dmethods <- getDerivedMethodRefs diface palds
   diids <- getDerivedIIds diface
   mymethods <- mapM (getMethodRef palds)  methods
   dmethodStrings <- getDerivedMethodStrings diface
   myMethodStrings <- mapM SN.getMethodStrings methods
   myiid <- State.lookupIId ifaceid
   let
         dall = myiid `mjoin` diids
         methodarray = Slim.MethodRefArrayRef (Ref.Val (dmethods ++ mymethods))
         iids = Slim.AEEIIDArrayRef (Ref.Val dall)
         numIds = (length dall)
         methodStrings = dmethodStrings ++ myMethodStrings
         methodStringsRefs = map (Slim.MethodStringsRef . Ref.Val) methodStrings
         methodStringsArray = Slim.MethodStringsArrayRef  (Ref.Val methodStringsRefs)
         numMethods = fromIntegral $ (length dmethods) + (length mymethods)
         siface = Slim.Interface sn numMethods methodarray numIds iids methodStringsArray
   return [siface]

interfaceFromDef _ _ _                   = return []

getDerivedIIds :: (Pos.SourcePos, Maybe ID.ScopedName) -> State.StateM [Word32]
getDerivedIIds (_,Nothing)    = return []
getDerivedIIds (sp,(Just sn)) = do
   def <- State.lookupDef (sp,sn)
   getDerivedIIds' def

getDerivedIIds' :: (ID.Definition) -> State.StateM [Word32]
getDerivedIIds' (ID.InterfaceDcl _ _ _ ifaceid diface _) = do
   iid <- State.lookupIId ifaceid
   diids <- getDerivedIIds diface
   return (iid `mjoin` diids)

getDerivedIIds' _ = return []

getDerivedMethodRefs :: (Pos.SourcePos, Maybe ID.ScopedName) -> (TP.TypeDictionary , TP.ConstDictionary)-> State.StateM [Slim.MethodRef]
getDerivedMethodRefs (_,Nothing)  _          = return []
getDerivedMethodRefs (sp,(Just sn))  palds       = do
   (ID.InterfaceDcl _ False False _ diface decs) <- State.lookupDef (sp, sn)
   dmethods <- getDerivedMethodRefs diface palds
   let
      methods = filter isOperationDcl decs
   mymethods <- mapM (getMethodRef palds) methods
   return $ dmethods ++ mymethods

getDerivedMethodStrings :: (Pos.SourcePos, Maybe ID.ScopedName) -> State.StateM [Slim.MethodStrings]
getDerivedMethodStrings (_,Nothing)            = return []
getDerivedMethodStrings (sp,(Just sn))         = do
   (ID.InterfaceDcl _ False False _ diface decs) <- State.lookupDef (sp, sn)
   dmethods <- getDerivedMethodStrings diface
   let
      methods = filter isOperationDcl decs
   mymethods <- mapM SN.getMethodStrings methods
   return $ dmethods ++ mymethods


getMethodRef :: (TP.TypeDictionary , TP.ConstDictionary) -> ID.Declaration -> State.StateM Slim.MethodRef
getMethodRef (tp,_) (ID.Declaration _ _ _ (ID.Identifier _) (ID.OperationDcl isasync attr _ _ params)) = do
   let
         isMode mms (ID.Parameter _ pm  _) = pm `elem` mms
         toType (ID.Parameter _ _  ty) = ty


         max8 :: Word8 -> Word8 -> Word8
         max8 a b = a `max` b

         computePrimIn (primIn,ial) (sz,al) =
               (SA.alignAndAdd primIn (sz, al),   (al `max8` ial))
         computePrimROut (primIn,ial) (primROut,ral)  (Slim.SizeROut (risz, rial) (rrsz,rral)) =
               ( (SA.alignAndAdd primIn (risz,rial), ial `max8` rial)
               , (SA.alignAndAdd primROut (rrsz,rral), ral `max8` rral)
               )
         compute (primIn, primROut) (par, ws)
            | isMode [ID.ParameterIn] par   = ( computePrimIn primIn (Slim.asIn ws)
                                              , primROut
                                              )
            | isMode [ID.ParameterROut] par = computePrimROut primIn primROut (Slim.asROut ws)
            | isMode [ID.ParameterInROut] par =
               let   primIn' = computePrimIn primIn (Slim.asIn ws)
               in    computePrimROut primIn' primROut (Slim.asROut ws)
            | otherwise                        = error "internal error: unsupported mode"

   _ <- swapScalars (Slim.Scalars 0 0 0 0)
   pars <- mapM (getParameter tp isasync) params
   sc <- liftM Slim.curScalars $ State.get
   wss <- mapM SA.fromType $ (map toType params)
   let
         parar = Slim.ParameterRefArrayRef (Ref.Val pars)
         (primIn,primROut) = List.foldl' compute ((0,0::Word8),(0,0::Word8)) (zip params wss)
         withPrim osc
            | (fst primIn) > 0 && (fst primROut) > 0 = osc `mergeScalars` (Slim.Scalars 1 1 0 0 )
            | (fst primIn) > 0 = osc `mergeScalars` (Slim.Scalars 1 0 0 0 )
            | (fst primROut) > 0 = osc `mergeScalars` (Slim.Scalars 0 1 0 0 )
            | otherwise = osc
         sc' = withPrim sc
   nArgs <- liftM sum $ mapM numArgs params
   return $ Slim.MethodRef (Ref.Val (Slim.Method attr sc' primIn primROut nArgs (fromIntegral $ length pars) parar))

getMethodRef _ _ = error "internal error: pattern match not an operation"

numArgs:: ID.Parameter -> State.StateM Int32
numArgs (ID.Parameter _ md ty) = numArgsFromType md ty

numArgsFromDef :: ID.ParameterMode -> ID.Definition -> State.State Slim.State Int32
numArgsFromDef _  (ID.InterfaceDcl _ _ _ _ _ _)          = return 1
numArgsFromDef md (ID.TypeDcl ty)                      = numArgsFromType md ty
numArgsFromDef _ _                                     = error "internal error: pattern match numArgs"

numArgsFromType :: ID.ParameterMode -> ID.Type -> State.State Slim.State Int32
numArgsFromType md              (ID.TypeRef sp _ sn)     = State.lookupDef (sp,sn) >>= numArgsFromDef md
numArgsFromType ID.ParameterIn  (ID.StringType {})       = return 1
numArgsFromType ID.ParameterIn  (ID.WideStringType {})   = return 1
numArgsFromType ID.ParameterIn  (ID.DmahandleType {})    = return 3
numArgsFromType ID.ParameterIn  (ID.Sequence {})         = return 2
numArgsFromType ID.ParameterIn  ty@(ID.PrimType {})      = do
   ws <- SA.fromType ty
   let   num
            | (sznAl $ Slim.asNative ws) == (8,8) = 3
            | otherwise                           = 1
   return $ num

numArgsFromType _               (ID.Interface )          = return 2
numArgsFromType _               (ID.Sequence {})         = return 3
numArgsFromType _               (ID.StringType {})       = return 3
numArgsFromType _               (ID.WideStringType {})   = return 3
numArgsFromType _               (ID.DmahandleType {})    = return 3
numArgsFromType _               _                        = return 1



getParameter :: TP.TypeDictionary -> Bool -> ID.Parameter -> State.StateM Slim.ParameterRef
getParameter tp async (ID.Parameter (ID.Identifier _) md ty@(ID.TypeRef _ notnil _)) = do
   st <- State.get
   State.put $ st { Slim.mode = md }
   ty' <- getType ty  Nothing (Just tp) async
   return $ Slim.ParameterRef $ Ref.Val $ Slim.Parameter ty' md notnil

getParameter tp async (ID.Parameter _ md  ty) = do
   st <- State.get
   State.put $ st { Slim.mode = md }
   ty' <- getType ty  Nothing (Just tp) async
   return $ Slim.ParameterRef $ Ref.Val $ Slim.Parameter ty' md False

getIdentify :: (ID.ScopedName, TK.Type) -> State.StateM String
getIdentify (nm,typ) = do
       case typ of
                (TK.PrimType p)-> do
                   case (length nm) of
                          1 -> return (head nm)
                          _ -> return ""
                (TK.Struct _ _)-> do
                   case (length nm) of
                          1 -> return (head nm)
                          _ -> return ""
                (TK.Sequence _ _ _)-> do
                   case (length nm) of
                          1 -> return (head nm)
                          _ -> return ""
                _ -> return ""



objsIn ::State.StateM ()
objsIn = do
   st <- State.get
   let sc = Slim.curScalars st
   State.put $ st { Slim.curScalars = sc { Slim.objsIn = 1 + (Slim.objsIn sc) } }

objsROut ::State.StateM ()
objsROut = do
   st <- State.get
   let sc = Slim.curScalars st
   State.put $ st { Slim.curScalars = sc { Slim.objsROut = 1 + (Slim.objsROut sc) } }

bufsIn ::State.StateM ()
bufsIn = do
   st <- State.get
   let sc = Slim.curScalars st
   State.put $ st { Slim.curScalars = sc { Slim.bufsIn = 1 + (Slim.bufsIn sc) } }

bufsROut ::State.StateM ()
bufsROut = do
   st <- State.get
   let sc = Slim.curScalars st
   State.put $ st { Slim.curScalars = sc { Slim.bufsROut = 1 + (Slim.bufsROut sc) } }


onMode :: [ID.ParameterMode] -> State.StateM () -> State.StateM ()
onMode mms rv = do
   mymode <- liftM Slim.mode $ State.get
   when (mymode `elem` mms || ID.ParameterInROut == mymode) rv

typeFromDef :: Bool -> ID.Definition -> [String] -> TP.TypeDictionary -> Bool -> State.StateM Slim.Type
typeFromDef notnil (ID.TypeDcl (ID.TypeRef sp notnil' snt)) sn tp async  = do
   def <- State.lookupDef (sp,snt)
   typeFromDef (notnil || notnil') def sn tp async

typeFromDef notnil ty@(ID.TypeDcl (ID.Interface))  _ _   _         = do
   mysize <- liftM Slim.asNative $ SA.fromDef ty
   onMode [ID.ParameterIn] $ objsIn
   onMode [ID.ParameterROut] $ objsROut
   return $ Slim.Type (Slim.TypeInterface notnil) mysize

typeFromDef notnil ty@(ID.InterfaceDcl _ _ _ iid _ _) _ _  _ = do
   let
         from (Just (ID.ConstExpr _ (ID.ConstExprRef ["AEEIID_remote_handle64"]))) = Slim.iid_remote_handle
         from (Just _) = error $ "invalid object" ++ (show ty)
         from Nothing = error $ "invalid object" ++ (show ty)
   mysize <- liftM Slim.asNative $ SA.fromDef ty
   onMode [ID.ParameterIn] $ objsIn
   onMode [ID.ParameterROut] $ objsROut
   let iid' = from iid
   return $ Slim.Type (Slim.TypeObject iid' notnil) mysize

typeFromDef _ (ID.TypeDcl ty) [nm] tp  async                        = getType ty (Just nm) (Just tp) async
typeFromDef _ _    _        _       _                           = error "internal error: pattern match 3"

getType :: ID.Type -> Maybe String -> Maybe TP.TypeDictionary -> Bool ->  State.StateM Slim.Type
getType (ID.TypeRef sp notnil sn)  _ (Just tp) async      = do
   def <- State.lookupDef (sp,sn)
   typeFromDef notnil def sn tp async



getType ty@(ID.Struct name mms)  (Just nm)   (Just tp)    async        = do
   ws <- SA.fromType ty
   os <- swapScalars (Slim.Scalars 0 0 0 0)
   nsl <- mapM getIdentify tp
   let nme = (nm `elem` nsl)
   (typeRefs, _) <- liftM List.unzip $ mapM (fromMem tp) mms
   ns <- liftM Slim.curScalars $ State.get
   _ <- swapScalars $ mergeScalars os ns
   let
         ntrf = Slim.TypeRefArrayRef (Ref.Val typeRefs)
         ref = Slim.StructTypeRef (Ref.Val $ Slim.StructType (fromIntegral $ length mms) ntrf (Slim.asIn ws) (Slim.asROut ws))
         structType
            | (isComplex ns)  = Slim.TypeComplexStructure ref nm nme
            | otherwise       = Slim.TypeStructure ref nm nme
   st <- State.get
   let md = Slim.mode st
   case md of
         ID.ParameterROut -> do
           case async of
               True -> error "internal error: primType: rout struct not supported for async"
               False -> return $ Slim.Type structType (Slim.asNative ws)
         ID.ParameterInROut -> do
           case async of
               True -> error "internal error: primType: inrout struct not supported for async"
               False -> return $ Slim.Type structType (Slim.asNative ws)
         _ -> return $ Slim.Type structType (Slim.asNative ws)

getType ty@(ID.Union _ nm desc cases defcase)  _  (Just tp)  async  = do
   mysize <- SA.fromType ty
   desc' <- getType desc Nothing (Just tp) False
   os <- swapScalars (Slim.Scalars 0 0 0 0)
   (typeRefs, caseValues, cwss, bDefaultCase) <- fromCases defcase cases tp
   ns <- liftM Slim.curScalars $ State.get
   _ <- swapScalars $ mergeScalars os ns
   ds <- SA.fromType desc
   let
         ncv =  mkCaseValues (sznSz $ Slim.asNative ds) caseValues
         ntrf = Slim.TypeRefArrayRef $ Ref.Val typeRefs
         descRef = Slim.TypeRef (Ref.Val desc')
         incain = maximum $ map (snd . Slim.asIn) cwss
         routcain = maximum $ map (snd . Slim.primIn . Slim.asROut) cwss
         routcarout = maximum $ map (snd . Slim.primROut . Slim.asROut) cwss
         nativeca32 = maximum $ map (fst . sznAl . Slim.asNative) cwss
         nativeca64 = maximum $ map (snd . sznAl . Slim.asNative) cwss
         caseAligns = (incain, routcain, routcarout, (nativeca32, nativeca64))
         ref = Slim.UnionTypeRef $ Ref.Val $ Slim.UnionType descRef (fromIntegral $ length typeRefs) ncv ntrf (Slim.asIn mysize) (Slim.asROut mysize) caseAligns bDefaultCase
         unionType
            | (isComplex ns)  = Slim.TypeComplexUnion ref
            | otherwise       = Slim.TypeUnion ref nm
   st <- State.get
   let md = Slim.mode st
   case md of
         ID.ParameterROut -> do
           case async of
               True -> error "internal error: primType: rout union not supported for async"
               False -> return $ Slim.Type unionType (Slim.asNative mysize)
         ID.ParameterInROut -> do
           case async of
               True -> error "internal error: primType: inrout union not supported for async"
               False -> return $ Slim.Type unionType (Slim.asNative mysize)
         _ -> return $ Slim.Type unionType (Slim.asNative mysize)

getType ty@(ID.Enum name _)  (Just nm)     (Just tp)    async                             = do
   ws <- SA.fromType ty
   ns <- mapM getIdentify tp
   let nme = (nm `elem` ns)
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout enum not supported for async"
               False -> return $ Slim.Type  (Slim.TypeEnum nm nme) (Slim.asNative ws)
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: primType: inrout enum not supported for async"
               False -> return $ Slim.Type  (Slim.TypeEnum nm nme) (Slim.asNative ws)
        _ -> return $ Slim.Type  (Slim.TypeEnum nm nme) (Slim.asNative ws)

getType ty@(ID.Sequence ml sty)     _     (Just tp)    _                 = do
   mysize <- liftM Slim.asNative $ SA.fromType ty
   ws <- SA.fromType sty
   when ((fst (Slim.asIn ws)) > 0) $ onMode [ID.ParameterIn] $ bufsIn
   let
         routInSize = fst $ Slim.primIn (Slim.asROut ws)
         routROutSize = fst $ Slim.primROut (Slim.asROut ws)
   when (routInSize > 0 || routROutSize > 0) $ onMode [ID.ParameterROut] $ bufsROut
   os <- swapScalars (Slim.Scalars 0 0 0 0)
   nt <- getType sty Nothing (Just tp) False
   ns <- liftM Slim.curScalars $ State.get
   _ <- swapScalars $ mergeSequenceScalars os ns
   ml' <- State.constExprToInt32 ml
   nsp <- mapM getIdentify tp
   let
         ntr = Slim.TypeRef $ Ref.Val nt
         idr
            | (isStruct $ getTypeDesc nt) = getIdentifier $ getTypeDesc nt
            | (isSeq $ getTypeDesc nt) = getIdentifierseq $ getTypeDesc nt
            | otherwise     =  ""
         km
            | isTypeRef sty = getTypename sty
            | otherwise = "--"
         lm = (notEmpty idr km)
         dm = lm `elem` nsp

         st  = Slim.SequenceType ntr ml' (Slim.asIn ws) (Slim.asROut ws)
         str = Slim.SequenceTypeRef $ Ref.Val st
         sq
            | (isComplex ns) = Slim.TypeComplexSequence str idr dm km
            | otherwise      = Slim.TypeSequence ntr ml' idr dm km
   return $ Slim.Type sq mysize


getType ty@(ID.StringType ml)     (Just nm)    (Just tp)  async                         = do
   mysize <- liftM Slim.asNative $ SA.fromType ty
   onMode [ID.ParameterIn] $ bufsIn
   onMode [ID.ParameterROut] $ bufsROut
   ns <- mapM getIdentify tp
   let nme = (nm `elem` ns)
   ml' <- State.constExprToInt32 ml
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout string not supported for async"
               False -> return $ Slim.Type (Slim.TypeString ml' nm nme) mysize
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: primType: inrout string not supported for async"
               False -> return $ Slim.Type (Slim.TypeString ml' nm nme) mysize
        _ -> return $ Slim.Type (Slim.TypeString ml' nm nme) mysize


getType ty@(ID.StringType ml)     (Nothing)    (Just tp)  async                         = do
   mysize <- liftM Slim.asNative $ SA.fromType ty
   onMode [ID.ParameterIn] $ bufsIn
   onMode [ID.ParameterROut] $ bufsROut
   ml' <- State.constExprToInt32 ml
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout string not supported for async"
               False -> return $ Slim.Type (Slim.TypeString ml' "" False) mysize
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: primType: inrout string not supported for async"
               False -> return $ Slim.Type (Slim.TypeString ml' "" False) mysize
        _ -> return $ Slim.Type (Slim.TypeString ml' "" False) mysize

getType ty@(ID.WideStringType ml)     (Just nm)    (Just tp)  async                   = do
   mysize <- liftM Slim.asNative $ SA.fromType ty
   onMode [ID.ParameterIn] $ bufsIn
   onMode [ID.ParameterROut] $ bufsROut
   ns <- mapM getIdentify tp
   let nme = (nm `elem` ns)
   ml' <- State.constExprToInt32 ml
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout wide string not supported for async"
               False -> return $ Slim.Type (Slim.TypeWString ml' nm nme) mysize
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: primType: inrout wide string not supported for async"
               False -> return $ Slim.Type (Slim.TypeWString ml' nm nme) mysize
        _ -> return $ Slim.Type (Slim.TypeWString ml' nm nme) mysize

getType ty@(ID.WideStringType ml)     (Nothing)    (Just tp)  async                   = do
   mysize <- liftM Slim.asNative $ SA.fromType ty
   onMode [ID.ParameterIn] $ bufsIn
   onMode [ID.ParameterROut] $ bufsROut
   ml' <- State.constExprToInt32 ml
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout wide string not supported for async"
               False -> return $ Slim.Type (Slim.TypeWString ml' "" False) mysize
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: primType: inrout wide string not supported for async"
               False -> return $ Slim.Type (Slim.TypeWString ml' "" False) mysize
        _ -> return $ Slim.Type (Slim.TypeWString ml' "" False) mysize

getType ty@(ID.DmahandleType ml)      _      _         async           = do
   mysize <- liftM Slim.asNative $ SA.fromType ty
   onMode [ID.ParameterIn] $ objsIn
   onMode [ID.ParameterROut] $ objsROut
   ml' <- State.constExprToInt32 ml
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout dmahandle not supported for async"
               False -> return $ Slim.Type (Slim.TypeDmahandle ml') mysize
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: primType: inrout dmahandle not supported for async"
               False -> return $ Slim.Type (Slim.TypeDmahandle ml') mysize
        _ -> return $ Slim.Type (Slim.TypeDmahandle ml') mysize

getType ty@(ID.Array ml tp)    _      (Just typ)      async                   = do
   mysize <- liftM Slim.asNative $ SA.fromType ty
   os <- swapScalars (Slim.Scalars 0 0 0 0)
   nt <- getType tp Nothing (Just typ) False
   ns <- liftM Slim.curScalars $ State.get
   ml' <- State.constExprToInt32 ml
   _ <- swapScalars $ mergeArrayScalars (fromIntegral ml') os ns
   nsp <- mapM getIdentify typ
   let
         ntr = Slim.TypeRef $ Ref.Val nt
         idr
            | (isStruct $ getTypeDesc nt) = getIdentifier $ getTypeDesc nt
            | otherwise  = ""
         dm
            | (isStruct $ getTypeDesc nt) && (idr `elem` nsp) = True
            | (isStruct $ getTypeDesc nt) && not (idr `elem` nsp)     =  False
            | otherwise = True
         arr
            | (isComplex ns) = Slim.TypeComplexArray ntr ml' idr
            | otherwise      = Slim.TypeArray ntr ml' idr dm
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout array not supported for async"
               False -> return $ Slim.Type arr mysize
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: primType: inrout array not supported for async"
               False -> return $ Slim.Type arr mysize
        _ -> return $ Slim.Type arr mysize

getType ty@(ID.Interface)    _   _       async                         = do
   mysize <- liftM Slim.asNative $ SA.fromType ty
   onMode [ID.ParameterIn] $ objsIn
   onMode [ID.ParameterROut] $ objsROut
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: wrong interface declaration for async rout"
               False -> return $ Slim.Type (Slim.TypeInterface False) mysize
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: wrong interface declaration for async inrout"
               False -> return $ Slim.Type (Slim.TypeInterface False) mysize
        _ -> return $ Slim.Type (Slim.TypeInterface False) mysize

getType ty@(ID.PrimType tp) (Just nm)   (Just typ)     async                           = do
   ws <- SA.fromType ty
   ns <- mapM getIdentify typ
   let nme = (nm `elem` ns)
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout primitive type not supported for async"
               False -> getPrim tp (Just nm) nme ws
        ID.ParameterInROut -> do
            case async of
               True -> error "internal error: primType: inrout primitive type not supported for async"
               False -> getPrim tp (Just nm) nme ws
        otherwise -> getPrim tp (Just nm) nme ws

getType ty@(ID.PrimType tp) Nothing _     async                            = do
   ws <- SA.fromType ty
   st <- State.get
   let md = Slim.mode st
   case md of
        ID.ParameterROut -> do
            case async of
               True -> error "internal error: primType: rout primitive type not supported for async"
               False -> getPrim tp Nothing False ws
        ID.ParameterInROut -> do
           case async of
               True -> error "internal error: primType: inrout primitive type not supported for async"
               False -> getPrim tp Nothing False ws
        otherwise -> getPrim tp Nothing False ws


getType (ID.Native _)   _       _     _                           = error "internal error: pattern match 4"

getPrim :: ID.Prim -> Maybe String -> Bool -> Slim.SizeAlign -> State.StateM Slim.Type
getPrim tp (Just nm) nme ws = do
                case tp of
                    OP.SignedShortType      ->   return $ Slim.Type (Slim.SignedShortType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedLongType       -> return $ Slim.Type (Slim.SignedLongType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedLongLongType   -> return $ Slim.Type (Slim.SignedLongLongType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedCharFixedTType        -> return $ Slim.Type (Slim.SignedCharFixedTType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedShortFixedTType       -> return $ Slim.Type (Slim.SignedShortFixedTType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedLongFixedTType       -> return $ Slim.Type (Slim.SignedLongFixedTType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedLongLongFixedTType       -> return $ Slim.Type (Slim.SignedLongLongFixedTType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedCharFixedType       -> return $ Slim.Type (Slim.SignedCharFixedType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedShortFixedType       -> return $ Slim.Type (Slim.SignedShortFixedType (Just nm) nme) (Slim.asNative ws)
                    OP.SignedLongFixedType       -> return $ Slim.Type (Slim.SignedLongFixedType (Just nm) nme)   (Slim.asNative ws)
                    OP.SignedLongLongFixedType       -> return $ Slim.Type (Slim.SignedLongLongFixedType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedShortType      ->   return $ Slim.Type (Slim.UnsignedShortType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedLongType       -> return $ Slim.Type (Slim.UnsignedLongType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedLongLongType   -> return $ Slim.Type (Slim.UnsignedLongLongType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedCharFixedTType        -> return $ Slim.Type (Slim.UnsignedCharFixedTType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedShortFixedTType       -> return $ Slim.Type (Slim.UnsignedShortFixedTType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedLongFixedTType       -> return $ Slim.Type (Slim.UnsignedLongFixedTType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedLongLongFixedTType       -> return $ Slim.Type (Slim.UnsignedLongLongFixedTType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedCharFixedType       -> return $ Slim.Type (Slim.UnsignedCharFixedType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedShortFixedType       -> return $ Slim.Type (Slim.UnsignedShortFixedType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedLongFixedType       -> return $ Slim.Type (Slim.UnsignedLongFixedType (Just nm) nme) (Slim.asNative ws)
                    OP.UnsignedLongLongFixedType       -> return $ Slim.Type (Slim.UnsignedLongLongFixedType (Just nm) nme) (Slim.asNative ws)
                    OP.BooleanType                   -> return $ Slim.Type (Slim.BooleanType (Just nm) nme)(Slim.asNative ws)
                    OP.FloatType                   -> return $ Slim.Type (Slim.FloatType (Just nm) nme) (Slim.asNative ws)
                    OP.DoubleType                   -> return $ Slim.Type (Slim.DoubleType (Just nm) nme) (Slim.asNative ws)
                    OP.OctetType                   -> return $ Slim.Type (Slim.OctetType (Just nm) nme) (Slim.asNative ws)
                    OP.CharType                   -> return $ Slim.Type (Slim.CharType (Just nm) nme) (Slim.asNative ws)
                    OP.WideCharType                   -> return $ Slim.Type (Slim.WideCharType (Just nm) nme) (Slim.asNative ws)
                    OP.EnumType _                   -> error "internal error: primType: unexpected"
                    _ ->  error "internal error: primType: unexpected"

getPrim tp Nothing _ ws = do
                case tp of
                     OP.SignedShortType      ->   return $ Slim.Type (Slim.SignedShortType Nothing False) (Slim.asNative ws)
                     OP.SignedLongType       -> return $ Slim.Type (Slim.SignedLongType Nothing False) (Slim.asNative ws)
                     OP.SignedLongLongType   -> return $ Slim.Type (Slim.SignedLongLongType Nothing False) (Slim.asNative ws)
                     OP.SignedCharFixedTType        -> return $ Slim.Type (Slim.SignedCharFixedTType Nothing False) (Slim.asNative ws)
                     OP.SignedShortFixedTType       -> return $ Slim.Type (Slim.SignedShortFixedTType Nothing False) (Slim.asNative ws)
                     OP.SignedLongFixedTType       -> return $ Slim.Type (Slim.SignedLongFixedTType Nothing False) (Slim.asNative ws)
                     OP.SignedLongLongFixedTType       -> return $ Slim.Type (Slim.SignedLongLongFixedTType Nothing False) (Slim.asNative ws)
                     OP.SignedCharFixedType       -> return $ Slim.Type (Slim.SignedCharFixedType Nothing False) (Slim.asNative ws)
                     OP.SignedShortFixedType       -> return $ Slim.Type (Slim.SignedShortFixedType Nothing False) (Slim.asNative ws)
                     OP.SignedLongFixedType       -> return $ Slim.Type (Slim.SignedLongFixedType Nothing False)   (Slim.asNative ws)
                     OP.SignedLongLongFixedType       -> return $ Slim.Type (Slim.SignedLongLongFixedType Nothing False) (Slim.asNative ws)
                     OP.UnsignedShortType      ->   return $ Slim.Type (Slim.UnsignedShortType Nothing False) (Slim.asNative ws)
                     OP.UnsignedLongType       -> return $ Slim.Type (Slim.UnsignedLongType Nothing False) (Slim.asNative ws)
                     OP.UnsignedLongLongType   -> return $ Slim.Type (Slim.UnsignedLongLongType Nothing False) (Slim.asNative ws)
                     OP.UnsignedCharFixedTType        -> return $ Slim.Type (Slim.UnsignedCharFixedTType Nothing False) (Slim.asNative ws)
                     OP.UnsignedShortFixedTType       -> return $ Slim.Type (Slim.UnsignedShortFixedTType Nothing False) (Slim.asNative ws)
                     OP.UnsignedLongFixedTType       -> return $ Slim.Type (Slim.UnsignedLongFixedTType Nothing False) (Slim.asNative ws)
                     OP.UnsignedLongLongFixedTType       -> return $ Slim.Type (Slim.UnsignedLongLongFixedTType Nothing False) (Slim.asNative ws)
                     OP.UnsignedCharFixedType       -> return $ Slim.Type (Slim.UnsignedCharFixedType Nothing False) (Slim.asNative ws)
                     OP.UnsignedShortFixedType       -> return $ Slim.Type (Slim.UnsignedShortFixedType Nothing False) (Slim.asNative ws)
                     OP.UnsignedLongFixedType       -> return $ Slim.Type (Slim.UnsignedLongFixedType Nothing False) (Slim.asNative ws)
                     OP.UnsignedLongLongFixedType       -> return $ Slim.Type (Slim.UnsignedLongLongFixedType Nothing False) (Slim.asNative ws)
                     OP.BooleanType                   -> return $ Slim.Type (Slim.BooleanType Nothing False)(Slim.asNative ws)
                     OP.FloatType                   -> return $ Slim.Type (Slim.FloatType Nothing False) (Slim.asNative ws)
                     OP.DoubleType                   -> return $ Slim.Type (Slim.DoubleType Nothing False) (Slim.asNative ws)
                     OP.OctetType                   -> return $ Slim.Type (Slim.OctetType Nothing False) (Slim.asNative ws)
                     OP.CharType                   -> return $ Slim.Type (Slim.CharType Nothing False) (Slim.asNative ws)
                     OP.WideCharType                   -> return $ Slim.Type (Slim.WideCharType Nothing False) (Slim.asNative ws)
                     OP.EnumType _                   -> error "internal error: primType: unexpected enum"
                     _ ->  error "internal error: primType: unexpected"


mkCaseValues :: Integral n => (Int32,Int32) -> [n] -> Slim.CaseValueArrayRef
mkCaseValues sz caseValues = do
   case (sz) of
      (1,1) -> Slim.CaseValue8ArrayRef  $ Ref.Val (map fromIntegral caseValues)
      (2,2) -> Slim.CaseValue16ArrayRef $ Ref.Val (map fromIntegral caseValues)
      (4,4) -> Slim.CaseValue32ArrayRef $ Ref.Val (map fromIntegral caseValues)
      (8,8) -> Slim.CaseValue64ArrayRef $ Ref.Val (map fromIntegral caseValues)
      _ -> error "internal error: unexpected union descriptor size, expected 1, 2, 4, or 8"

fromMem :: TP.TypeDictionary ->(ID.Member)  -> State.StateM (Slim.TypeRef, Slim.SizeAlign)
fromMem typ (ID.Member _ _ (ID.Identifier _) tp)  = do
   nt <- getType tp Nothing (Just typ) False
   let ntr = Slim.TypeRef $ Ref.Val nt
   ws <- SA.fromType tp
   return (ntr, ws)

getTypeDesc :: Slim.Type -> Slim.TypeDesc
getTypeDesc (Slim.Type ts _) = ts

getTypename :: ID.Type -> String
getTypename sty@(ID.TypeRef _ _ nm) = head nm

notEmpty :: String -> String -> String
notEmpty idr "--" = idr
notEmpty _ km = km

isTypeRef :: ID.Type -> Bool
isTypeRef sty@(ID.TypeRef _ _ nm) = True
isTypeRef _ = False

isStruct :: Slim.TypeDesc -> Bool
isStruct ts@(Slim.TypeComplexStructure _ _ _) = True
isStruct ts@(Slim.TypeStructure _ _ _) = True
isStruct _ = False

isSeq :: Slim.TypeDesc -> Bool
isSeq ts@(Slim.TypeComplexSequence _ _ _ _) = True
isSeq ts@(Slim.TypeSequence _ _ _ _ _) = True
isSeq _ = False

getIdentifier :: Slim.TypeDesc -> String
getIdentifier (Slim.TypeComplexStructure ref nm _) = nm
getIdentifier (Slim.TypeStructure ref nm _) = nm

getIdentifierseq :: Slim.TypeDesc -> String
getIdentifierseq (Slim.TypeComplexSequence str idr dm km) = km
getIdentifierseq (Slim.TypeSequence _ str idr dm km) = km

fromCases :: (Maybe ID.DefaultCase) -> [(ID.UnionCase)] -> TP.TypeDictionary -> State.StateM ([Slim.TypeRef],  [Word32], [Slim.SizeAlign], Bool)
fromCases (Just dc) cases  tp = do
   (dtr, dwsa) <- fromMem tp dc
   (typeRefs, caseVals, csa) <- liftM List.unzip3 $ mapM (fromCase tp) cases
   return (typeRefs ++ [dtr], caseVals, csa ++ [dwsa], True)

fromCases Nothing cases  tp = do
   (typeRefs, caseVals, csa) <- liftM List.unzip3 $ mapM (fromCase tp) cases
   return (typeRefs, caseVals, csa, False)

fromCase :: TP.TypeDictionary -> (ID.UnionCase) -> State.StateM (Slim.TypeRef, Word32, Slim.SizeAlign)
fromCase  tp (ID.UnionCase [cs] mm) = do
   (tr,  sa) <- fromMem tp mm
   wr <- State.constLookupWord32 cs
   return (tr, wr, sa)

fromCase _ (ID.UnionCase _ _) = error "internal error:fromCase"

isOperationDcl :: ID.Declaration -> Bool
isOperationDcl (ID.Declaration _ _ _ _ (ID.OperationDcl {})) = True
isOperationDcl _ = False

isComplex :: Slim.Scalars -> Bool
isComplex (Slim.Scalars abi abo aoi aoo) = (abi + abo + aoi + aoo > 0)


mergeScalars :: Slim.Scalars -> Slim.Scalars -> Slim.Scalars
mergeScalars (Slim.Scalars abi abo aoi aoo)  (Slim.Scalars bbi bbo boi boo) = (Slim.Scalars (abi `ab` bbi) (abo `ab` bbo) (aoi `ao` boi) (aoo `ao` boo))
   where
      ab 0xfff _ = 0xfff
      ab _ 0xfff = 0xff
      ab aa bb = aa + bb

      ao _ 0x0f = 0x0f
      ao 0x0f _ = 0x0f
      ao aa bb = aa + bb

mergeArrayScalars :: Int -> Slim.Scalars -> Slim.Scalars -> Slim.Scalars
mergeArrayScalars sz osc nsc = List.foldl' mergeScalars osc (take sz $ repeat nsc)

mergeSequenceScalars :: Slim.Scalars -> Slim.Scalars -> Slim.Scalars
mergeSequenceScalars (Slim.Scalars abi abo aoi aoo)  (Slim.Scalars bbi bbo boi boo) = (Slim.Scalars (abi `ab` bbi) (abo `cb` bbo) (aoi `ao` boi) (aoo `ao` boo))
   where
      ab 0 0 = isRout abo bbo
      ab aa 0 = aa
      ab _ _ = 0xff
      cb 0 0 = isIn abi bbi
      cb aa 0 = aa
      cb _ _ = 0xff
      ao aa 0 = aa
      ao _ _ = 0x0f

swapScalars :: Slim.Scalars -> State.StateM (Slim.Scalars)
swapScalars ns = do
   st <- State.get
   State.put $ st { Slim.curScalars = ns }
   return (Slim.curScalars st)

isRout :: Word8->Word8->Word8
isRout 0 0 = 0x01
isRout _ _ = 0

isIn :: Word8->Word8->Word8
isIn 0 0 = 0x01
isIn _ _ = 0



-- Removing as part of QAIC Source Code Cleanup

-- isList :: ID.ScopedName -> State.StateM Bool
-- isList nm = do
--        case (length nm) of
--                1 -> return True
--                _ -> return False


-- literalToCaseValue :: ID.Type -> [L.Literal] -> State.StateM Int
-- literalToCaseValue _ [ll@(L.IntLiteral _ _)] = return $ TD.literalToIntegral ll
-- literalToCaseValue (ID.TypeRef sp _ sn) lls = do
--    (ID.TypeDcl ty) <- State.lookupDef (sp,sn)
--    literalToCaseValue ty lls

-- literalToCaseValue (ID.Enum  _ labels) lls = do
--    let
--          literalToString (L.StringLiteral str) = str
--          literalToString _                     = error "internal error: unexpected literal"
--    return $ fromJust $ (concatMap literalToString lls)  `List.elemIndex` (snd $ unzip labels)

-- literalToCaseValue _ _ = error "internal error: unexpected union case"
