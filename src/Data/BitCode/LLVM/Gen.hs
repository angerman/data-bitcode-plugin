{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RecursiveDo, LambdaCase, FlexibleInstances, FlexibleContexts #-}
module Data.BitCode.LLVM.Gen where

import qualified Data.BitCode.LLVM.Gen.Monad as Llvm
import qualified EDSL.Monad as EDSL
import qualified EDSL as EDSL
import qualified Data.BitCode.LLVM.Classes.HasType as EDSL (ty)
import qualified Data.BitCode.LLVM.Util as EDSL
import EDSL ((-->))
import Data.BitCode.LLVM.Gen.Monad (LlvmT, runLlvmT, LlvmEnv(..))
import EDSL.Monad (BodyBuilderT, execBodyBuilderT)
import Data.BitCode.LLVM.Pretty (pretty)
import Text.PrettyPrint
import qualified Data.BitCode.LLVM.Function        as Func
-- import Data.BitCode.LLVM (Ident(..))
-- import Data.BitCode.LLVM.Codes.Identification (Epoch(Current))

import CgUtils ( fixStgRegisters )
import PprCmm
import ErrUtils
import Outputable (panic)
import CmmUtils
import Hoopl

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (runExceptT, ExceptT, throwE)


import Outputable as Outp hiding ((<+>), text, ($+$), int)

-- for outputfn
import Stream           (Stream)
import Module
import DynFlags
-- for phaseInputExt
import DriverPhases (Phase(..))

import qualified Stream
-- debugging
import Debug.Trace
import Cmm
import Cmm.Pretty
import Data.Maybe (catMaybes)

import CLabel
import Platform

import CodeGen.Platform ( activeStgRegs, callerSaves )

import ForeignCall

-- body builder
import Data.BitCode.LLVM.Instruction (Inst)
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Types (BasicBlockId)

import qualified Data.BitCode.LLVM.Type            as Ty
import Data.Maybe (fromMaybe)

import Data.List (nub, sort)
import Data.Either (lefts, rights)
import Control.Monad
import Control.Monad.Fix (MonadFix(..))

--------------------------------------------------------------------------------
-- * Types
-- | Global registers live on proc entry
type LiveGlobalRegs = [GlobalReg]


--------------------------------------------------------------------------------
-- * Llvm Monad
newtype LlvmM a = LlvmM { unLlvmM :: LlvmT IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance HasDynFlags LlvmM where
  getDynFlags = LlvmM $ getDynFlags

runLlvm :: DynFlags -> FilePath -> LlvmM [Either Symbol Func.Function] -> IO ()
runLlvm dflags fp m = do
  putStrLn $ "File: " ++ fp
  decls <- flip evalStateT env (runLlvmT (unLlvmM m))
  let mod = EDSL.mod' "anon" (lefts decls) (rights decls)
  -- stop here for now!
--  error . show $ pretty mod

  -- TODO: FLAGS: if -drump-ast
  -- liftIO . putStrLn $ show (pretty mod)

  EDSL.writeModule fp mod
  putStrLn $ "Wrote " ++ fp
  return ()
  where env = LlvmEnv { envDynFlags = dflags }

type BodyBuilder a = BodyBuilderT LlvmM a
type Edsl a = EDSL.EdslT LlvmM a

instance HasDynFlags (BodyBuilderT LlvmM) where
  getDynFlags = lift getDynFlags

-- instance HasDynFlags (ExceptT e (BodyBuilderT LlvmM)) where
--  getDynFlags = lift getDynFlags

--------------------------------------------------------------------------------
-- * Lifted functions
getDynFlag :: (DynFlags -> a) -> LlvmM a
getDynFlag = LlvmM . Llvm.getDynFlag
dumpIfSetLlvm :: DumpFlag -> String -> Outp.SDoc -> LlvmM ()
dumpIfSetLlvm f s = LlvmM . Llvm.dumpIfSetLlvm f s

-- | Get the platform we are generating code for
getLlvmPlatform :: LlvmM Platform
getLlvmPlatform = getDynFlag targetPlatform

--------------------------------------------------------------------------------
-- * Cmm Helper
showCmm cmm = (\dflags -> showSDoc dflags (ppr cmm)) <$> getDynFlags

--------------------------------------------------------------------------------
-- Plugin Hook
outputFn :: (DynFlags -> Module -> ModLocation -> FilePath -> Stream IO RawCmmGroup () -> [UnitId] -> IO ())
         -> DynFlags -> Module -> ModLocation -> FilePath -> Stream IO RawCmmGroup () -> [UnitId] -> IO ()
outputFn super dflags mod mloc fp cmm_stream pkg_deps = runLlvm dflags fp $ llvmCodeGen (Llvm.liftStream cmm_stream)

phaseInputExt :: (Phase -> String) -> Phase -> String
phaseInputExt super phase
  | phase == LlvmOpt = "bc"
  | otherwise = super phase

--------------------------------------------------------------------------------
-- Llvm Code gen

llvmCodeGen :: Stream.Stream LlvmM RawCmmGroup () -> LlvmM [Either Symbol Func.Function]
llvmCodeGen cmm_stream = do
  liftIO $ putStrLn $ "llvmCodeGen..."
  -- The cmm stream contains multiple groups.
  --
  -- each group consists of a set of data and procs.
  fns <- Stream.collect $ Stream.mapM llvmGroupGen cmm_stream
  liftIO $ putStrLn $ "done with llvmCodeGen"
  -- as we want to put all these data and procs into a single module
  -- we simply concat the result of the stream.
  return $ concat fns

-- llvmCodeGen' :: RawCmmGroup -> LlvmM ()

--------------------------------------------------------------------------------
-- * Groups
llvmGroupGen :: RawCmmGroup -> LlvmM [Either Symbol Func.Function]
llvmGroupGen = mapM llvmCodeGen'

llvmCodeGen' :: RawCmmDecl -> LlvmM (Either Symbol Func.Function)
llvmCodeGen' dat@(CmmData{}) = genLlvmData dat >>= \case
  Right d -> pure (Left d)
  Left e -> panic $ "Error generating code gen:\n" ++ e
llvmCodeGen' prc@(CmmProc{}) = do
    -- rewrite assignments to global regs
    dflags <- getDynFlag id
    fixed_cmm@(CmmProc  infos entry_lbl live graph) <- pure $ {- fixBottom $ -} -- do we still need fix bottom?
      {-# SCC "llvm_fix_regs" #-}
      fixStgRegisters dflags prc

    dumpIfSetLlvm Opt_D_dump_opt_cmm "Optimised Cmm" (pprCmmGroup [fixed_cmm])

    -- extract the proper label for this function.
    let mb_info = mapLookup (g_entry graph) infos
        funLbl = case mb_info of
                   Nothing                   -> entry_lbl
                   Just (Statics info_lbl _) -> info_lbl

    lbl <- strCLabel_llvm funLbl

--        link | externallyVisibleCLabel lbl = Visibility.Default -- External
--             | otherwise                   = Visibility.Hidden  -- Internal


    -- prefix data
    prefix <- case mb_info of
      Nothing -> return Nothing
      Just (Statics _ statics) -> do
        infoStatics <- mapM genData statics
        return $ Just $ case EDSL.struct <$> sequence infoStatics of
          Right d -> d
          Left e -> panic $ "Error while compiling prefix data:\n" ++ e

    let addPrefix = fromMaybe id (EDSL.withPrefixData <$> prefix)

    let blocks = toBlockListEntryFirstFalseFallthrough graph


    trash <- getTrashRegs
    -- let live = activeStgRegs (targetPlatform dflags)
    let getAssignedRegs :: CmmNode O O -> [CmmReg]
        getAssignedRegs (CmmAssign reg _)= [reg]
          -- Calls will trash all registers. Unfortunately, this needs them to
          -- be stack-allocated in the first place.
        getAssignedRegs (CmmUnsafeForeignCall _ rs _) = map CmmGlobal trash ++ map CmmLocal rs
        getAssignedRegs _                             = []
        getRegsBlock (_, body, _)                     = concatMap getAssignedRegs $ blockToList body
        assignedRegs = nub . sort $ concatMap (getRegsBlock . blockSplit) blocks
        localRegs  = [r | CmmLocal r  <- assignedRegs ]
        globalRegs = [r | CmmGlobal r <- assignedRegs ]
        isLive r     = r `elem` alwaysLive || r `elem` live

    -- build up the function body; signature (and function is generated below)
    -- body <- genLlvmProc (CmmProc infos entry_lbl live graph) -- this is prc with modified live
    body <- basicBlocksCodeGen live blocks


    -- TODO: FLAGS: -traceDefSigs
    -- let sig = (fnSig dflags globalRegs)
    -- show the pretty signature on definition. E.g. add `traceShow (pretty sig)` infront of (fnSig...)

    -- produce a ghc function.
    -- now run the BodyBuilder on it with the function arguments.
    -- Eventually producing an LlvmM value.
    runExceptT (EDSL.ghcdefT lbl (fnSig dflags globalRegs) (\args -> body args)) >>= \case
      Right f -> pure . Right . addPrefix $ f
      Left e -> panic $ "Error while compiling " ++ lbl ++ "\n" ++ e

llvmCodeGen' _ = panic "LlvmCodeGen': unhandled raw cmm group"

fnSig :: DynFlags -> LiveGlobalRegs -> Ty.Ty
fnSig dflags live = (llvmFunArgs dflags live) --> EDSL.void

allocaAndStoreArg arg = do
  slot <- EDSL.alloca (EDSL.ty arg) (EDSL.int32 1)
  EDSL.store slot arg
  return slot


-- TODO: Make CmmType an instance of HasType.
--       Also can we get away with a little less casting, by using isGcPtrType?
--       I'm still a big confused about the `Gc` in there though.
fromCmmType :: CmmType -> Ty.Ty
fromCmmType ty | isVecType ty   = EDSL.vec (vecLength ty) (fromCmmType (vecElemType ty))
               | isFloatType ty = floatTypeWithWidth (typeWidth ty)
               | otherwise      = intTypeWithWidth   (typeWidth ty)
  where floatTypeWithWidth W32  = EDSL.f32
        floatTypeWithWidth W64  = EDSL.f64   -- aka double
        floatTypeWithWidth W80  = EDSL.f80   -- x86 specific?
        floatTypeWithWidth W128 = EDSL.f128  -- always avilable?
        intTypeWithWidth        = EDSL.i . widthInBits

-- | Construct a floating point value
-- TODO: Maybe push Width into EDSL Types and Values?
float :: Width -> Rational -> Symbol
float W32 = EDSL.float32
float W64 = EDSL.float64
float W80 = EDSL.float80
float W128 = EDSL.float128

allocaLocalRegs (LocalReg id ty) = EDSL.alloca (fromCmmType ty) (EDSL.int32 1)

activeRegs :: DynFlags -> LiveGlobalRegs -> LiveGlobalRegs
activeRegs dflags live = filter isLive (activeStgRegs (targetPlatform dflags))
  where isLive r = not (isSSE r) || r `elem` alwaysLive || r `elem` live
        isSSE (FloatReg _)  = True
        isSSE (DoubleReg _) = True
        isSSE (XmmReg _)    = True
        isSSE (YmmReg _)    = True
        isSSE (ZmmReg _)    = True
        isSSE _             = False


-- TODO: filter out all *non* live regs. (See LlvmCodeGen/Base.hs)
llvmFunArgs :: DynFlags -> LiveGlobalRegs -> [Ty.Ty]
llvmFunArgs dflags live = map regType (activeRegs dflags live)
  where wordSize = wORD_SIZE dflags
        wordGlobal = EDSL.word wordSize
        ptrGlobal  = EDSL.ptr wordGlobal
        fltGlobal = EDSL.f32
        dblGlobal = EDSL.f64
        xmmGlobal = EDSL.vec 4 EDSL.i32
        ymmGlobal = EDSL.vec 8 EDSL.i32
        zmmGlobal = EDSL.vec 16 EDSL.i32
        regType BaseReg      = ptrGlobal
        regType Sp           = ptrGlobal
        regType Hp           = ptrGlobal
        regType VanillaReg{} = wordGlobal
        regType SpLim        = wordGlobal
        regType FloatReg{}   = fltGlobal
        regType DoubleReg{}  = dblGlobal
        regType XmmReg{}     = xmmGlobal
        regType YmmReg{}     = ymmGlobal
        regType ZmmReg{}     = zmmGlobal
        regType MachSp       = wordGlobal
        regType r            = panic $ "LlvmCodeGen.Reg: llvmFunArgs GlobalReg (" ++ show r ++ ") not supported!"

--------------------------------------------------------------------------------
-- * Data
--
-- TODO: Missing.
-- CmmData (part of RawCmmDecl = GenCmmDecl CmmStatics (BlockEvn CmmStatics) CmmGraph) for reference:
--
-- CmmData Section d -- d here is CmmStatics
-- Section = Section SectionType CLabel
-- SectionType = Text | Data | ReadOnlyData | RelocatableReadOnlyData | UninitialisedData | ReadOnlyData16 | OtherSection String
-- CmmStatics = Statics CLabel [CmmStatic]
-- CmmStatic = CmmStaticLit CmmLit
--           | CmmUninitialised Int
--           | CmmString [Word8]
--
-- Labels are addresses, and offsets are usually given in bytes.
-- CmmLit = CmmInt Integer Width
--        | CmmFloat Rational Width
--        | CmmVec [CmmLit]
--        | CmmLabel CLabel                     -- address of label
--        | CmmLabelOff CLabel Int              -- address of label + offset
--        | CmmLabelDiffOff CLabel CLabel Int   -- address of label1 - label2 + offset
--        | CmmBlock BlockId                    -- code label
--        | CmmHighStackMark                    -- This will *not* be supported!

genLlvmData :: RawCmmDecl -> LlvmM (Either EDSL.Error Symbol)
genLlvmData (CmmData section statics) = genStatics statics
  -- TODO: We irgnore the section right now.
  -- We will turn [CmmStatic] into a Struct.
  -- showCmm statics >>= (\s -> error $ "DATA: " ++ s)
  -- This is what we do for prefix data:
  -- Just (Statics _ statics) -> do
  -- infoStatics <- mapM genData statics
  -- return $ Just $ EDSL.struct infoStatics

genStatics :: CmmStatics -> LlvmM (Either EDSL.Error Symbol)
genStatics (Statics l statics) = do
  body <- liftM sequence (mapM genData statics)
  lbl  <- strCLabel_llvm l
  ty   <- tyCLabel_llvm l
  -- similarly to the genStaticLit, we will turn the
  -- ptr into an int.
  pure $ EDSL.global lbl . EDSL.struct' <$> body

genData :: CmmStatic -> LlvmM (Either EDSL.Error Symbol)
genData (CmmString str) = return . pure . EDSL.cStrS $ map (toEnum . fromIntegral) str
genData (CmmUninitialised bytes) = pure . Left $ "genData: Uninitialised " ++ show bytes ++ " bytes"
genData (CmmStaticLit lit) = genStaticLit lit

-- | Generate llvm code for a static literal
--
-- Will either generate the code or leave it unresolved if it is a 'CLabel'
-- which isn't yet known.
genStaticLit :: CmmLit -> LlvmM (Either EDSL.Error Symbol)
genStaticLit = \case
  (CmmInt i w)   -> pure . Right $ EDSL.int (widthInBits w) i
  (CmmFloat r w) -> pure . Left $ "genStaticLit: CmmFloat not supported!"
  (CmmVec ls)    -> pure . Left $ "genStaticLit: CmmVec not supported!"
  (CmmLabel l)   -> do
    lbl <- strCLabel_llvm l
    ty  <- tyCLabel_llvm l
    return $ EDSL.ptrToIntC ty (EDSL.label lbl (EDSL.lift ty))
  (CmmLabelOff l off) | off == 0  -> genStaticLit (CmmLabel l)
                      | otherwise -> do
                          size <- (*8) . wORD_SIZE <$> getDynFlags
                          let n = EDSL.int size off
                          l'   <- genStaticLit (CmmLabel l)
                          pure $ join $ EDSL.addC n <$> l'
  (CmmLabelDiffOff l1 l2 off) | off == 0  -> do
                                  l1' <- genStaticLit (CmmLabel l1)
                                  l2' <- genStaticLit (CmmLabel l2)
                                  pure . join $ EDSL.subC <$> l1' <*> l2'
                              | otherwise -> do
                                  size <- (*8) . wORD_SIZE <$> getDynFlags
                                  let n = EDSL.int size off
                                  l1' <- genStaticLit (CmmLabel l1)
                                  l2' <- genStaticLit (CmmLabel l2)
                                  pure . join $ EDSL.addC n <$> (join $ EDSL.subC <$> l1' <*> l2')

  (CmmBlock b)     -> pure . Left $ "genStaticLit: CmmBlock not supported!"
  CmmHighStackMark -> pure . Left $ "genStaticLit: CmmHighStackMark unsupported!"
  _                -> pure . Left $ "genStaticLit: unsupported lit!"

genLit :: BlockMap -> RegMap -> CmmLit -> Edsl Symbol
genLit blockMap regMap = \case
  (CmmInt i w)   -> pure $ EDSL.int (widthInBits w) i
  (CmmFloat r w) -> throwE "genLit: CmmFloat not supported!" -- pure $ EDSL.float
  (CmmVec ls)    -> throwE "genLit: CmmVec not supported!"
  (CmmLabel l)   -> do
    lbl <- lift . lift $ strCLabel_llvm l
    ty  <- lift . lift $ tyCLabel_llvm l
    -- FIXME: We do a ptrToInt cast here, if ty is int. This
    --        should better be done at the resolution site
    --        but we are not in the BodyBuilder at that point.
    if EDSL.isPtr ty
      then return $ EDSL.label lbl ty
      else EDSL.ptrToInt ty (EDSL.label lbl (EDSL.lift ty))
  (CmmLabelOff l o) -> do
    liftIO . putStrLn . show =<< showCmm (CmmLabelOff l o)
    width <- (*8) . wORD_SIZE <$> getDynFlags
    lbl   <- genLit blockMap regMap (CmmLabel l)
    let off = EDSL.int width o
    EDSL.add lbl off
  (CmmLabelDiffOff l1 l2 off) -> throwE "genLit: CmmLabelDiffOff not supported!"
  (CmmBlock b)                -> throwE "genLit: CmmBlock not supported!"
  CmmHighStackMark            -> throwE "genLit: CmmHighStackMark unsupported!"
  l                           -> throwE "genLit: unsupported lit!"

--------------------------------------------------------------------------------
-- * Procs
--
-- | Pretty print a 'CLabel'.
strCLabel_llvm :: CLabel -> LlvmM String
strCLabel_llvm lbl = do
  platform <- getLlvmPlatform
  dflags <- getDynFlags
  let sdoc = pprCLabel platform lbl
      str = Outp.renderWithStyle dflags sdoc (Outp.mkCodeStyle Outp.CStyle)
  return str

tyCLabel_llvm :: CLabel -> LlvmM Ty.Ty
tyCLabel_llvm lbl = do
  dflags <- getDynFlags
  let ltype = cmmLitType dflags (CmmLabel lbl)
  return $ fromCmmType ltype


genLlvmProc :: RawCmmDecl -> LlvmM ([Symbol] -> Edsl ())
genLlvmProc (CmmProc infos lbl live graph) = do
  let blocks = toBlockListEntryFirstFalseFallthrough graph
  basicBlocksCodeGen live blocks

genLlvmProc _ = panic "genLlvmProc: unhandled raw cmm decl"

getTrashRegs :: LlvmM [GlobalReg]
getTrashRegs = do plat <- getLlvmPlatform
                  return $ filter (callerSaves plat) (activeStgRegs plat)
-- | Generate code for a list of blocks that make up a complete
-- procedure.  The first block in the list is expected to be the
-- entry point.  We will prefix this with the list of all
-- registers, to use in the function body.  LLVM's mem2reg
-- optimization pass will perform the actual register allocation
-- for us.
--
basicBlocksCodeGen :: LiveGlobalRegs -> [CmmBlock] -> LlvmM ([Symbol] -> Edsl ())
basicBlocksCodeGen live bs@(entryBlock:cmmBlocks) = do
  -- insert the function prologue, containing the
  -- local registers available.  As we generate typed
  -- references for each allocation, we end up with a
  -- list of (Register, TRef)

  trash <- getTrashRegs
  let getAssignedRegs :: CmmNode O O -> [CmmReg]
      getAssignedRegs (CmmAssign reg _)= [reg]
        -- Calls will trash all registers. Unfortunately, this needs them to
        -- be stack-allocated in the first place.
      getAssignedRegs (CmmUnsafeForeignCall _ rs _) = map CmmGlobal trash ++ map CmmLocal rs
      getAssignedRegs _                             = []
      getRegsBlock (_, body, _)                     = concatMap getAssignedRegs $ blockToList body
      assignedRegs = nub . sort $ concatMap (getRegsBlock . blockSplit) bs
      localRegs  = [r | CmmLocal r  <- assignedRegs ]
      globalRegs = [r | CmmGlobal r <- assignedRegs ]
      isLive r     = r `elem` alwaysLive || r `elem` live

  dflags <- getDynFlags
  let liveGlobalRegs = activeRegs dflags globalRegs

  -- this shows the liveGlobalRegs names
  -- mapM showCmm liveGlobalRegs >>= liftIO . putStrLn . show

  return $ \args -> mdo
    (eMap, regSlots) <-        entryBlockCodeGen liveGlobalRegs args    localRegs  idMap  entryBlock
    idMap <- (eMap:) <$> mapM (basicBlockCodeGen liveGlobalRegs regSlots           idMap) cmmBlocks
    return ()

type BlockMapEntry = (Label, BasicBlockId)
type BlockMap = [BlockMapEntry]
type RegMapEntry = (CmmReg, Symbol)
type RegMap = [RegMapEntry]

entryBlockCodeGen :: LiveGlobalRegs
                  -> [Symbol]         -- ^ a set of arguments (entry block)
                  -> [LocalReg]       -- ^ a set of local registerst that will get assigned.
                  -> BlockMap
                  -> CmmBlock
                  -> Edsl (BlockMapEntry, RegMap)
entryBlockCodeGen live args localRegs idMap block = do
  let (_, nodes, tail) = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes
  EDSL.block'' id $ do
    dflags <- getDynFlags
    -- for the entry block we will turn all arguments into
    -- assignments.
    -- create space on the stack to move all the function arguments into.
    -- the result will then contain a mapping of register to the references
    -- to that virtual register
    -- We also allocate local registers before hand. (TODO: can we allocate them on demand?)
    gRegs  <- mapM allocaAndStoreArg args
    lRegs  <- mapM allocaLocalRegs localRegs
    let regMap = (zip (map CmmGlobal live) gRegs)
              ++ (zip (map CmmLocal localRegs) lRegs)
    _ <- stmtsToInstrs idMap regMap stmts
    _ <- stmtToInstrs  idMap regMap tail
    return regMap

basicBlockCodeGen :: LiveGlobalRegs                                       -- ^ live global regs
                  -> RegMap                                               -- ^ Register -> Reference map.
                  -> BlockMap                                             -- ^ a map of BlockLabel -> BlockId
                  -> CmmBlock                                             -- ^ the actual block to assemble.
                  -> Edsl BlockMapEntry
basicBlockCodeGen live regMap idMap block = do
  let (_, nodes, tail) = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes
  EDSL.block' id $ do
    dflags <- getDynFlags
    _ <- stmtsToInstrs idMap regMap stmts
    _ <- stmtToInstrs idMap regMap tail
    pure ()
-- | Convert a list of CmmNode's to LlvmStatement's
stmtsToInstrs :: BlockMap -> RegMap -> [CmmNode e x] -> Edsl ()
stmtsToInstrs blockMap regMap stmts =  mapM_ (stmtToInstrs blockMap regMap) stmts


-- NOTE TO SELF: ULabel is {-# UNPACK #-} !Label

lookup_ k = fromMaybe (panic "not found") . lookup k

lookupGlobalReg g map = case lookup (CmmGlobal g) map of
  Just slot -> pure slot
  Nothing   -> do dflags <- getDynFlags
                  panic $ "Failed to lookup global reg: " ++ showSDoc dflags (ppr g)
                  pure undefined

lookupLocalReg l map = case lookup (CmmLocal l) map of
  Just slot -> pure slot
  Nothing   -> do dflags <- getDynFlags
                  panic $ "Failed to lookup global reg: " ++ showSDoc dflags (ppr l)
                  pure undefined

lookupReg (CmmGlobal g) = lookupGlobalReg g
lookupReg (CmmLocal  l) = lookupLocalReg  l

loadGlobalReg g map = lookupGlobalReg g map >>= EDSL.load
loadLocalReg  l map = lookupLocalReg  l map >>= EDSL.load

loadReg :: CmmReg -> RegMap -> Edsl Symbol
loadReg r m = lookupReg r m >>= EDSL.load

-- | Convert a CmmStmt to a list of LlvmStatement's
stmtToInstrs :: BlockMap -> RegMap -> CmmNode e x -> Edsl ()
stmtToInstrs blockMap regMap stmt = do
  dflags <- getDynFlags
  -- liftIO . putStrLn $ "Compiling Cmm statement: " ++ showSDoc dflags (ppr stmt)
  res <- case stmt of
    -- nuke these
    CmmComment _ -> pure ()
    CmmTick    _ -> pure ()
    CmmUnwind {} -> pure () -- not yet supported

    -- CmmReg -> CmmExpr
    CmmAssign reg src -> do
      slot <- lookupReg reg regMap
      var <- exprToVar blockMap regMap src
      EDSL.store slot var

    -- CmmExpr -> CmmExpr
    CmmStore addr src -> genStore blockMap regMap addr src

    -- ULabel
    CmmBranch id      -> panic "stmtToInstrs: CmmBranch not supported!"
    -- CmmExpr -> ULabel -> ULabel -> Maybe Bool
    CmmCondBranch cond true false hint -> do
      c <- exprToVar blockMap regMap cond
      EDSL.br c (lookup_ true blockMap) (lookup_ false blockMap)
      pure ()
    -- CmmExpr -> SwitchTargets
    CmmSwitch cond ids -> panic "stmtToInstrs: CmmSwitch not supported!"

    -- Foreign call
    -- ForeignTarget -> [CmmFormal] -> [CmmActual]
    CmmUnsafeForeignCall target res args -> genCall blockMap regMap target res args

    -- Tail call
    CmmCall { cml_target = target,
              cml_args_regs = live }
      | (CmmLit (CmmLabel lbl)) <- target -> do
          -- liftIO . putStrLn $ "CmmCall"
          -- call a known function using a jump.
          fname <- lift . lift $ strCLabel_llvm lbl
          fty   <- lift . lift $ tyCLabel_llvm lbl
          fty'  <- flip fnSig live <$> (lift getDynFlags)
          -- Let's ignore this for now, and just always generate the full type.
          -- unless (fty == fty') $ panic $ "types do not match for fn " ++ show fname ++"!\n  fty: " ++ show fty ++ "\n  fty': " ++ show fty'
          EDSL.tailghccall (EDSL.ghcfun fname fty') =<< funArgs blockMap regMap live
          EDSL.retVoid

      | otherwise -> do
          -- liftIO . putStrLn $ "CmmCall other"
          s <- exprToVar blockMap regMap target
          fty <- flip fnSig live <$> (lift getDynFlags)
          f <- EDSL.intToPtr (EDSL.lift fty) s
          EDSL.tailghccall f =<< funArgs blockMap regMap live
          EDSL.retVoid

    _ -> panic "Llvm.GenCode.stmtToInstrs"
  return res


-- | A list of STG Registers that should always be considered alive
alwaysLive :: [GlobalReg]
alwaysLive = [BaseReg, Sp, Hp, SpLim, HpLim, node] -- node is in CmmExpr.

funArgs :: BlockMap -> RegMap
        -> LiveGlobalRegs
        -> Edsl [Symbol]
funArgs blockMap regMap live = do

  let liveRegs = alwaysLive ++ live
      isSSE (FloatReg _)  = True
      isSSE (DoubleReg _) = True
      isSSE (XmmReg _)    = True
      isSSE (YmmReg _)    = True
      isSSE (ZmmReg _)    = True
      isSSE _             = False

  -- Set to value or "undef" depending on whether the register is
  -- actually live
  dflags <- getDynFlags

  -- XXX platform dependence!
  -- TODO: We always load *all* regs.
  platform <- lift . lift $ getDynFlag targetPlatform
  loads <- flip mapM (filter (not . isSSE) (activeStgRegs platform)) $ \case
    r | r `elem` liveRegs -> loadGlobalReg r regMap
      | not (isSSE r)     -> pure $ EDSL.undef (fromCmmType (globalRegType dflags r))

  return loads

-- genCall ---------------------------------------------------------------------
-- Calling a foreign function
genCall :: BlockMap -> RegMap
        -> ForeignTarget -> [CmmFormal] -> [CmmActual]
        -> Edsl ()
genCall blockMap regMap target dsts args = case target of
  -- TODO: Insert Fence instruction if needed, or can we simply insert one
  --       for each platform, and LLVM will ignore where not needed?
  (PrimTarget MO_WriteBarrier) -> panic "genCall WriteBarrier not implemented"
  (PrimTarget MO_Touch)        -> pure () -- ignore
  (PrimTarget (MO_UF_Conv w))
    | ([dst],[e]) <- (dsts, args) -> panic "genCall: UF_Conv not implemented"
    | otherwise                   -> panic $ "genCall: Too many arguments to MO_UF_Conv. "
                                     ++ "Can only handle 1, given " ++ show (length args) ++ "."
  (PrimTarget (MO_Prefetch_Data localityInt))
    | ([], args) <- (dsts, args), 0 <= localityInt && localityInt <= 3 -> panic "genCall: Prefetch_Data not implemented"
    | ([], args) <- (dsts, args)             -> panic $ "prefetch locality level integer must be between 0 and 3, given." ++ (show localityInt)
    | otherwise                              -> panic $ "genCall: Prefetch_data expected exactly 0 destinations, " ++ show (length dsts) ++ " given."
  (PrimTarget (MO_PopCnt w))                 -> panic "genCall: PopCnt not implemented."
  (PrimTarget (MO_Clz w))                    -> panic "genCall: Clz not implemented."
  (PrimTarget (MO_Ctz w))                    -> panic "genCall: Ctz not implemented."
  (PrimTarget (MO_BSwap w))                  -> panic "genCall: BSwap not implemented."
  (PrimTarget (MO_AtomicRMW width amop))     -> panic "genCall: AtomicRMW not implemented."
  (PrimTarget (MO_AtomicRead _))             -> panic "genCall: AtomicRead not implemented."
  (PrimTarget (MO_Cmpxchg width))            -> panic "genCall: Cmpxchg not implemented."
  (PrimTarget (MO_AtomicWrite width))        -> panic "genCall: AtomicWrite not implemented."
  -- Handle memcpy function specifically since llvm's intrinsic version takes
  -- some extra parameters.
  (PrimTarget op)
    | ([], Just alignt) <- (dsts, machOpMemcpyishAlign op) -> panic "Memcpy special not implemented."
  -- We handle MO_U_Mul2 by simply using a 'mul' instruction, but with operands
  -- twice the width (we first zero-extend them), e.g., on 64-bit arch we will
  -- generate 'mul' on 128-bit operands. Then we only need some plumbing to
  -- extract the two 64-bit values out of 128-bit result.
  (PrimTarget (MO_U_Mul2 w))     -> panic "genCall: U_Mul2 not implemented"
  -- MO_U_QuotRem2 is another case we handle by widening the registers to double
  -- the width and use normal LLVM instructions (similarly to the MO_U_Mul2). The
  -- main difference here is that we need to combine two words into one register
  -- and then use both 'udiv' and 'urem' instructions to compute the result.
  (PrimTarget (MO_U_QuotRem2 w)) -> panic "genCall: U_QuotRem2 not implemented"
  -- Handle the MO_{Add,Sub}IntC separately. LLVM versions return a record from
  -- which we need to extract the actual values.
  (PrimTarget (MO_AddIntC w))    -> panic "genCall: AddIntC not implemented"
  (PrimTarget (MO_SubIntC w))    -> panic "genCall: SubIntC not implemented"
  -- Similar to MO_{Add,Sub}IntC, but MO_Add2 expects the first element of the
  -- return tuple to be the overflow bit and the second element to contain the
  -- actual result of the addition. So we still use genCallWithOverflow but swap
  -- the return registers.
  (PrimTarget (MO_Add2 w))       -> panic "genCall: Add2 not implemented"
  (PrimTarget (MO_SubWordC w))   -> panic "genCall: SubWordC not implemented"
  target                         -> do
    -- liftIO $ putStrLn "Generic Call"
    dflags <- getDynFlags

    -- parameter types
    let arg_type (_, AddrHint) = EDSL.i8ptr
        -- cast pointers to i8*. Llvm equivalent of void*
        arg_type (expr, _) = fromCmmType $ cmmExprType dflags expr
        -- return type
        ret_type :: [(LocalReg, ForeignHint)] -> Ty.Ty
        ret_type [] = EDSL.void
        ret_type [(_, AddrHint)] = EDSL.i8ptr
        ret_type [(reg, _)] = fromCmmType $ localRegType reg
        ret_type t = panic $ "genCall: Too many return values! Can only handle"
                          ++ " 0 or 1, given " ++ show (length t) ++ "."

    let cc = case target of
          ForeignTarget _ (ForeignConvention conv _ _ _) ->
            case conv of
              PrimCallConv -> panic "genCall: PrimCallConv"
              JavaScriptCallConv -> panic "genCall: JavaScriptCallConv"
              -- while this can be made target dependent
              -- by emitting Stdcc for X86 targets, we'll
              -- try to be non-target dependent, and go with
              -- Ccc
              -- StdCallConv | CCCallConv | CApiConv
              _ -> True -- Cc_ccc
          PrimTarget _ -> True
    {-
        CC_Ccc of the possibilities here are a worry with the use of a custom
        calling convention for passing STG args. In practice the more
        dangerous combinations (e.g StdCall + llvmGhcCC) don't occur.

        The native code generator only handles StdCall and CCallConv.
    -}

    -- call attributes
    -- TODO: somehow handle this?!
    -- let fnAttrs | never_returns = NoReturn : llvmStdFunAttrs
    --             | otherwise     = llvmStdFunAttrs

    --     never_returns = case target of
    --          ForeignTarget _ (ForeignConvention _ _ _ CmmNeverReturns) -> True
    --          _ -> False

    -- fun type
    let (res_hints, arg_hints) = foreignTargetHints target
        args_hints = zip args arg_hints
        ress_hints = zip dsts res_hints
        retTy = ret_type ress_hints
        argTy = map arg_type args_hints -- TODO: we completely ignore any param attributes!
--    let funTy = \name -> LMFunction $ LlvmFunctionDecl name ExternallyVisible
--                             lmconv retTy FixedArgs argTy (llvmFunAlign dflags)
    fn <- getFunPtr (argTy --> retTy) target

    -- try to coerce the args where necessary.
    let coerceArg t v | t == (EDSL.ty v) = pure v
                      -- if both are pointers, assume we want a bitcast
                      | EDSL.isPtr t && EDSL.isPtr (EDSL.ty v) = EDSL.bitcast t v
                      -- if the required argument is a pointer, but the value is not
                      -- assume the value represents a pointer.
                      | EDSL.isPtr t = EDSL.intToPtr t v
                      | otherwise = error . show $ text "Foreign Call type error"
                                    $+$ (text "Cannot coerce: " <+> pretty v
                                        $+$ text "to: " <+> pretty t)

    -- TODO: make use of hints.
    argVars <- zipWithM coerceArg argTy =<< mapM (exprToVar blockMap regMap) args
    let call = EDSL.ccall -- tail calls should be done through CmmJump, we'll do CCallConv and a standard call (no explicit tail)

    -- store the result value
    case retTy of
      Ty.Void   | length dsts == 0 -> call fn argVars >> pure ()
                | otherwise        -> panic $ "genCall: void result with register assignment!"
      _         | [reg] <- dsts    -> do Just res <- call fn argVars -- we *know* the function has a return value!
                                         slot <- lookupLocalReg reg regMap
                                         -- TODO: this is quite cmplex. We now go ahead
                                         --       and store res -> slot, even though we
                                         --       could later on just use the res slot.
                                         res' <- case (EDSL.ty res, EDSL.lower (EDSL.ty slot)) of
                                                   (t, s) | t == s -> pure res
                                                          -- if the slot type is a pointer
                                                          -- just bitcast the result to that.
                                                          | EDSL.isPtr s -> EDSL.bitcast s res
                                                          -- if the slot type is an Integer,
                                                          -- assume we want to store the pointer
                                                          -- address.
                                                          | EDSL.isInt s && EDSL.isPtr t -> EDSL.ptrToInt s res
                                                          | otherwise -> panic . show $ text "genCall: CmmReg" <+> pretty slot <+> text "bad match for result" <+> pretty res
                                         EDSL.store slot res'
                                         -- TODO: Add Return Nothing, if TailCall
                                         --       Add Unreachable if never_returns.
                                         --       Add nothing, otherwise.
                                         pure ()
                | otherwise        -> panic $ "genCall: Bad number of registers! Can only handle"
                                           ++ " 1, given " ++ show (length dsts) ++ "."

getFunPtr :: Ty.Ty -> ForeignTarget -> Edsl Symbol
getFunPtr ty = \case
  ForeignTarget (CmmLit (CmmLabel lbl)) _ -> do
    lbl <- lift . lift $ strCLabel_llvm lbl
    return $ EDSL.fun lbl ty
  ForeignTarget expr _ -> panic "getFunPtr \\w expr"
  PrimTarget mop -> panic "getFunPtr \\w primOp"
  _              -> panic "getFunPtr, unknown case not implemented!"

-- genStore --------------------------------------------------------------------
-- TODO: WIP!
-- | CmmStore operation
genStore :: BlockMap -> RegMap -> CmmExpr -> CmmExpr -> Edsl ()
-- First we try to detect a few common cases and produce better code for
-- these then the default case. We are mostly trying to detect Cmm code
-- like I32[Sp + n] and use 'getelementptr' operations instead of the
-- generic case that uses casts and pointer arithmetic
genStore blockMap regMap addrE val = case addrE of
  (CmmReg (CmmGlobal r))       -> genStore_fast' addrE r 0 =<< exprToVar blockMap regMap val
  (CmmRegOff (CmmGlobal r) n)  -> genStore_fast' addrE r n =<< exprToVar blockMap regMap val
  (CmmMachOp (MO_Add _)
   [ (CmmReg (CmmGlobal r))
   , (CmmLit (CmmInt n _)) ])  -> genStore_fast' addrE r (fromInteger n) =<< exprToVar blockMap regMap val
  (CmmMachOp (MO_Sub _)
   [ (CmmReg (CmmGlobal r))
   , (CmmLit (CmmInt n _)) ])  -> genStore_fast' addrE r (negate $ fromInteger n) =<< exprToVar blockMap regMap val
  _                            -> genStore_slow' addrE =<< exprToVar blockMap regMap val

  where genStore_fast' = genStore_fast blockMap regMap
        genStore_slow' = genStore_slow blockMap regMap


-- | CmmStore operation
-- This is a special case for storing to a global register pointer
-- offset such as I32[Sp+8].
genStore_fast :: BlockMap -> RegMap
              -> CmmExpr -> GlobalReg -> Int -> Symbol
              -> Edsl ()
genStore_fast blockMap regMap addr r n val = do
  -- ptrSize (ptrs are the size of words)
  ptrSize <- (*8) . wORD_SIZE <$> (lift getDynFlags)
  slot <- loadGlobalReg r regMap
  let slotTy = EDSL.ty slot
  -- Note: n is in bytes. Hence we need to compute the actual offset
  --       depending on the underlying structure ourself.  As the
  --       getElementPointer works relative to the size of the
  --       underlying structure.
  -- we could compute the size of the element using gep.
  -- see: http://stackoverflow.com/a/30830445
  -- That way, we would need to insert additional blocks to
  -- handle the slow case, as we would need to verify that there
  -- is no remainder.
  --
  -- for now we will assume a pointer has the size of a word.
      (ix, rem) = n `divMod` ((EDSL.size ptrSize slotTy) `div` 8)
  if EDSL.isPtr slotTy && rem == 0
    then do ptr <- EDSL.gep slot [EDSL.int32 ix]
            -- liftIO . putStrLn $ "(genStore_fast)gep: " ++ show (pretty slot) ++ " at " ++ show ix ++ " -> " ++ show (pretty ptr)
            EDSL.store ptr val
    -- if its a bit type then we use the slow method since we
    -- can't avoid casting anyway.
    else genStore_slow blockMap regMap addr val

genStore_slow blockMap regMap addrExpr val = do
  slot <- exprToVar blockMap regMap addrExpr
  -- case EDSL.ty slot of
  panic $ "genStore_slow:\n Slot: " ++ (show slot) ++ "\n  Val: " ++ (show val)

--------------------------------------------------------------------------------
-- * CmmExpr code generation

exprToVar :: BlockMap -> RegMap -> CmmExpr -> Edsl Symbol
exprToVar blockMap regMap = \case
  -- Literal
  CmmLit lit         -> genLit blockMap regMap lit
  -- Read memory location
  CmmLoad e' ty      -> genLoad blockMap regMap e' ty
  -- Contents of register
  CmmReg r           -> loadReg r regMap -- TODO, might need to cast to int, as Cmm expects ints. See getCmmReg
  -- Machine operation
  CmmMachOp op exprs -> genMachOp blockMap regMap op exprs
  -- Expand the CmmRegOff shorthand.
  CmmRegOff reg off  -> do dflags <- lift getDynFlags
                           let rep = typeWidth (cmmRegType dflags reg)
                             in exprToVar blockMap regMap $ CmmMachOp (MO_Add rep) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) rep)]
  CmmStackSlot _ _   -> panic "exprToVar: CmmStackSlot not supported!"


-- TODO: We might also want to short cut ((Reg +/- N) +/- M)
--       Instead of getting the relative offset of R and then
--       computing ptrToInt -> add/sub -> intToPtr.
genLoad :: BlockMap -> RegMap -> CmmExpr -> CmmType -> Edsl Symbol
genLoad blockMap regMap e ty = case e of
  (CmmReg (CmmGlobal r))        -> genLoad_fast' e r 0 ty
  (CmmRegOff (CmmGlobal r) n)   -> genLoad_fast' e r n ty
  (CmmMachOp (MO_Add _)
   [ (CmmReg (CmmGlobal r))
   , (CmmLit (CmmInt n _))]) -> genLoad_fast' e r (fromInteger n) ty
  (CmmMachOp (MO_Sub _)
   [ (CmmReg (CmmGlobal r))
   , (CmmLit (CmmInt n _))]) -> genLoad_fast' e r (negate $ fromInteger n) ty
  _ -> genLoad_slow' e ty
  where genLoad_fast' = genLoad_fast blockMap regMap
        genLoad_slow' = genLoad_slow blockMap regMap

genLoad_fast :: BlockMap -> RegMap
             -> CmmExpr -> GlobalReg -> Int -> CmmType
             -> Edsl Symbol
genLoad_fast blockMap regMap e r n ty = do
  ptrSize <- (*8) <$> wORD_SIZE <$> (lift getDynFlags)
  slot <- lookupGlobalReg r regMap
  let slotTy = EDSL.lower (EDSL.ty slot)
      expectTy = fromCmmType ty
      (ix, rem) = n `divMod` ((EDSL.size ptrSize slotTy) `div` 8)
  case rem of
    -- if its a bit type then we use the slow method since we
    -- can't avoid casting anyway.
    r | r /= 0 -> genLoad_slow blockMap regMap e ty
      | not (EDSL.isPtr slotTy) -> genLoad_slow blockMap regMap e ty
      | not (baseTy slotTy == baseTy expectTy) -> error . show $ pretty "(genLoad_fast)gep:" <+> (text "Type error, slot and expectedTy do not have the same base."
                                                                                                  $+$ text "Slot:" <+> pretty slotTy
                                                                                                  $+$ text "ExpT:" <+> pretty expectTy)
      -- If the ptrLevel of slotTy (lowered slot) and expectTy match. Let's just GEP it.
      | ptrLvl slotTy == ptrLvl expectTy -> EDSL.load =<< EDSL.gep slot [EDSL.int32 ix]
      -- If the ptrLevel of the slotTy is larger; we need to bitcast the result.
      | ptrLvl slotTy >  ptrLvl expectTy -> EDSL.load =<< EDSL.bitcast (EDSL.lift expectTy) =<< EDSL.gep slot [EDSL.int32 ix]
      -- this is just not logical!
      | otherwise -> error . show $
            pretty "(genLoad_fast)gep:" <+> (    text "Slot:" <+> pretty slot     <+> text "ptrLvl" <+> int (ptrLvl (EDSL.ty slot))
                                             $+$ text "ExpT:" <+> pretty expectTy <+> text "ptrLvl" <+> int (ptrLvl expectTy))

  where
    ptrLvl t | EDSL.isPtr t = 1 + ptrLvl (EDSL.lower t)
             | otherwise = 0
    baseTy t | EDSL.isPtr t = baseTy (EDSL.lower t)
             | otherwise = t

genLoad_slow :: BlockMap -> RegMap
             -> CmmExpr -> CmmType
             -> Edsl Symbol
genLoad_slow blockMap regMap e ty = do
  ptr <- exprToVar blockMap regMap e
  e' <- showCmm e
  ty' <- showCmm ty
  if fromCmmType ty == (EDSL.ty ptr)
    then pure ptr
    else if fromCmmType ty == (EDSL.lower (EDSL.ty ptr))
         then EDSL.load ptr
         else panic $ "genLoad_slow not implemented, expr: " ++ e' ++ "("++ ty' ++ ")" ++ " -> " ++ show ptr

genMachOp :: BlockMap -> RegMap -> MachOp -> [CmmExpr] -> Edsl Symbol
genMachOp blockMap regMap op [x] = panicOp
  -- case op of
  -- _        -> panicOp
  where panicOp = panic $ "LLVM.CodeGen.genMachOp: non unary op encountered"
                       ++ "with one argument! (" ++ show op ++ ")"

-- Handle GlobalRegs pointers
genMachOp blockMap regMap o@(MO_Add _) e@[(CmmReg (CmmGlobal r)), (CmmLit (CmmInt n _))]
    = genMachOp_fast blockMap regMap o r (fromInteger n) e

genMachOp blockMap regMap o@(MO_Sub _) e@[(CmmReg (CmmGlobal r)), (CmmLit (CmmInt n _))]
    = genMachOp_fast blockMap regMap o r (negate . fromInteger $ n) e

-- Generic case
genMachOp blockMap regMap op e = genMachOp_slow blockMap regMap op e

genMachOp_fast blockMap regMap op r n e = do
  -- See genStore_fast
  ptrSize <- (*8) <$> wORD_SIZE <$> (lift getDynFlags)
  slot <- loadGlobalReg r regMap
  let slotTy = EDSL.ty slot
      (ix, rem) = n `divMod` ((EDSL.size ptrSize slotTy) `div` 8)
  if EDSL.isPtr slotTy && rem == 0
    then EDSL.gep slot [EDSL.int32 ix]
    else genMachOp_slow blockMap regMap op e

-- | Handle CmmMachOp expressions
-- This handles all the cases not handle by the specialised genMachOp_fast.
-- Element extraction
genMachOp_slow :: BlockMap -> RegMap -> MachOp -> [CmmExpr] -> Edsl Symbol
-- genMachOp_slow blockMap regMap (MO_V_Extract  l w) [val, idx] = return
-- genMachOp_slow blockMap regMap (MO_VF_Extract l w) [val, idx] = return
-- -- Element insertion
-- genMachOp_slow blockMap regMap (MO_V_Insert  l w) [val, elt, idx] = return
-- genMachOp_slow blockMap regMap (MO_VF_Insert l w) [val, elt, idx] = return
-- -- Binary MachOp
genMachOp_slow blockMap regMap op [x, y] = case op of
    MO_Eq _   -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.ieq lhs rhs
    MO_Ne _   -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.ineq lhs rhs
    MO_S_Gt _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.isgt lhs rhs
    MO_S_Ge _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.isge lhs rhs
    MO_S_Lt _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.islt lhs rhs
    MO_S_Le _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.isle lhs rhs

    MO_U_Gt _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.iugt lhs rhs
    MO_U_Ge _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.iuge lhs rhs
    MO_U_Lt _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.iult lhs rhs
    MO_U_Le _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.iule lhs rhs

    MO_Add _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.add lhs rhs
    MO_Sub _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      if EDSL.ty lhs == EDSL.ty rhs
        then EDSL.sub lhs rhs
        else if EDSL.isPtr (EDSL.ty lhs)
             -- we likely want pointer arithmetic.
             -- TODO: Use GEP or fold into parent
             --       instruction, see TODO above.
             --       not sure if that's even safe/legal.
             --       otherwise we migth have to adjust the
             --       load logic, to always ptrToInt...
             then do lhs' <- EDSL.ptrToInt (EDSL.ty rhs) lhs
                     EDSL.sub lhs' rhs
             else error $ "Cannot sub: " ++ (show . pretty . EDSL.ty $ rhs) ++ " from " ++ (show . pretty . EDSL.ty $ lhs)
    MO_Mul _ -> do
      lhs <- exprToVar blockMap regMap x
      rhs <- exprToVar blockMap regMap y
      EDSL.mul lhs rhs

    -- MO_U_MulMayOflo _ -> panic "genMachOp: MO_U_MulMayOflo unsupported!"

    -- MO_S_MulMayOflo w -> isSMulOK w x y

    -- MO_S_Quot _ -> genBinMach LM_MO_SDiv
    -- MO_S_Rem  _ -> genBinMach LM_MO_SRem

    -- MO_U_Quot _ -> genBinMach LM_MO_UDiv
    -- MO_U_Rem  _ -> genBinMach LM_MO_URem

    -- MO_F_Eq _ -> genBinComp opt LM_CMP_Feq
    -- MO_F_Ne _ -> genBinComp opt LM_CMP_Fne
    -- MO_F_Gt _ -> genBinComp opt LM_CMP_Fgt
    -- MO_F_Ge _ -> genBinComp opt LM_CMP_Fge
    -- MO_F_Lt _ -> genBinComp opt LM_CMP_Flt
    -- MO_F_Le _ -> genBinComp opt LM_CMP_Fle

    -- MO_F_Add  _ -> genBinMach LM_MO_FAdd
    -- MO_F_Sub  _ -> genBinMach LM_MO_FSub
    -- MO_F_Mul  _ -> genBinMach LM_MO_FMul
    -- MO_F_Quot _ -> genBinMach LM_MO_FDiv

    -- MO_And _   -> genBinMach LM_MO_And
    -- MO_Or  _   -> genBinMach LM_MO_Or
    -- MO_Xor _   -> genBinMach LM_MO_Xor
    -- MO_Shl _   -> genBinMach LM_MO_Shl
    -- MO_U_Shr _ -> genBinMach LM_MO_LShr
    -- MO_S_Shr _ -> genBinMach LM_MO_AShr

    -- MO_V_Add l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Add
    -- MO_V_Sub l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Sub
    -- MO_V_Mul l w   -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_Mul

    -- MO_VS_Quot l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_SDiv
    -- MO_VS_Rem  l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_SRem

    -- MO_VU_Quot l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_UDiv
    -- MO_VU_Rem  l w -> genCastBinMach (LMVector l (widthToLlvmInt w)) LM_MO_URem

    -- MO_VF_Add  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FAdd
    -- MO_VF_Sub  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FSub
    -- MO_VF_Mul  l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FMul
    -- MO_VF_Quot l w -> genCastBinMach (LMVector l (widthToLlvmFloat w)) LM_MO_FDiv

    MO_Not _       -> panicOp
    MO_S_Neg _     -> panicOp
    MO_F_Neg _     -> panicOp

    MO_SF_Conv _ _ -> panicOp
    MO_FS_Conv _ _ -> panicOp
    MO_SS_Conv _ _ -> panicOp
    MO_UU_Conv _ _ -> panicOp
    MO_FF_Conv _ _ -> panicOp

    MO_V_Insert  {} -> panicOp
    MO_V_Extract {} -> panicOp

    MO_VS_Neg {} -> panicOp

    MO_VF_Insert  {} -> panicOp
    MO_VF_Extract {} -> panicOp

    MO_VF_Neg {} -> panicOp

    _            -> panicOp
    where
      lowerIfNeeded :: Symbol -> Edsl Symbol
      lowerIfNeeded x | EDSL.isPtr (EDSL.ty x) = EDSL.ptrToInt (EDSL.lower (EDSL.ty x)) x
                      | otherwise = return x
      panicOp = panic $ "LLVM.CodeGen.genMachOp_slow: unary op encountered"
                ++ "with two arguments! (" ++ show op ++ ")"

genMachOp_slow blockMap regMap op e = panic $ "genMachOp_slow not supported for (" ++ show op ++ ")."
