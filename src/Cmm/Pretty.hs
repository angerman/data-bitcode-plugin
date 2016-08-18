{-# LANGUAGE GADTs #-}
module Cmm.Pretty where

import Cmm --              ( RawCmmGroup )
import CLabel
import BlockId
import Outputable (panic, ppr, showSDocUnsafe)

import CmmUtils (toBlockListEntryFirst )
import Hoopl

import Text.PrettyPrint

ppDecl :: GenCmmDecl CmmStatics (BlockEnv CmmStatics) CmmGraph -> IO ()
ppDecl (CmmProc hdr lbl regs graph) = putStrLn $ show $ text "Proc" <+> (ppLabel lbl <+> (if externallyVisibleCLabel lbl then text "Extern" else text "Intern")
                                                                         $+$ ppProcGraph graph
                                                                        )
ppDecl (CmmData (Section t l) (Statics l2 statics))
  = putStrLn $ show $ text (show t) <+> (ppLabel l <+> (if externallyVisibleCLabel l then text "Extern" else text "Intern")
    $+$ ppLabel l2 <+> text ":" <+> int (length statics) <+> text "els:") <+> ppStatics statics


--------------------------------------------------------------------------------
-- Data (CmmStatics)
ppStatics :: [CmmStatic] -> Doc
ppStatics = hcat' . map ppStatic

ppStatic :: CmmStatic -> Doc
ppStatic (CmmString str) = text "Str" <+> (hcat' $ map (int . fromIntegral) str)
ppStatic (CmmUninitialised n) = text "U" <> int n
ppStatic (CmmStaticLit lit) = text "Lit" <+> ppStaticLit lit

ppStaticLit (CmmInt i w) = text "Int" <+> integer i <+> text "width" <+> int (widthInBits w)
ppStaticLit (CmmFloat r w) = text "Flt" <+> rational r <+> text "width" <+> int (widthInBits w)
ppStaticLit (CmmVec ls) = text "Vec" <+> hcat' (map ppStaticLit ls)
ppStaticLit (CmmLabel l) = text "Lbl" <+> ppLabel l
ppStaticLit (CmmLabelOff l off) = text "OffLbl" <+> ppLabel l <+> text " offset " <+> int off
ppStaticLit (CmmLabelDiffOff l l2 off) = text "DiffOffLbl" <+> ppLabel l <+> ppLabel l2 <+> text " offset " <+> int off
ppStaticLit (CmmBlock b) = text "Block (?)"

ppLabel :: CLabel -> Doc
ppLabel = text . showSDocUnsafe . ppr

hcat' = hcat . punctuate comma

--------------------------------------------------------------------------------
-- Procs
ppProcGraph :: CmmGraph -> Doc
ppProcGraph g = text "Blocks" <+> (int (length blocks + 1) $+$ vcat (map ppProcBlock bs))
  where bs@(entry:blocks) = toBlockListEntryFirst g
        -- Note: There is also toBlockListEntryFirstFalseFallthrough,
        --       which supposedly is required for LLVM to generate fast
        --       code.  Maybe it still is?
ppProcGraph _ = panic "ppProc: unknown proc."

ppProcBlock :: CmmBlock -> Doc
ppProcBlock b = let (head, nodes, tail) = blockSplit b
  in text "Head:" <+> nest 3 (ppInst head)
     $+$ text "Nodes:" <+> nest 3 (vcat (map ppInst (blockToList nodes)))
     $+$ text "Tail:" <+> nest 3 (ppInst tail)

ppInst :: CmmNode e x -> Doc
ppInst (CmmEntry l _) = text "Entry" <+> text (show l)
ppInst (CmmComment _) = text "Comment"
ppInst (CmmTick _)    = text "Tick"
ppInst (CmmUnwind gr e) = text "Unwind"
ppInst (CmmAssign r e) = text "Assign" <+> ppReg r <+> text "<-" <+> ppExpr e
ppInst (CmmStore e e2) = text "Store" <+> ppExpr e <+> text "->" <+> ppExpr e2
ppInst (CmmBranch l) = text "UBranch" <+> text (show l)
ppInst (CmmCondBranch cond onTrue onFalse hint) = text "Branch" <+> text (show onTrue) <+> text (show onFalse)
ppInst (CmmSwitch e ts) = text "Switch"
ppInst (CmmUnsafeForeignCall target formal actual) = text "UnsafeForeignCall"
ppInst (CmmCall{}) = text "Call"
ppInst (CmmForeignCall{}) = text "SafeForeignCall"
ppInst _ = panic "ppInst: unknown inst."

ppReg :: CmmReg -> Doc
ppReg (CmmLocal (LocalReg ident ty)) = text "LReg" <> text (show ident)
ppReg (CmmGlobal (VanillaReg n p))  = text "GRegV" <> int n
ppReg (CmmGlobal (FloatReg n))      = text "GRegF" <> int n
ppReg (CmmGlobal (DoubleReg n))     = text "GRegD" <> int n
ppReg (CmmGlobal (LongReg n))       = text "GRegL" <> int n
ppReg (CmmGlobal (XmmReg n))        = text "GRegX" <> int n
ppReg (CmmGlobal (YmmReg n))        = text "GRegY" <> int n
ppReg (CmmGlobal (ZmmReg n))        = text "GRegZ" <> int n
ppReg (CmmGlobal Sp)                = text "GSp"
ppReg (CmmGlobal SpLim)             = text "GSpLim"
ppReg (CmmGlobal Hp)                = text "GHp"
ppReg (CmmGlobal HpLim)             = text "GHpLim"
ppReg (CmmGlobal CCCS)              = text "GCCCS"
ppReg (CmmGlobal CurrentTSO)        = text "GCurrentTSO"
ppReg (CmmGlobal CurrentNursery)    = text "GCurrentNursery"
ppReg (CmmGlobal HpAlloc)           = text "GHpAlloc"
ppReg (CmmGlobal EagerBlackholeInfo) = text "GEagerBlackholeInfo"
ppReg (CmmGlobal GCEnter1)           = text "GGCEnter1"
ppReg (CmmGlobal GCFun)             = text "GGCFun"
ppReg (CmmGlobal BaseReg)           = text "GBaseReg"
ppReg (CmmGlobal MachSp)            = text "GMachSp"
ppReg (CmmGlobal UnwindReturnReg)   = text "GUnwindReturnReg"
ppReg (CmmGlobal PicBaseReg)        = text "GPicBaseReg" -- only NCG

ppExpr (CmmLit lit)                 = text "Literal" <+> ppStaticLit lit
ppExpr (CmmLoad e t)                = text "Load" <+> ppExpr e
ppExpr (CmmReg r)                   = text "RegVal of" <+> ppReg r
ppExpr (CmmMachOp _ _)              = text "MachOp (?)"
ppExpr (CmmStackSlot _ _)           = text "StackSlot (?)"
ppExpr (CmmRegOff r i)              = text "RegOffset, reg" <+> ppReg r <+> text "offset" <+> int i
