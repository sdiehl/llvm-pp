{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Library (
  ppllvm,
) where

import GHC.Word

import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.Float as F
import LLVM.General.AST.Type hiding (float, double)

import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import LLVM.General.AST.Attribute

import Data.String
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen

import Data.List (intersperse)

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

commas :: [Doc] -> Doc
commas  = hsep . punctuate (char ',')

colons :: [Doc] -> Doc
colons  = hcat . intersperse (char ':')

hlinecat :: [Doc] -> Doc
hlinecat = hcat . intersperse (hardline <> hardline)

wrapbraces :: Doc -> Doc
wrapbraces x = char '{' <$$> ( x ) <$$> char '}'

local :: Doc -> Doc
local a = "%" <> a

global :: Doc -> Doc
global a = "@" <> a

label :: Doc -> Doc
label a = "label" <+> "%" <> a

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

class PP p where
  ppr :: Int -> p -> Doc

  {-# INLINE pp #-}
  pp :: p -> Doc
  pp = ppr 0

instance PP String where
  ppr p x = text x

instance PP Word32 where
  ppr p x = int (fromIntegral x)

instance PP Integer where
  ppr p = integer

instance PP Name where
  ppr p (Name x) = text x
  ppr p (UnName x) = int (fromIntegral x)

instance PP Parameter where
  ppr p (Parameter ty name attrs) = pp ty <+> local (pp name)

instance PP ([Parameter], Bool) where
  ppr p (params, False) = commas (fmap pp params)

instance PP (Operand, [ParameterAttribute]) where
  ppr p (op, attrs) = pp op

instance PP Type where
  ppr p (IntegerType width) = "i" <> pp width
  ppr p (FloatingPointType 32 spec) = "float"
  ppr p (FloatingPointType 64 spec) = "double"
  ppr p (FloatingPointType width spec) = "f" <> pp width

  ppr p (VoidType) = "void"
  ppr p (PointerType ref@(FunctionType {}) addr) = pp ref
  ppr p (PointerType ref addr) = pp ref <> "*"
  ppr p (FunctionType retty argtys vararg) = pp retty <+> parens (commas $ fmap pp argtys)
  ppr p x = error (show x)

instance PP Global where
  ppr p (Function {..}) =
      case basicBlocks of
        [] ->
          ("declare" <+> pp returnType <+> global (pp name) <> parens (ppAnonParams parameters))

        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] ->
            ("define" <+> pp returnType <+> global (pp name) <> parens (pp parameters)) <+>
            wrapbraces (indent 2 $ ppSingleBlock b)

        bs ->
          ("define" <+> pp returnType <+> global (pp name) <> parens (pp parameters)) <+>
           wrapbraces ((vcat $ fmap pp bs))
    where
      blk a = nest 2 a

instance PP Definition where
  ppr p (GlobalDefinition x) = pp x

instance PP BasicBlock where
  ppr p (BasicBlock nm instrs term) =
    nest 2 (pp nm <> ":" <$$> ((vcat $ (fmap pp instrs) ++ [pp term])))

instance PP Terminator where
  ppr p (Br dest meta) = "br" <+> label (pp dest)
  ppr p (Ret val meta) = "ret" <+> maybe "void" pp val
  ppr p (CondBr cond tdest fdest meta) =
      "br" <+> pp cond
    <> "," <+> label (pp tdest)
    <> "," <+> label (pp fdest)
  ppr p x = error (show x)

instance PP Instruction where
  ppr p (Mul {..}) = "mul" <+> ppr 0 operand0 <> "," <+> ppr 1 operand1
  ppr p (Add {..}) = "add" <+> ppr 0 operand0 <> "," <+> ppr 1 operand1

  ppr p (FMul {..}) = "fmul" <+> ppr 0 operand0 <> "," <+> ppr 1 operand1
  ppr p (FAdd {..}) = "fadd" <+> ppr 0 operand0 <> "," <+> ppr 1 operand1
  ppr p (FCmp {..}) = "fcmp" <+> pp fpPredicate <+> ppr 0 operand0 <> "," <+> ppr 1 operand1

  ppr p (Alloca {..}) = "alloca" <+> pp allocatedType
  ppr p (Store {..}) = "store" <+> pp value <> "," <+> pp address
  ppr p (Load {..}) = "load" <+> pp address
  ppr p (Phi {..}) = "phi" <+> pp type' <+> commas (fmap phiIncoming incomingValues)

  ppr p f@(Call {}) = "call" <+>  ppFunction f

  ppr p x = error (show x)

instance PP CallableOperand where
  ppr p (Left asm) = undefined
  ppr p (Right op) = ppr 1 op

instance PP Operand where
  ppr 0 (LocalReference ty nm) = pp ty <+> local (pp nm)
  ppr 1 (LocalReference ty nm) = local (pp nm)

  ppr 0 c@(ConstantOperand con) = typePrefix c <+> pp con
  ppr 1 (ConstantOperand con) = pp con

instance PP C.Constant where
  ppr p (C.Int width val) = pp val
  ppr p (C.Float (F.Double val)) = text $ printf "%6.6e" val
  ppr p (C.Float (F.Single val)) = text $ printf "%6.6e" val
  ppr p (C.GlobalReference ty nm) = pp ty <+> pp nm
  ppr p x = error (show x)

instance PP a => PP (Named a) where
  ppr p (nm := a) = "%" <> pp nm <+> "=" <+> pp a
  ppr p (Do a) = pp a

instance PP Module where
  ppr p (Module {..}) =
    let header = printf "; ModuleID = '%s'" moduleName in
    hlinecat (fromString header : (fmap pp moduleDefinitions))

instance PP (FP.FloatingPointPredicate) where
  ppr p op = case op of
   FP.False -> "false"
   FP.True  -> "false"
   FP.ONE   -> "one"

-------------------------------------------------------------------------------
-- Special Case Hacks
-------------------------------------------------------------------------------

phiIncoming :: (Operand, Name) -> Doc
phiIncoming (op, nm) = brackets (ppr 1 op <> "," <+> (local (pp nm)))

ppAnonParam :: Parameter -> Doc
ppAnonParam (Parameter ty _ _) = pp ty

ppAnonParams :: ([Parameter], Bool) -> Doc
ppAnonParams (ps, varrg) = commas (fmap ppAnonParam ps)

-- functions rearrange the arguments in a non-consistent way
ppFunction :: Instruction -> Doc
ppFunction
  (Call { function = Right (ConstantOperand (C.GlobalReference (PointerType (FunctionType retty argtys vararg) addr) nm)) ,..})
  = pp retty <+> "@" <> pp nm <> parens (commas $ fmap pp arguments)

ppSingleBlock :: BasicBlock -> Doc
ppSingleBlock (BasicBlock nm instrs term) = ((vcat $ (fmap pp instrs) ++ [pp term]))

typePrefix (LocalReference ty _) = pp ty
typePrefix (ConstantOperand (C.Int width val)) = "i" <> pp width
typePrefix (ConstantOperand (C.Float (F.Single _))) = "float"
typePrefix (ConstantOperand (C.Float (F.Double _))) = "double"
typePrefix (ConstantOperand (C.GlobalReference ty nm)) = pp ty <+> pp nm

ppllvm :: Module -> String
ppllvm mod  = displayS (renderPretty 0.4 100 (ppr 0 mod)) ""
