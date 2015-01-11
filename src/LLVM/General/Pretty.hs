{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LLVM.General.Pretty (
  ppllvm,
) where

import GHC.Word

import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.Type

import LLVM.General.AST.Attribute
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.AddrSpace as AS
import qualified LLVM.General.AST.Float as F

import Data.String
import Text.Printf
import Text.PrettyPrint

import Data.Char (chr, ord, isControl, isLetter, isDigit)
import Data.List (intersperse)
import Numeric (showHex)

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
hlinecat = vcat . intersperse (text "")

wrapbraces :: Doc -> Doc -> Doc
wrapbraces leadIn x = (leadIn <> char '{') $+$ ( x ) $+$ char '}'

local :: Doc -> Doc
local a = "%" <> a

global :: Doc -> Doc
global a = "@" <> a

label :: Doc -> Doc
label a = "label" <+> "%" <> a

-----
-- Reasoning about types
-----

class Typed a where
    typeOf :: a -> Type

instance Typed Operand where
    typeOf (LocalReference t _) = t
    typeOf (ConstantOperand c)  = typeOf c
    typeOf _                    = MetadataType

instance Typed C.Constant where
    typeOf (C.Int bits _)  = IntegerType bits
    typeOf (C.Float float) = typeOf float
    typeOf (C.Null t)      = t
    typeOf (C.Struct {..}) = StructureType isPacked (map typeOf memberValues)
    typeOf (C.Array {..})  = ArrayType (fromIntegral $ length memberValues) memberType
    typeOf (C.Vector {..}) = case memberValues of
                                    [] -> error "Vectors of size zero are not allowed"
                                    (x:_) -> ArrayType (fromIntegral $ length memberValues) (typeOf x)
    typeOf (C.Undef t)     = t
    typeOf (C.BlockAddress {..})   = PointerType (IntegerType 8) (AS.AddrSpace 0)
    typeOf (C.GlobalReference t _) = t

instance Typed F.SomeFloat where
    typeOf (F.Half _)          = FloatingPointType 16  IEEE
    typeOf (F.Single _)        = FloatingPointType 32  IEEE
    typeOf (F.Double _)        = FloatingPointType 64  IEEE
    typeOf (F.Quadruple _ _)   = FloatingPointType 128 IEEE
    typeOf (F.X86_FP80 _ _)    = FloatingPointType 80  DoubleExtended
    typeOf (F.PPC_FP128 _ _)   = FloatingPointType 80  PairOfFloats

instance Typed Global where
    typeOf (GlobalVariable {..}) = type'
    typeOf (GlobalAlias {..})    = type'
    typeOf (Function {..})       = let (params, isVarArg) = parameters
                                   in FunctionType returnType (map typeOf params) isVarArg
instance Typed Parameter where
    typeOf (Parameter t _ _) = t

isFunctionPtr (PointerType (FunctionType {..}) _) = True
isFunctionPtr _ = False

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

class PP p where
  pp :: p -> Doc

ppMaybe (Just x) = pp x
ppMaybe Nothing = empty

instance PP Word32 where
  pp x = int (fromIntegral x)

instance PP Word64 where
  pp x = int (fromIntegral x)

instance PP Integer where
  pp = integer

instance PP Name where
  pp (Name []) = doubleQuotes empty
  pp (Name name@(first:_))
    | isFirst first && all isRest name = text name
    | otherwise = doubleQuotes . hcat . map escape $ name
    where
        isFirst c = isLetter c || c == '-' || c == '_'
        isRest c = isDigit c || isFirst c
  pp (UnName x) = int (fromIntegral x)

instance PP Parameter where
  pp (Parameter ty name attrs) = pp ty <+> local (pp name)

instance PP ([Parameter], Bool) where
  pp (params, False) = commas (fmap pp params)

instance PP (Operand, [ParameterAttribute]) where
  pp (op, attrs) = ppTyped op

instance PP Type where
  pp (IntegerType width) = "i" <> pp width
  pp (FloatingPointType 16    IEEE) = "half"
  pp (FloatingPointType 32    IEEE) = "float"
  pp (FloatingPointType 64    IEEE) = "double"
  pp (FloatingPointType width IEEE) = "fp" <> pp width
  pp (FloatingPointType width DoubleExtended) = "x86_fp" <> pp width
  pp (FloatingPointType width PairOfFloats)   = "ppc_fp" <> pp width

  pp VoidType = "void"
  pp (PointerType ref@(FunctionType {}) addr) = pp ref
  pp (PointerType ref addr) = pp ref <> "*"
  pp ft@(FunctionType {..}) = pp resultType <+> ppFunctionArgumentTypes ft
  pp (VectorType {..}) = "<" <> pp nVectorElements <+> "x" <+> pp elementType <> ">"
  pp (StructureType {..}) = if isPacked
                               then "<{" <> (commas $ fmap pp elementTypes ) <> "}>"
                               else  "{" <> (commas $ fmap pp elementTypes ) <> "}"
  pp (ArrayType {..}) = brackets $ pp nArrayElements <+> "x" <+> pp elementType
  pp (NamedTypeReference name) = "%" <> pp name

instance PP Global where
  pp (Function {..}) =
      case basicBlocks of
        [] ->
          ("declare" <+> pp returnType <+> global (pp name) <> ppParams (pp . typeOf) parameters)

        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] ->
            ("define" <+> pp returnType <+> global (pp name) <> ppParams pp parameters)
            `wrapbraces` (nest 2 $ ppSingleBlock b)

        bs ->
          ("define" <+> pp returnType <+> global (pp name) <> ppParams pp parameters)
           `wrapbraces` (vcat $ fmap pp bs)

  pp (GlobalVariable {..}) = global (pp name) <+> "=" <+> "global" <+> pp type' <+> ppMaybe initializer


instance PP Definition where
  pp (GlobalDefinition x) = pp x

instance PP BasicBlock where
  pp (BasicBlock nm instrs term) =
    pp nm <> ":" $+$ nest 2 (vcat $ (fmap pp instrs) ++ [pp term])

instance PP Terminator where
  pp (Br dest meta) = "br" <+> label (pp dest)
  pp (Ret val meta) = "ret" <+> maybe "void" ppTyped val
  pp (CondBr cond tdest fdest meta) =
      "br" <+> ppTyped cond
    <> "," <+> label (pp tdest)
    <> "," <+> label (pp fdest)

instance PP Instruction where
  pp (Mul {..}) = "mul" <+> ppTyped operand0 <> "," <+> pp operand1
  pp (Add {..}) = "add" <+> ppTyped operand0 <> "," <+> pp operand1
  pp (Sub {..}) = "sub" <+> ppTyped operand0 <> "," <+> pp operand1
  pp (FSub {..}) = "fsub" <+> ppTyped operand0 <> "," <+> pp operand1
  pp (FMul {..}) = "fmul" <+> ppTyped operand0 <> "," <+> pp operand1

  pp (FAdd {..}) = "fadd" <+> ppTyped operand0 <> "," <+> pp operand1
  pp (FCmp {..}) = "fcmp" <+> pp fpPredicate <+> ppTyped operand0 <> "," <+> pp operand1

  pp (Alloca {..}) = "alloca" <+> pp allocatedType
  pp (Store {..}) = "store" <+> ppTyped value <> "," <+> ppTyped address
  pp (Load {..}) = "load" <+> ppTyped address
  pp (Phi {..}) = "phi" <+> pp type' <+> commas (fmap phiIncoming incomingValues)

  pp (ICmp {..}) = "icmp" <+> pp iPredicate <+> ppTyped operand0 <> "," <+> pp operand1

  pp c@(Call {..}) = ppCall c

  pp (GetElementPtr {..}) = "getelementptr" <+> bounds inBounds <+> commas (fmap ppTyped (address:indices))
    where
      bounds True = "inbounds"
      bounds False = empty

instance PP CallableOperand where
  pp (Left asm) = undefined
  pp (Right op) = pp op

instance PP Operand where
  pp (LocalReference _ nm) = local (pp nm)
  pp (ConstantOperand con) = pp con


instance PP C.Constant where
  pp (C.Int width val) = pp val
  pp (C.Float (F.Double val)) = text $ printf "%6.6e" val
  pp (C.Float (F.Single val)) = text $ printf "%6.6e" val
  pp (C.GlobalReference ty nm) = "@" <> pp nm

  pp (C.Array {..})
    | memberType == (IntegerType 8) = "c" <> (doubleQuotes $ hcat [ppIntAsChar val | C.Int _ val <- memberValues])
    | otherwise = brackets $ commas $ fmap ppTyped memberValues


  pp (C.GetElementPtr {..}) = "getelementptr" <+> bounds inBounds <+> commas (fmap ppTyped (address:indices))
    where
      bounds True = "inbounds"
      bounds False = empty

instance PP a => PP (Named a) where
  pp (nm := a) = "%" <> pp nm <+> "=" <+> pp a
  pp (Do a) = pp a

instance PP Module where
  pp (Module {..}) =
    let header = printf "; ModuleID = '%s'" moduleName in
    hlinecat (fromString header : (fmap pp moduleDefinitions))

instance PP (FP.FloatingPointPredicate) where
  pp op = case op of
   FP.False -> "false"
   FP.OEQ   -> "oeq"
   FP.OGT   -> "ogt"
   FP.OGE   -> "oge"
   FP.OLT   -> "olt"
   FP.OLE   -> "ole"
   FP.ONE   -> "one"
   FP.ORD   -> "ord"
   FP.UEQ   -> "ueq"
   FP.UGT   -> "ugt"
   FP.UGE   -> "uge"
   FP.ULT   -> "ult"
   FP.ULE   -> "ule"
   FP.UNE   -> "une"
   FP.UNO   -> "uno"
   FP.True  -> "true"

instance PP (IP.IntegerPredicate) where
  pp op = case op of
   IP.EQ  -> "eq"
   IP.NE  -> "ne"
   IP.UGT -> "ugt"
   IP.UGE -> "uge"
   IP.ULT -> "ult"
   IP.ULE -> "ule"
   IP.SGT -> "sgt"
   IP.SGE -> "sge"
   IP.SLT -> "slt"
   IP.SLE -> "sle"

-------------------------------------------------------------------------------
-- Special Case Hacks
-------------------------------------------------------------------------------

escape :: Char -> Doc
escape '"'  = "\\\""
escape '\\' = "\\\\"
escape c    = if isControl c
              then "\\" <> hex c
              else char c
    where
        hex :: Char -> Doc
        hex = pad0 . ($ []) . showHex . ord
        pad0 :: String -> Doc
        pad0 [] = "00"
        pad0 [x] = "0" <> char x
        pad0 xs = text xs

ppIntAsChar :: Integral a => a -> Doc
ppIntAsChar = escape . chr . fromIntegral


-- print an operand and its type
ppTyped :: (PP a, Typed a) => a -> Doc
ppTyped a = pp (typeOf a) <+> pp a

phiIncoming :: (Operand, Name) -> Doc
phiIncoming (op, nm) = brackets (pp op <> "," <+> (local (pp nm)))

ppParams :: (a -> Doc) -> ([a], Bool) -> Doc
ppParams ppParam (ps, varrg) = parens . commas $ fmap ppParam ps ++ vargs
    where
        vargs = if varrg then ["..."] else []

ppFunctionArgumentTypes :: Type -> Doc
ppFunctionArgumentTypes (FunctionType {..}) = ppParams pp (argumentTypes, isVarArg)

ppCall :: Instruction -> Doc
ppCall (Call { function = Right f,..})
  = tail <+> "call" <+> pp resultType <+> ftype <+> pp f <> parens (commas $ fmap pp arguments)
    where
      tail = if isTailCall then "tail" else empty
      (functionType@FunctionType {..}) = typeOf f
      ftype = if isVarArg || isFunctionPtr resultType
              then ppFunctionArgumentTypes functionType <> "*"
              else empty
ppCall x = error (show x)

ppSingleBlock :: BasicBlock -> Doc
ppSingleBlock (BasicBlock nm instrs term) = ((vcat $ (fmap pp instrs) ++ [pp term]))

ppllvm :: Module -> String
ppllvm = render . pp
