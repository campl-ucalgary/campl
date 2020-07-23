{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MPLAST.MPLPrinter where

import Optics

import MPLAST.MPLTypeAST
import MPLAST.MPLExprAST
import MPLAST.MPLPatternAST
import MPLAST.MPLProg
import MPLAST.MPLProgI
import MPLAST.MPLProgGraph

import MPLAST.MPLASTTranslateType
import MPLUtil.UniqueSupply

import Data.Foldable

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.List

import Language.PrintMPL
import Language.AbsMPL as B

class PPrint a where
    pprint :: a -> String

instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (Type calldef ident typevar) where
    pprint = printTree . translateTypeToBnfcType

instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (ExprG ident typevar) where
    pprint = printTree . translateExprGToBnfcExpr

instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (PatternG ident typevar) where
    pprint = printTree . translatePatternGtoBnfcPattern

instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (FunctionDefG ident typevar) where
    pprint = printTree . translateFunctionGToBnfcFunction


instance PPrint BnfcIdent where
    pprint n = n ^. bnfcIdentName

instance PPrint TaggedBnfcIdent where
    pprint n = n ^. taggedBnfcIdentName ++ pprint (n ^. uniqueTag)

instance PPrint UniqueTag where
    pprint (UniqueTag n) = "__" ++ pprint n 

instance PPrint TypeTag where
    pprint (TypeTag (UniqueTag n)) = pprint n

instance PPrint Unique where
    pprint (Unique n) = show n

instance (PPrint a, PPrint b) => PPrint (a,b) where
    pprint (a,b) = "(" ++ pprint a ++ ", " ++ pprint b ++ ")"

translateTypeToBnfcType :: 
    ( Eq typevar, PPrint ident, PPrint typevar) =>
    Type calldef ident typevar -> 
    MplType
translateTypeToBnfcType n = case n of
    TypeWithArgs ident _ [] -> 
        MPL_UIDENT_NO_ARGS_TYPE (toBnfcUIdent $ pprint ident) 
    TypeVar ident [] -> MPL_UIDENT_NO_ARGS_TYPE (toBnfcUIdent $ pprint ident) 

    TypeWithArgs ident _ args -> 
        let args' = map translateTypeToBnfcType args
        in MPL_UIDENT_ARGS_TYPE (toBnfcUIdent $ pprint ident) bnfcLBracket args' bnfcRBracket
    TypeVar ident args -> 
        let args' = map translateTypeToBnfcType args
        in MPL_UIDENT_ARGS_TYPE (toBnfcUIdent $ pprint ident) bnfcLBracket args' bnfcRBracket

    TypeSeq n -> case n of
        TypeIntF ident -> 
            MPL_UIDENT_NO_ARGS_TYPE (toBnfcUIdent $ pprint ident) 
        TypeCharF ident -> 
            MPL_UIDENT_NO_ARGS_TYPE (toBnfcUIdent $ pprint ident) 
        TypeDoubleF ident -> 
            MPL_UIDENT_NO_ARGS_TYPE (toBnfcUIdent $ pprint ident) 
        TypeStringF ident -> 
            MPL_UIDENT_NO_ARGS_TYPE (toBnfcUIdent $ pprint ident) 
        TypeUnitF ident -> 
            MPL_UIDENT_NO_ARGS_TYPE (toBnfcUIdent $ pprint ident) 
        TypeBoolF -> 
            MPL_UIDENT_NO_ARGS_TYPE (toBnfcUIdent $ "Bool") 
        TypeListF t -> 
            MPL_LIST_TYPE bnfcLSquareBracket (translateTypeToBnfcType t) bnfcRSquareBracket
        TypeSeqArrF from to -> 
            MPL_SEQ_ARROW_TYPE 
                (nub $ map (MPL_SEQ_FUN_TYPE_FORALL_LIST . toBnfcUIdent) 
                    (concatMap (map pprint . toList) from ++ map pprint (toList to)))
                (map translateTypeToBnfcType from) 
                (translateTypeToBnfcType to)
    TypeConc n -> case n of
        TypeGetF ident larg rarg  -> error "concurrent translationsnot implemented yet"
        _  -> error "concurrent translationsnot implemented yet" 

translateExprGToBnfcExpr ::
    ( PPrint ident, Eq typevar, PPrint typevar) =>
    ExprG ident typevar ->
    B.Expr
translateExprGToBnfcExpr (EConstructorDestructor ident calldef args etype) = 
    B.TYPED_EXPR expr' etype'
  where
    expr' = B.DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR 
            (toBnfcUIdent $ pprint ident) 
            bnfcLBracket
            (map translateExprGToBnfcExpr args)
            bnfcRBracket
    etype' = translateTypeToBnfcType etype

translateExprGToBnfcExpr (EVar ident etype) = 
    B.TYPED_EXPR expr' etype'
  where
    expr' = VAR_EXPR $ toBnfcPIdent $ pprint ident
    etype' = translateTypeToBnfcType etype

translateExprGToBnfcExpr (ECase ecaseon ecases etype) = 
    B.TYPED_EXPR expr' etype'
  where
    expr' = CASE_EXPR (translateExprGToBnfcExpr ecaseon) (map f $ NE.toList ecases)
    f (patt, expr) = PATTERN_TO_EXPR 
        (map translatePatternGtoBnfcPattern [patt]) 
        (translateExprGToBnfcExpr expr)
    etype' = translateTypeToBnfcType etype

translatePatternGtoBnfcPattern ::
    ( PPrint ident, Eq typevar, PPrint typevar) =>
    PatternG ident typevar -> 
    B.Pattern
translatePatternGtoBnfcPattern (PConstructor ident calldef pargs ptype) = 
    B.TYPED_PATTERN patt' ptype'
  where
    patt' = B.CONSTRUCTOR_PATTERN_ARGS 
        (toBnfcUIdent $ pprint ident)
        bnfcLBracket
        (map translatePatternGtoBnfcPattern pargs)
        bnfcRBracket
    ptype' = translateTypeToBnfcType ptype
translatePatternGtoBnfcPattern (PVar ident ptype) = 
    B.TYPED_PATTERN patt' ptype'
  where
    patt' = VAR_PATTERN $ toBnfcPIdent $ pprint ident
    ptype' =  translateTypeToBnfcType ptype

translateFunctionGToBnfcFunction :: 
    ( PPrint ident, Eq typevar, PPrint typevar) =>
    FunctionDefG ident typevar -> 
    B.FunctionDefn
translateFunctionGToBnfcFunction (FunctionDefn funname funtype fundefn) = 
    INTERNAL_TYPED_FUNCTION_DEFN 
        (toBnfcPIdent $ pprint funname)
        (translateTypeToBnfcType funtype)
        (map f $ NE.toList fundefn)
  where
    f (patts, expr) = B.PATTERN_TO_EXPR 
        (map translatePatternGtoBnfcPattern patts)
        (translateExprGToBnfcExpr expr)


bnfcLBracket = LBracket ((-1,-1), "(")
bnfcRBracket = RBracket ((-1,-1), ")")

bnfcLSquareBracket = LSquareBracket ((-1,-1), "[")
bnfcRSquareBracket = RSquareBracket ((-1,-1), "]")

toBnfcUIdent str = UIdent ((-1,-1), str)
toBnfcPIdent str = PIdent ((-1,-1), str)
toBnfcUPIdent str = UPIdent ((-1,-1), str)
