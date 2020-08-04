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
import MPLAST.MPLASTIdent 

import MPLAST.MPLASTTranslateType
import MPLUtil.UniqueSupply

import Data.Foldable
import Data.Functor.Foldable
import Data.Coerce

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.List

import Language.PrintMPL
import Language.AbsMPL as B

class PPrint a where
    pprint :: a -> String

instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (Type calldef ident typevar) where
    pprint = printTree . translateTypeToBnfcType

instance (PPrint ident, Eq typevar, PPrint typevar, PPrint chident) => PPrint (ExprG ident typevar chident) where
    pprint = printTree . translateExprGToBnfcExpr

instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (PatternG ident typevar) where
    pprint = printTree . translatePatternGtoBnfcPattern

instance (PPrint ident, Eq typevar, PPrint typevar, PPrint chident) => PPrint (FunctionDefG ident typevar chident) where
    pprint = printTree . translateFunctionGToBnfcFunction


instance PPrint BnfcIdent where
    pprint n = n ^. bnfcIdentName

instance PPrint TaggedChIdent where
    pprint n = n ^. bnfcIdentName   
        ++ "__" 
        ++ n ^. taggedChIdentPolarity % to show

instance PPrint TaggedBnfcIdent where
    pprint n = n ^. taggedBnfcIdentName ++ pprint (n ^. uniqueTag)

instance PPrint UniqueTag where
    pprint (UniqueTag n) = "__" ++ pprint n 

instance PPrint TypeTag where
    pprint (TypeTag (UniqueTag n)) = "T" ++ pprint n

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
    ( PPrint ident, Eq typevar, PPrint typevar, PPrint chident) =>
    ExprG ident typevar chident ->
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

translateExprGToBnfcExpr (ECall ident calldef args etype) = 
    B.TYPED_EXPR expr' etype'
  where
    expr' = B.FUN_EXPR 
        (toBnfcPIdent $ pprint ident)
        bnfcLBracket
        (map translateExprGToBnfcExpr args)
        bnfcRBracket
    f (patt, expr) = PATTERN_TO_EXPR 
        (map translatePatternGtoBnfcPattern [patt]) 
        (translateExprGToBnfcExpr expr)
    etype' = translateTypeToBnfcType etype
translateExprGToBnfcExpr (ERecord recordphrases etype) = 
    B.TYPED_EXPR expr' etype'
  where
    expr' = B.RECORD_EXPR bnfcLBracket recordphrases' bnfcRBracket
    recordphrases' = NE.toList $ fmap f recordphrases
      where
        f (ident, (calldef, ([],expr))) = 
            B.RECORD_EXPR_PHRASE (toBnfcUIdent $ pprint ident) (translateExprGToBnfcExpr expr)
        f (ident, (calldef, (patts,expr))) = 
            B.RECORD_EXPR_HIGHER_ORDER_PHRASE 
                (toBnfcUIdent $ pprint ident) 
                $ B.PATTERN_TO_EXPR
                    (map translatePatternGtoBnfcPattern patts)
                    (translateExprGToBnfcExpr expr)
        -- f (ident, (calldef, (_,expr))) = undefined
    etype' = translateTypeToBnfcType etype

translateExprGToBnfcExpr (EFold foldon phrases etype) = 
    B.TYPED_EXPR expr' etype'
  where
    expr' = B.FOLD_EXPR (translateExprGToBnfcExpr foldon) $ 
        map foldTranslate $ NE.toList phrases
      where
        foldTranslate (FoldPhraseF ident _ args t) = 
            B.FOLD_EXPR_PHRASE 
                (toBnfcUIdent $ pprint ident)
                bnfcColon
                (map translatePatternGtoBnfcPattern args)
                (translateExprGToBnfcExpr t)

    etype' = translateTypeToBnfcType etype

translateExprGToBnfcExpr (EUnfold foldon phrases etype) = 
    B.TYPED_EXPR expr' etype'
  where
    expr' = B.UNFOLD_EXPR (translateExprGToBnfcExpr foldon) $ 
        map f $ NE.toList phrases
      where
        f (UnfoldPhraseF patt foldphrases) = 
            B.UNFOLD_EXPR_PHRASE 
                (translatePatternGtoBnfcPattern patt)
                $ map foldTranslate $ NE.toList foldphrases

        -- duplicated code...
        foldTranslate (FoldPhraseF ident _ args t) = 
            B.FOLD_EXPR_PHRASE 
                (toBnfcUIdent $ pprint ident)
                bnfcColon
                (map translatePatternGtoBnfcPattern args)
                (translateExprGToBnfcExpr t)

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
translatePatternGtoBnfcPattern (PRecord pargs ptype) = 
    B.TYPED_PATTERN patt' ptype'
  where
    patt' = B.RECORD_PATTERN
        bnfcLBracket
        (NE.toList $ fmap toDestructorPatternPhrase pargs)
        bnfcRBracket
    ptype' = translateTypeToBnfcType ptype

    toDestructorPatternPhrase (ident, (calldef, patt)) =  
            (B.DESTRUCTOR_PATTERN_PHRASE 
                (toBnfcUIdent $ pprint ident) 
                (translatePatternGtoBnfcPattern patt)
                )
translatePatternGtoBnfcPattern (PVar ident ptype) = 
    B.TYPED_PATTERN patt' ptype'
  where
    patt' = VAR_PATTERN $ toBnfcPIdent $ pprint ident
    ptype' =  translateTypeToBnfcType ptype

translatePatternGtoBnfcPattern (PNull ident ptype) = 
    B.TYPED_PATTERN patt' ptype'
  where
    patt' = NULL_PATTERN $ NullPattern $ ((-1,-1), pprint ident)
    ptype' =  translateTypeToBnfcType ptype

translateFunctionGToBnfcFunction :: 
    ( PPrint ident, Eq typevar, PPrint typevar, PPrint chident) =>
    FunctionDefG ident typevar chident -> 
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

bnfcColon = Colon ((-1,-1), ":")

toBnfcUIdent str = UIdent ((-1,-1), str)
toBnfcPIdent str = PIdent ((-1,-1), str)
toBnfcUPIdent str = UPIdent ((-1,-1), str)

-- | Converts a TypeGTypeTag to a TypeG TaggedBnfcIdent.
-- The reason why this is in this module instead of say
-- ProgGraph is because the result (the generated bnfc ident)
-- is so closely linked to how it is printed and this module
-- takes care of all the pretty printing
typeGTypeTagToTypeG ::  
    TypeGTypeTag -> 
    TypeG TaggedBnfcIdent
typeGTypeTagToTypeG = cata f
  where
    f :: TypeF 
            (TypeClauseCallDefKnot TaggedBnfcIdent TaggedBnfcIdent) 
            TaggedBnfcIdent TypeTag (TypeG TaggedBnfcIdent) ->
        TypeG TaggedBnfcIdent  
    f (TypeWithArgsF ident def args) = 
        TypeWithArgs ident def args
    f (TypeVarF tag args) = 
        _TypeVar # (_TaggedBnfcIdent # (BnfcIdent (pprint tag, (-1,-1)), coerce tag), args)
    f (TypeSeqF n) = TypeSeq n
    f (TypeConcF n) = TypeConc n
