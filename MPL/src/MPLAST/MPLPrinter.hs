module MPLAST.MPLPrinter where

import Optics

import MPLAST.MPLTypeAST
import MPLAST.MPLProg
import MPLAST.MPLProgI
import MPLAST.MPLProgGraph

import MPLAST.MPLASTTranslateType

import Data.Foldable

import Language.PrintMPL
import Language.AbsMPL as B

class PPrint a where
    pprint :: a -> String

instance (PPrint ident, PPrint typevar) => PPrint (Type calldef ident typevar) where
    pprint = printTree . translateTypeToBnfcType

instance PPrint BnfcIdent where
    pprint n = n ^. bnfcIdentName

instance PPrint TaggedBnfcIdent where
    pprint n = n ^. taggedBnfcIdentName ++ pprint (n ^. uniqueTag)

instance PPrint UniqueTag where
    pprint (UniqueTag n) = "__" ++ show n 

instance PPrint TypeTag where
    pprint (TypeTag (UniqueTag n)) = show n

instance (PPrint a, PPrint b) => PPrint (a,b) where
    pprint (a,b) = "(" ++ pprint a ++ ", " ++ pprint b ++ ")"

translateTypeToBnfcType :: 
    ( PPrint ident, PPrint typevar) =>
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
                (map (MPL_SEQ_FUN_TYPE_FORALL_LIST . toBnfcUIdent) 
                    (concatMap (map pprint . toList) from ++ map pprint (toList to)))
                (map translateTypeToBnfcType from) 
                (translateTypeToBnfcType to)
    TypeConc n -> case n of
        TypeGetF ident larg rarg  -> error "concurrent translationsnot implemented yet"
        _  -> error "concurrent translationsnot implemented yet" 

bnfcLBracket = LBracket ((-1,-1), "(")
bnfcRBracket = RBracket ((-1,-1), ")")

bnfcLSquareBracket = LSquareBracket ((-1,-1), "[")
bnfcRSquareBracket = RSquareBracket ((-1,-1), "]")

toBnfcUIdent str = UIdent ((-1,-1), str)
toBnfcUPIdent str = UPIdent ((-1,-1), str)
