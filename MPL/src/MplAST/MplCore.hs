{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module MplAST.MplCore (
    module MplAST.MplExpr
    , module MplAST.MplPattern
    , module MplAST.MplCmd
    , module MplAST.MplProg
    , module MplAST.MplType
    , module MplAST.MplPrinter
    , module MplAST.MplIdent 
    , module MplAST.MplExt 
    , module MplAST.MplProgUtil 
    , module MplAST.MplParsed 
    , module MplAST.MplRenamed 
    , module MplAST.MplKind 
    , typeLocationSpan
    ) where
{- Module for rexporting things related to the AST..
 -
 -}

import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplCmd
import MplAST.MplProg
import MplAST.MplType
import MplAST.MplPrinter
import MplAST.MplIdent 
import MplAST.MplExt 
import MplAST.MplProgUtil 
import MplAST.MplParsed 
import MplAST.MplRenamed 
import MplAST.MplKind 

import Control.Arrow
import Optics

import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Functor.Foldable hiding (fold)
import Data.Foldable


{- | This will compute the span of a MplParsed type. -}
typeLocationSpan :: 
    forall x. 
    ( HasLocation (TypeP x)
    , HasLocation (IdP x)
    -- , (XTypeVar x)
    -- , (XTypeWithNoArgs x)
    -- , (XTypeSeqWithArgs x)
    -- , (XTypeSeqVarWithArgs x)
    -- , (XTypeConcWithArgs x)
    -- , (XTypeConcVarWithArgs  x)
    , HasLocation (XTypeIntF x)
    , HasLocation (XTypeCharF x)
    , HasLocation (XTypeDoubleF x)
    , HasLocation (XTypeStringF x)
    , HasLocation (XTypeUnitF x)
    , HasLocation (XTypeBoolF x)
    , HasLocation (XTypeListF x)
    , HasLocation (XTypeTupleF x)

    , HasLocation (XTypeGet x)
    , HasLocation (XTypePut x)
    , HasLocation (XTypeTensor x)
    , HasLocation (XTypePar x)
    , HasLocation (XTypeTopBot x)
    , HasLocation (XTypeNeg x)
    ) =>
    {-
    , c (XXType x)

    , c (XTypeIntF x)
    , c (XTypeCharF x)
    , c (XTypeDoubleF x)
    , c (XTypeStringF x)
    , c (XTypeUnitF x)
    , c (XTypeBoolF x)
    , c (XTypeListF x)
    , c (XTypeTupleF x)
    , c (XTypeSeqWithArgs x)
    , c (XTypeSeqVar x)

    , c (XTypeGet x)
    , c (XTypePut x)
    , c (XTypeTensor x)
    , c (XTypePar x)
    , c (XTypeTopBot x)
    , c (XTypeNeg x)

    , c (XTypeSeqArrF x)
    , c (XTypeConcArrF x)
    
    , c (MplBuiltInTypesF x (MplType x))
    -}
    
    MplType x -> Span 
typeLocationSpan = cata go
  where
    go :: MplTypeF x Span -> Span
    go = \case
        TypeVarF _ann typep -> locationToSpan typep
        TypeWithNoArgsF _ann idp -> locationToSpan idp

        TypeSeqWithArgsF _ann idp rst -> sconcat $ locationToSpan idp :| rst 
        TypeSeqVarWithArgsF _ann typep rst -> sconcat $ locationToSpan typep :| rst 

        TypeConcWithArgsF _ann idp (ls, rs) ->
            sconcat $ locationToSpan idp :| ls ++ rs
        TypeConcVarWithArgsF _ann typep (ls, rs) ->
            sconcat $ locationToSpan typep :| ls ++ rs

        TypeBuiltInF ty -> case ty of
            TypeIntF ann -> locationToSpan ann
            TypeCharF ann -> locationToSpan ann
            TypeDoubleF ann -> locationToSpan ann
            TypeGetF ann l r -> sconcat $ locationToSpan ann :| [l, r]
            TypePutF ann l r -> sconcat $ locationToSpan ann :| [l, r]
            TypeTensorF ann l r -> sconcat $ locationToSpan ann :| [l, r]
            TypeParF ann l r -> sconcat $ locationToSpan ann :| [l, r]
            TypeNegF ann l -> sconcat $ locationToSpan ann :| [l]
            TypeTopBotF ann -> locationToSpan ann

            TypeStringF ann -> locationToSpan ann
            TypeUnitF ann -> locationToSpan ann
            TypeBoolF ann -> locationToSpan ann
            TypeListF ann rst -> locationToSpan ann <> rst

            TypeSeqArrF _ann dom codom -> sconcat $ NE.cons codom dom
            TypeConcArrF _ann seqs ins outs -> fold $ concat [seqs, ins, outs]

            TypeTupleF ann (a,b,c)-> sconcat $ locationToSpan ann :| a : b : c

    locationToSpan :: HasLocation l => l -> Span
    locationToSpan = view (location % to (Span <<< id &&& id))

