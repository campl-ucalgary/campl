{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module MPLAST.MPLASTTranslateType where

import Optics.TH
import Optics.Prism
import Optics.Operators
import Optics.Optic
import Optics.Getter
import Optics.Setter
import Optics.Review
import Optics.Prism
import Optics.Fold
import Optics.Empty
import Optics.AffineFold
import Data.Either.Optics
import Optics.Iso
import Data.Tuple.Optics

import Data.Function
import qualified Data.Bifunctor as Bifunctor
import Control.Monad

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 
import Data.Maybe

import MPLAST.MPLTypeAST
import MPLAST.MPLPatternAST
import MPLAST.MPLExprAST
import MPLAST.MPLProcessCommandsAST
import MPLAST.MPLProg
import MPLAST.MPLProgI

import MPLUtil.Data.Either

import MPLAST.MPLASTTranslateErrors

import Language.AbsMPL as B

import Data.Data
import Data.Typeable
import Data.Either
import Data.Semigroup
import Control.Arrow

import Text.PrettyPrint.GenericPretty

translateSeqTypeClauseArgs ::
    forall e.
    AsTranslateBnfcErrors e => 
    Prism' e (Type BnfcIdent BnfcIdent, Type BnfcIdent BnfcIdent) ->
    MplType ->
    -- ^from (stsatevar)
    MplType ->
    -- ^to (name)
    [SeqTypePhraseDefn] ->
    -- ^ handles
    Either (NonEmpty e) (SeqTypeClause BnfcIdent BnfcIdent)
translateSeqTypeClauseArgs err from to handles = do
        from' <- translateBnfcTypeToType from
        to' <- translateBnfcTypeToType to

        handles' <- map translateBnfcSeqTypePhrasesToSeqTypePhrase handles ^. collectsOnlyIfNoLeftsGetter
        case from' of
            TypeWithNoArgs statevar -> 
                case to' of
                    TypeWithNoArgs name ->  
                        return $ SeqTypeClause {
                                _seqTypeClauseName = statevar 
                                , _seqTypeClauseArgs = []
                                , _seqTypeClauseStateVar = name 
                                , _seqTypePhrases = concat handles'
                            }
                    TypeWithArgs name args -> do
                        args' <- translateTypeDeclarationArgs name (NE.toList args)
                        return $ SeqTypeClause {
                                _seqTypeClauseName = statevar 
                                , _seqTypeClauseArgs = args'
                                , _seqTypeClauseStateVar = name 
                                , _seqTypePhrases = concat handles'
                            }
                    _ -> Left $ review err (from',to') :| []
            _ -> Left $ review err (from',to') :| []

translateConcTypeClauseArgs ::
    forall e.
    AsTranslateBnfcErrors e => 
    Prism' e (Type BnfcIdent BnfcIdent, Type BnfcIdent BnfcIdent) ->
    MplType ->
    -- ^from (stsatevar)
    MplType ->
    -- ^to (name)
    [ConcurrentTypePhraseDefn] ->
    -- ^ handles
    Either (NonEmpty e) (ConcTypeClause BnfcIdent BnfcIdent)
translateConcTypeClauseArgs err from to handles = do
        from' <- translateBnfcTypeToType from
        to' <- translateBnfcTypeToType to

        handles' <- map translateConcurrentTypePhraseToConcTypePhrase handles ^. collectsOnlyIfNoLeftsGetter
        case from' of
            TypeWithNoArgs statevar -> 
                case to' of
                    TypeWithNoArgs name ->  
                        return $ ConcTypeClause {
                                _concTypeClauseName = statevar 
                                , _concTypeClauseArgs = []
                                , _concTypeClauseStateVar = name 
                                , _concTypePhrases = concat handles'
                            }
                    TypeWithArgs name args -> do
                        args' <- translateTypeDeclarationArgs name (NE.toList args)
                        return $ ConcTypeClause {
                                _concTypeClauseName = statevar 
                                , _concTypeClauseArgs = args'
                                , _concTypeClauseStateVar = name 
                                , _concTypePhrases = concat handles'
                            }
                    _ -> Left $ review err (from',to') :| []
            _ -> Left $ review err (from',to') :| []


getTypeVar ::
    forall e .
    AsTranslateBnfcErrors e => 
    Type BnfcIdent BnfcIdent ->
    Either (NonEmpty e) BnfcIdent
getTypeVar (TypeVar n) = Right n
getTypeVar (TypeWithNoArgs n) = Right n
getTypeVar n = Left (review _IllegalNonTypeVar n :| [])

translateTypeDeclarationArgs ::
    forall e .
    AsTranslateBnfcErrors e => 
    BnfcIdent -> 
        -- ^ focused type
    [Type BnfcIdent BnfcIdent] -> 
        -- ^ argument
    Either (NonEmpty e) [BnfcIdent]
translateTypeDeclarationArgs focus args = 
    map getTypeVar args ^. collectsOnlyIfNoLeftsGetter


translateBnfcSeqTypePhrasesToSeqTypePhrase ::
    forall e.
    AsTranslateBnfcErrors e => 
    SeqTypePhraseDefn -> 
    Either (NonEmpty e) ([SeqTypePhrase BnfcIdent BnfcIdent])
translateBnfcSeqTypePhrasesToSeqTypePhrase (SEQ_TYPE_PHRASE handles fromtypes totype) = 
    map f handles ^. collectsOnlyIfNoLeftsGetter
  where
    f :: TypeHandleName -> Either (NonEmpty e) (SeqTypePhrase BnfcIdent BnfcIdent)
    f (TYPE_HANDLE_NAME name) = do
        fromtypes' <- map translateBnfcTypeToType fromtypes ^. collectsOnlyIfNoLeftsGetter
        totype' <- translateBnfcTypeToType totype >>= getTypeVar 

        return $ SeqTypePhrase {
            _seqTypePhraseName = name ^. uIdentBnfcIdentGetter
            , _seqTypePhraseFrom = fromtypes'
            , _seqTypePhraseTo = totype'
        }
        -- can apply fusion law..

translateConcurrentTypePhraseToConcTypePhrase ::
    forall e. 
    AsTranslateBnfcErrors e => 
    ConcurrentTypePhraseDefn ->
    Either (NonEmpty e) [ConcTypePhrase BnfcIdent BnfcIdent]
translateConcurrentTypePhraseToConcTypePhrase (CONCURRENT_TYPE_PHRASE handles a b)  = 
    view collectsOnlyIfNoLeftsGetter $ map f handles 
  where
    f (TYPE_HANDLE_NAME name) = do
        a' <- translateBnfcTypeToType a >>= getTypeVar
        b' <- translateBnfcTypeToType b >>= getTypeVar 

        return $ ConcTypePhrase {
            _concTypePhraseName = name ^. uIdentBnfcIdentGetter
            , _concTypePhraseFrom = a'
            , _concTypePhraseTo = b'
        }
        -- can apply fusion law..

translateBnfcTypeToType :: 
    forall e. 
    AsTranslateBnfcErrors e => 
    MplType -> 
    Either (NonEmpty e) (Type BnfcIdent BnfcIdent)

translateBnfcTypeToType (MPL_TYPE n) = translateBnfcTypeToType n

translateBnfcTypeToType (PAR_TYPE a (Par p) b) = 
    review (_TypeConc % _TypeParF) 
        <$> ( (p ^. swapped,, ) 
            <$> translateBnfcTypeToType a 
            <*> translateBnfcTypeToType b )
translateBnfcTypeToType (TENSOR_TYPE a (Tensor p) b) = 
    review (_TypeConc % _TypeTensorF) 
        <$> ( (p ^. swapped,, ) 
            <$> translateBnfcTypeToType a 
            <*> translateBnfcTypeToType b )

translateBnfcTypeToType (GETPUT_TYPE getput _ a b _) = maybe 
    illegalgetput f (preview _InternalConcTypeParser getputname)
  where
    f :: InternalConcTypes -> Either (NonEmpty e) (Type BnfcIdent BnfcIdent)
    f InternalGet = review (_TypeConc % _TypeGetF) <$> args
    f InternalPut = review (_TypeConc % _TypePutF) <$> args
    f _ = illegalgetput

    getput' = getput ^. uIdentBnfcIdentGetter
    getputname = getput' ^. _1
    illegalgetput = Left $ review _IllegalGetPut getput' :| []

    args = (getput',,) <$> seqarg <*> concarg
    seqarg = translateBnfcTypeToType a
    concarg = translateBnfcTypeToType b

translateBnfcTypeToType (MPL_UIDENT_NO_ARGS_TYPE ident) = 
    review (_Right % _TypeWithNoArgs ) (ident ^. uIdentBnfcIdentGetter)
translateBnfcTypeToType (MPL_UIDENT_ARGS_TYPE ident _ [] _ ) = 
    review (_Right % _TypeWithNoArgs ) (ident ^. uIdentBnfcIdentGetter)
translateBnfcTypeToType (MPL_UIDENT_ARGS_TYPE ident _ lst _) =
    review _TypeWithArgs
        <$> ( (ident ^. uIdentBnfcIdentGetter,) . NE.fromList <$> res ^. collectsOnlyIfNoLeftsGetter )
  where
    res = map translateBnfcTypeToType lst

translateBnfcTypeToType (MPL_UNIT_TYPE ident _) =
    review (_Right % _TypeSeq % _TypeUnitF) (ident ^. lBracketBnfcIdentGetter)

translateBnfcTypeToType (MPL_BRACKETED_TYPE _ ty _) =
    translateBnfcTypeToType ty

translateBnfcTypeToType (MPL_LIST_TYPE _ ty _) =
    review (_TypeSeq % _TypeListF) <$> translateBnfcTypeToType ty

translateBnfcTypeToType (MPL_TUPLE_TYPE _ ty tys _) = 
    review (_TypeSeq % _TypeTupleF) <$> tupleargs
  where
    tupleargs = (,) <$> a <*> as 
    a = translateBnfcTypeToType ty
    as = NE.fromList <$> ((map f tys) ^. collectsOnlyIfNoLeftsGetter)
    f (TUPLE_LIST_TYPE n) = translateBnfcTypeToType n
