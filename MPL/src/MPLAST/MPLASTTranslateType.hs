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

import Optics

import Data.Function
import qualified Data.Bifunctor as Bifunctor
import Control.Monad

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 
import Data.Maybe
import Data.Coerce

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

{-
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
    Either (NonEmpty e) SeqTypeClauseI
translateSeqTypeClauseArgs err from to handles = do
        from' <- translateBnfcTypeToType from
        to' <- translateBnfcTypeToType to

        handles' <- map translateBnfcSeqTypePhrasesToSeqTypePhrase handles ^. collectsOnlyIfNoLeftsGetter
        case from' of
            TypeWithNoArgs statevar -> 
                case to' of
                    TypeWithNoArgs name ->  
                        return $ review _SeqTypeClause 
                            ( coerce statevar 
                            , []
                            , name 
                            , concat handles'
                            )
                    TypeWithArgs name args -> do
                        args' <- translateTypeDeclarationArgs name (NE.toList args)
                        return $ review _SeqTypeClause 
                            ( statevar 
                            , args'
                            , name 
                            , concat handles'
                            )

                    _ -> Left $ review err (from',to') :| []
            _ -> Left $ review err (from',to') :| []
-}

getTypeDeclarationName ::
    forall e.
    AsTranslateBnfcErrors e => 
    TypeI ->
    Either (NonEmpty e) (BnfcIdent, [BnfcIdent])
getTypeDeclarationName n = 
    case n of
        TypeWithNoArgs name -> return (name, [])
        TypeWithArgs name args -> do
            args' <- translateTypeDeclarationArgs (NE.toList args)
            return (name, args')
        n -> Left $ ((review _IllegalTypeName n) :|[] )


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
    Either (NonEmpty e) ConcTypeClauseI
translateConcTypeClauseArgs err from to handles = do
        from' <- translateBnfcTypeToType from
        to' <- translateBnfcTypeToType to

        handles' <- map translateConcurrentTypePhraseToConcTypePhrase handles ^. collectsOnlyIfNoLeftsGetter
        case from' of
            TypeWithNoArgs statevar -> 
                case to' of
                    TypeWithNoArgs name ->  
                        return $ review _ConcTypeClause 
                            ( statevar 
                            , []
                            , name 
                            , concat handles'
                            )
                    TypeWithArgs name args -> do
                        args' <- translateTypeDeclarationArgs (NE.toList args)
                        return $ review _ConcTypeClause 
                            (  statevar 
                            , args'
                            , name 
                            , concat handles'
                            )
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
    [Type BnfcIdent BnfcIdent] -> 
        -- ^ argument
    Either (NonEmpty e) [BnfcIdent]
translateTypeDeclarationArgs args = 
    map getTypeVar args ^. collectsOnlyIfNoLeftsGetter

translateBnfcSeqTypePhrasesToDataPhrase ::
    forall e.
    AsTranslateBnfcErrors e => 
    SeqTypePhraseDefn -> 
    Either (NonEmpty e) ([DataTypePhraseI])
translateBnfcSeqTypePhrasesToDataPhrase (SEQ_TYPE_PHRASE handles fromtypes totype) = 
    map f handles ^. collectsOnlyIfNoLeftsGetter
  where
    f :: TypeHandleName -> Either (NonEmpty e) DataTypePhraseI
    f (TYPE_HANDLE_NAME name) = do
        fromtypes' <- map translateBnfcTypeToType fromtypes ^. collectsOnlyIfNoLeftsGetter
        totype' <- translateBnfcTypeToType totype >>= getTypeVar 

        return $ review _TypePhrase
            ( name ^. uIdentBnfcIdentGetter
            , review _DataPhrase (fromtypes' , totype')
            )
        -- can apply fusion law..


translateBnfcSeqTypePhrasesToSeqTypePhrase ::
    forall e.
    AsTranslateBnfcErrors e => 
    SeqTypePhraseDefn -> 
    Either (NonEmpty e) ([SeqTypePhraseI])
translateBnfcSeqTypePhrasesToSeqTypePhrase (SEQ_TYPE_PHRASE handles fromtypes totype) = 
    map f handles ^. collectsOnlyIfNoLeftsGetter
  where
    f :: TypeHandleName -> Either (NonEmpty e) SeqTypePhraseI
    f (TYPE_HANDLE_NAME name) = do
        fromtypes' <- map translateBnfcTypeToType fromtypes ^. collectsOnlyIfNoLeftsGetter
        totype' <- translateBnfcTypeToType totype >>= getTypeVar 

        return $ review _SeqTypePhrase 
            ( name ^. uIdentBnfcIdentGetter
            , fromtypes'
            , totype')
        -- can apply fusion law..

translateConcurrentTypePhraseToConcTypePhrase ::
    forall e. 
    AsTranslateBnfcErrors e => 
    ConcurrentTypePhraseDefn ->
    Either (NonEmpty e) [ConcTypePhraseI]
translateConcurrentTypePhraseToConcTypePhrase (CONCURRENT_TYPE_PHRASE handles a b)  = 
    view collectsOnlyIfNoLeftsGetter $ map f handles 
  where
    f (TYPE_HANDLE_NAME name) = do
        a' <- translateBnfcTypeToType a 
        b' <- translateBnfcTypeToType b >>= getTypeVar 

        return $ review _ConcTypePhrase 
            (  name ^. uIdentBnfcIdentGetter
            , a'
            , b'
            )
        -- can apply fusion law..

translateBnfcTypeToType :: 
    forall e. 
    AsTranslateBnfcErrors e => 
    MplType -> 
    Either (NonEmpty e) (Type BnfcIdent BnfcIdent)

translateBnfcTypeToType (MPL_TYPE n) = translateBnfcTypeToType n

translateBnfcTypeToType (PAR_TYPE a (Par p) b) = 
    review (_TypeConc % _TypeParF) 
        <$> ( (BnfcIdent (p ^. swapped),, ) 
            <$> translateBnfcTypeToType a 
            <*> translateBnfcTypeToType b )
translateBnfcTypeToType (TENSOR_TYPE a (Tensor p) b) = 
    review (_TypeConc % _TypeTensorF) 
        <$> ( (BnfcIdent (p ^. swapped),, ) 
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
    getputname = stringPos getput' ^. _1
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
