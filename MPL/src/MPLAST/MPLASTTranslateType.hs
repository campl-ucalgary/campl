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
import MPLAST.MPLASTIdent 
import MPLAST.MPLPatternAST
import MPLAST.MPLExprAST
import MPLAST.MPLProcessCommandsAST
import MPLAST.MPLProg
import MPLAST.MPLProgI

import MPLUtil.Data.Either
import MPLUtil.Data.Either.AccumEither

import MPLAST.MPLASTTranslateErrors

import Language.AbsMPL as B

import Data.Data
import Data.Typeable
import Data.Either
import Data.Semigroup
import Control.Arrow

import Text.PrettyPrint.GenericPretty

getTypeDeclarationName ::
    forall e.
    AsTranslateBnfcErrors e => 
    TypeIBnfc ->
    Either (NonEmpty e) (BnfcIdent, [BnfcIdent])
getTypeDeclarationName n = 
    case n of
        TypeVar name args -> do
            args' <- translateTypeDeclarationArgs args
            return (name, args')
        TypeWithArgs name () args -> do
            args' <- translateTypeDeclarationArgs args
            return (name, args')
        n -> Left $ ((review _IllegalTypeName n) :|[] )

getTypeVar ::
    forall e .
    AsTranslateBnfcErrors e => 
    TypeIBnfc ->
    Either (NonEmpty e) BnfcIdent
getTypeVar (TypeVar n []) = Right n
getTypeVar (TypeWithArgs n () []) = Right n
getTypeVar n = Left (review _IllegalNonTypeVar n :| [])

translateTypeDeclarationArgs ::
    forall e .
    AsTranslateBnfcErrors e => 
    -- |argument
    [TypeIBnfc] -> 
    Either (NonEmpty e) [BnfcIdent]
translateTypeDeclarationArgs args = 
    map getTypeVar args ^. collectsOnlyIfNoLeftsGetter

translateBnfcSeqTypePhrasesToDataPhrase ::
    forall e.
    AsTranslateBnfcErrors e => 
    SeqTypePhraseDefn -> 
    Either (NonEmpty e) ([DataTypePhraseIBnfc])
translateBnfcSeqTypePhrasesToDataPhrase (SEQ_TYPE_PHRASE handles fromtypes totype) = 
    map f handles ^. collectsOnlyIfNoLeftsGetter
  where
    f :: TypeHandleName -> Either (NonEmpty e) DataTypePhraseIBnfc
    f (TYPE_HANDLE_NAME name) = do
        fromtypes' <- map translateBnfcTypeToType fromtypes ^. collectsOnlyIfNoLeftsGetter
        totype' <- translateBnfcTypeToType totype >>= getTypeVar 

        return $ review _TypePhrase
            ( ()
            , name ^. uIdentBnfcIdentGetter
            , fromtypes' 
            , TypeVar totype' []
            )

-- CODATA
translateBnfcSeqTypePhrasesToCodataPhrase ::
    forall e.
    AsTranslateBnfcErrors e => 
    SeqTypePhraseDefn -> 
    Either (NonEmpty e) ([CodataTypePhraseIBnfc])
translateBnfcSeqTypePhrasesToCodataPhrase (SEQ_TYPE_PHRASE handles fromtypes totype) = 
    map f handles ^. collectsOnlyIfNoLeftsGetter
  where
    f :: TypeHandleName -> Either (NonEmpty e) CodataTypePhraseIBnfc
    f (TYPE_HANDLE_NAME name) = do
        fromtypes' <- map translateBnfcTypeToType fromtypes 
                        ^.  collectsOnlyIfNoLeftsGetter
        totype' <- translateBnfcTypeToType totype 

        return $ review _TypePhrase
            ( ()
            , name ^. uIdentBnfcIdentGetter
            , fromtypes' 
            , totype'
            )

--  PROTOCOL
translateConcurrentTypePhraseToProtocolPhrase ::
    forall e. 
    AsTranslateBnfcErrors e => 
    ConcurrentTypePhraseDefn ->
    Either (NonEmpty e) [ProtocolTypePhraseIBnfc]
translateConcurrentTypePhraseToProtocolPhrase (CONCURRENT_TYPE_PHRASE handles a b)  = 
    view collectsOnlyIfNoLeftsGetter $ map f handles 
  where
    f (TYPE_HANDLE_NAME name) = do
        a' <- translateBnfcTypeToType a 
        b' <- translateBnfcTypeToType b >>= getTypeVar 

        return $ review _TypePhrase 
            ( ()
            , name ^. uIdentBnfcIdentGetter
            , [a']
            , TypeVar b' []
            )

-- Coprotocol
translateConcurrentTypePhraseToCoprotocolPhrase ::
    forall e. 
    AsTranslateBnfcErrors e => 
    ConcurrentTypePhraseDefn ->
    Either (NonEmpty e) [CoprotocolTypePhraseIBnfc]
translateConcurrentTypePhraseToCoprotocolPhrase (CONCURRENT_TYPE_PHRASE handles a b)  = 
    view collectsOnlyIfNoLeftsGetter $ map f handles 
  where
    f (TYPE_HANDLE_NAME name) = do
        a' <- translateBnfcTypeToType a >>= getTypeVar
        b' <- translateBnfcTypeToType b 

        return $ review _TypePhrase 
            ( ()
            , name ^. uIdentBnfcIdentGetter
            , [TypeVar a' []]
            , b'
            )

translateNameAndStateVar :: 
    AsTranslateBnfcErrors e =>
    MplType -> 
    MplType -> 
    Either (NonEmpty e) ((BnfcIdent, [BnfcIdent]), BnfcIdent)
translateNameAndStateVar from to = do
    (from', to') <- runAccumEither $ (,) 
        <$> liftAEither (translateBnfcTypeToType from) 
        <*> liftAEither (translateBnfcTypeToType to)
    (name, statevar) <- runAccumEither $ (,) 
        <$> liftAEither (getTypeDeclarationName from') 
        <*> liftAEither (getTypeVar to')
    return (name, statevar)

translateStateVarAndName :: 
    AsTranslateBnfcErrors e =>
    MplType -> 
    MplType -> 
    Either (NonEmpty e) (BnfcIdent, (BnfcIdent, [BnfcIdent]))
translateStateVarAndName from to = do
    (from', to') <- runAccumEither $ (,) 
        <$> liftAEither (translateBnfcTypeToType from) 
        <*> liftAEither (translateBnfcTypeToType to)
    (statevar, name) <- runAccumEither $ (,) 
        <$> liftAEither (getTypeVar from') 
        <*> liftAEither (getTypeDeclarationName to')
    return (statevar, name)

    

translateBnfcTypeToType :: 
    forall e. 
    AsTranslateBnfcErrors e => 
    MplType -> 
    Either (NonEmpty e) TypeIBnfc

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
    f :: InternalConcTypes -> Either (NonEmpty e) TypeIBnfc
    f InternalGet = review (_TypeConc % _TypeGetF) <$> args
    f InternalPut = review (_TypeConc % _TypePutF) <$> args
    f _ = illegalgetput

    getput' = getput ^. uIdentBnfcIdentGetter
    getputname = getput' ^. stringPos %_1 
    illegalgetput = Left $ review _IllegalGetPut getput' :| []

    args = (getput',,) <$> seqarg <*> concarg
    seqarg = translateBnfcTypeToType a
    concarg = translateBnfcTypeToType b

-- WE TRANSLATE THE TYPES TO THE INTERNAL VARIANTS HERE...
translateBnfcTypeToType (MPL_UIDENT_NO_ARGS_TYPE ident@(UIdent (_, str))) 
    | Just InternalTopBot <- str ^? _InternalConcTypeParser =
        _Right % _TypeConc % _TypeTopBotF # (ident ^. uIdentBnfcIdentGetter)
    | otherwise = review (_Right % _TypeVar ) (ident ^. uIdentBnfcIdentGetter, [])
translateBnfcTypeToType (MPL_UIDENT_ARGS_TYPE ident _ lst _) =
    review _TypeWithArgs <$> ( (ident ^. uIdentBnfcIdentGetter,(),)  <$> res ^. collectsOnlyIfNoLeftsGetter )
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

-- these cannot be generated by the parser (marked as internal)...
translateBnfcTypeToType (MPL_SEQ_ARROW_TYPE _ _ _) =  
    error "Internal error: Received an MPL_SEQ_ARROW_TYPE"
translateBnfcTypeToType (MPL_CONC_ARROW_TYPE _ _ _ _) = 
    error "Internal error: Received an MPL_CONC_ARROW_TYPE"

