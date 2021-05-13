{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module MplPasses.Parser.ParseMplType where

import qualified MplPasses.Parser.BnfcParse as B

import Optics 
import MplAST.MplCore
import MplAST.MplParsed
import MplPasses.Parser.ParseErrors 
import MplPasses.Parser.ParseUtils 

import Control.Monad.Writer
import Control.Monad.Except
import Data.Maybe

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Control.Arrow
import Control.Applicative


parseNoArgInternalType :: 
    ( ToNameOcc ident ) =>
    ident ->
    Maybe (MplType MplParsed)
parseNoArgInternalType typeident = f $ typeident ^. to toTypeIdentP
  where
    f ident 
        | Just InternalTopBot <- ident ^? unwrap %  _InternalConcTypeParser = 
            return $ _TypeTopBotF # toNameOcc ident
        | Just n <- ident ^? name % _Name % _InternalSeqBuiltInTypeParser = 
            case n of 
                InternalString -> return $ _TypeStringF # toNameOcc ident
                InternalUnit -> return $ _TypeUnitF # toNameOcc ident
                InternalBool -> return $ _TypeBoolF # toNameOcc ident
                _ -> Nothing
        | Just n <- ident ^? unwrap % _InternalSeqPrimitiveTypeParser =
            case n of
                InternalInt -> return $ _TypeIntF # toNameOcc ident
                InternalChar -> return $ _TypeCharF # toNameOcc ident
                InternalDouble -> return $ _TypeDoubleF # toNameOcc ident
        | otherwise = Nothing
    unwrap = name % _Name 


parseBnfcType :: 
    -- BnfcParse B.MplType (MplType MplParsed)
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    B.MplType -> 
    m (MplType MplParsed)
parseBnfcType (B.MPL_TYPE n) = parseBnfcType n
parseBnfcType (B.PAR_TYPE a p b) = do
    a' <- parseBnfcType a
    b' <- parseBnfcType b
    return $ _TypeParF # (toNameOcc p, a', b') 
parseBnfcType (B.TENSOR_TYPE a p b) = do
    a' <- parseBnfcType a
    b' <- parseBnfcType b
    return $ _TypeTensorF # (toNameOcc p, a', b') 
{-
parseBnfcType (B.GETPUT_TYPE getput _ a b _) = do
    a' <- parseBnfcType a
    b' <- parseBnfcType b
    let getput' = getput ^. to toTypeIdentP 
        res = (toLocation getput, a', b')
    case getput' ^? name % _Name % _InternalConcTypeParser of
        Just InternalGet -> return $ _TypeGetF # res
        Just InternalPut -> return $ _TypePutF # res
        _ -> tell [_ExpectedGetOrPutButGot # getput'] >> throwError ()
-}
parseBnfcType (B.MPL_UIDENT_NO_ARGS_TYPE ident) = case parseNoArgInternalType ident of
    Just mpltype -> return mpltype
    Nothing -> return $ _TypeVar # ((), toTypeIdentP ident)

parseBnfcType (B.MPL_UIDENT_ARGS_TYPE ident _ args _) = do
    let ident' = toTypeIdentP ident
        res = toNameOcc ident
    args' <- traverse parseBnfcType args
    case ident' ^? name % _Name % _InternalConcTypeParser of
        Just InternalNeg -> case args' of
            [arg'] -> return $ _TypeNegF # (res, arg')
            err -> tell [ _NegExpectedExactlyOneArgumentButGot # (res, err)  ]
                >> throwError ()
        _ -> return $ _TypeSeqVarWithArgs # ((), ident', args')

parseBnfcType (B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE ident lb [a] [b] rb) = do
    let ident' = toTypeIdentP ident 
        res = toNameOcc ident
    a' <- parseBnfcType a
    b' <- parseBnfcType b
    return $ case ident' ^? name % _Name % _InternalConcTypeParser of
        Just InternalGet -> _TypeGetF # (res, a', b')
        Just InternalPut -> _TypePutF # (res, a', b')
        _ -> _TypeConcVarWithArgs # ((), ident', ([a'], [b']))

parseBnfcType (B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE ident _ seqs concs _) = do
    seqs' <- traverse parseBnfcType seqs
    concs' <- traverse parseBnfcType concs
    return $ _TypeConcVarWithArgs # ((), toTypeIdentP ident, (seqs', concs'))

    {-
    args' <- traverse parseBnfcType args
    return $ _TypeWithArgs # ((), toTypeIdentP ident, args')
    -}

    {-
    a' <- parseBnfcType a
    b' <- parseBnfcType b
    let getput' = getput ^. to toTypeIdentP 
        res = (toLocation getput, a', b')
    case getput' ^? name % _Name % _InternalConcTypeParser of
        Just InternalGet -> return $ _TypeGetF # res
        Just InternalPut -> return $ _TypePutF # res
        _ -> tell [_ExpectedGetOrPutButGot # getput'] >> throwError ()
    -}

parseBnfcType (B.MPL_BRACKETED_TYPE _ ty _) =
    parseBnfcType ty

-- built in types from bnfc..
parseBnfcType (B.MPL_UNIT_TYPE ident _) =
    -- really should check if this is a valid unit type
    return $ _TypeUnitF # (toNameOcc ident)

parseBnfcType (B.MPL_LIST_TYPE l ty r) =
    review _TypeListF . (listname,) <$> parseBnfcType ty
  where
    listname = _NameOcc # (_Name # "[]", toSpanLocation l r)

parseBnfcType (B.MPL_TUPLE_TYPE l ty0 (ty1:tys)  r) =
    review _TypeTupleF . (tuplename,) 
        <$> ( (,,) <$> parseBnfcType ty0 <*> f ty1 <*> traverse f tys )
  where
    f (B.TUPLE_LIST_TYPE t) = parseBnfcType t
    tuplename = _NameOcc # (tupleName (length tys + 1), toSpanLocation l r)

parseBnfcType (B.MPL_TUPLE_TYPE _ _ _  _) =
    error "Internal error: Received an invalid MPL_TUPLE_TYPE"

parseBnfcType (B.MPL_SEQ_ARROW_TYPE _ _ _) =  
    error "Internal error: Received an MPL_SEQ_ARROW_TYPE"
parseBnfcType (B.MPL_CONC_ARROW_TYPE _ _ _ _) = 
    error "Internal error: Received an MPL_CONC_ARROW_TYPE"

parseTypeVariable :: 
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    MplType MplParsed -> 
    m (IdP MplParsed)
parseTypeVariable n = case n ^? _TypeVar of
    Just (_, a) -> return a
    _ -> tell [_ExpectedTypeVarButGot # n] >> throwError ()

parseTypeSeqWithArgs :: 
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    MplType MplParsed -> 
    m (IdP MplParsed, [MplType MplParsed])
parseTypeSeqWithArgs n 
    | Just (_, a, args) <- n ^? _TypeSeqVarWithArgs = return (a, args)
    | Just (_, a) <- n ^? _TypeVar = return (a, [])
    | otherwise = tell [_ExpectedTypeSeqWithArgsButGot # n] >> throwError ()

parseTypeConcWithArgs :: 
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    MplType MplParsed -> 
    m (IdP MplParsed, ([MplType MplParsed], [MplType MplParsed]))
parseTypeConcWithArgs n 
    | Just (_, a, args) <- n ^? _TypeConcVarWithArgs = return (a, args)
    | Just (_, a) <- n ^? _TypeVar = return (a, mempty)
    | otherwise = tell [_ExpectedTypeConcWithArgsButGot # n] >> throwError ()

parseTypeWithArgsSeqAndStateVar ::
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    B.MplType -> 
    B.MplType -> 
    m ((IdP MplParsed, [IdP MplParsed]), IdP MplParsed)
-- definetly could improve error messages here..
parseTypeWithArgsSeqAndStateVar typeident st = do
    typeident' <-  parseBnfcType typeident
    st' <- parseBnfcType st

    (typeident'', args) <- parseTypeSeqWithArgs typeident'
    args' <- traverseTryEach parseTypeVariable args

    st'' <- parseTypeVariable st'

    return ((typeident'', args'), st'')
        
parseStateVarAndTypeWithArgsSeq ::
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    B.MplType -> 
    B.MplType -> 
    m ((IdP MplParsed, [IdP MplParsed]), IdP MplParsed)
parseStateVarAndTypeWithArgsSeq st typeident = 
    parseTypeWithArgsSeqAndStateVar typeident st

parseTypeWithArgsConcAndStateVar ::
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    B.MplType -> 
    B.MplType -> 
    m ((IdP MplParsed, ([IdP MplParsed],[IdP MplParsed] )), IdP MplParsed)
-- definetly could improve error messages here..
parseTypeWithArgsConcAndStateVar typeident st = do
    typeident' <-  parseBnfcType typeident
    st' <- parseBnfcType st

    (typeident'', (seqs, concs)) <- parseTypeConcWithArgs typeident'
    seqs' <- traverseTryEach parseTypeVariable seqs
    concs' <- traverseTryEach parseTypeVariable concs

    st'' <- parseTypeVariable st'

    return ((typeident'', (seqs', concs')), st'')
        
parseStateVarAndTypeWithArgsConc ::
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    B.MplType -> 
    B.MplType -> 
    m ((IdP MplParsed, ([IdP MplParsed], [IdP MplParsed])), IdP MplParsed)
parseStateVarAndTypeWithArgsConc st typeident = 
    parseTypeWithArgsConcAndStateVar typeident st
