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
            return $ _TypeTopBotF # toLocation ident
        | Just n <- ident ^? name % _Name % _InternalSeqBuiltInTypeParser = 
            case n of 
                InternalString -> return $ _TypeStringF # toLocation ident
                InternalUnit -> return $ _TypeUnitF # toLocation ident
                InternalBool -> return $ _TypeBoolF # toLocation ident
                _ -> Nothing
        | Just n <- ident ^? unwrap % _InternalSeqPrimitiveTypeParser =
            case n of
                InternalInt -> return $ _TypeIntF # toLocation ident
                InternalChar -> return $ _TypeCharF # toLocation ident
                InternalDouble -> return $ _TypeDoubleF # toLocation ident
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
    return $ _TypeParF # (toLocation p, a', b') 
parseBnfcType (B.TENSOR_TYPE a p b) = do
    a' <- parseBnfcType a
    b' <- parseBnfcType b
    return $ _TypeTensorF # (toLocation p, a', b') 
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
    Nothing -> return $ _TypeSeqWithArgs # ((), toTypeIdentP ident, [])

parseBnfcType (B.MPL_UIDENT_ARGS_TYPE ident _ args _) = do
    args' <- traverse parseBnfcType args
    return $ _TypeSeqWithArgs # ((), toTypeIdentP ident, args')

parseBnfcType (B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE ident _ seqs concs _) = do
    seqs' <- traverse parseBnfcType seqs
    concs' <- traverse parseBnfcType concs
    undefined
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
    return $ _TypeUnitF # (toLocation ident)

parseBnfcType (B.MPL_LIST_TYPE l ty r) =
    review _TypeListF . (toSpanLocation l r,) <$> parseBnfcType ty

parseBnfcType (B.MPL_TUPLE_TYPE l ty0 (ty1:tys)  r) =
    review _TypeTupleF . (toSpanLocation l r,) 
        <$> ( (,,) <$> parseBnfcType ty0 <*> f ty1 <*> traverse f tys )
  where
    f (B.TUPLE_LIST_TYPE t) = parseBnfcType t
parseBnfcType (B.MPL_TUPLE_TYPE _ _ _  _) =
    error "Internal error: Received an invalid MPL_TUPLE_TYPE"

parseBnfcType (B.MPL_SEQ_ARROW_TYPE _ _ _) =  
    error "Internal error: Received an MPL_SEQ_ARROW_TYPE"
parseBnfcType (B.MPL_CONC_ARROW_TYPE _ _ _ _) = 
    error "Internal error: Received an MPL_CONC_ARROW_TYPE"

parseTypeSeqVariable :: 
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    MplType MplParsed -> 
    m (IdP MplParsed)
parseTypeSeqVariable n = case n ^? _TypeSeqWithArgs of
    Just (_, a, []) -> return a
    _ -> tell [_ExpectedTypeVarButGot # n] >> throwError ()

parseTypeSeqWithArgs :: 
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    MplType MplParsed -> 
    m (IdP MplParsed, [MplType MplParsed])
parseTypeSeqWithArgs n = case n ^? _TypeSeqWithArgs of
    Just (_, a, args) -> return (a, args)
    _ -> tell [_ExpectedTypeWithVarsButGot # n] >> throwError ()

parseTypeWithArgsAndStateVar ::
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    B.MplType -> 
    B.MplType -> 
    m ((IdP MplParsed, [IdP MplParsed]), IdP MplParsed)
-- definetly could improve error messages here..
parseTypeWithArgsAndStateVar typeident st = do
    typeident' <-  parseBnfcType typeident
    st' <- parseBnfcType st

    (typeident'', args) <- parseTypeSeqWithArgs typeident'
    args' <- traverseTryEach parseTypeSeqVariable args

    st'' <- parseTypeSeqVariable st'

    return ((typeident'', args'), st'')
        

parseStateVarAndTypeWithArgs ::
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m ) => 
    B.MplType -> 
    B.MplType -> 
    m ((IdP MplParsed, [IdP MplParsed]), IdP MplParsed)
parseStateVarAndTypeWithArgs st typeident = 
    parseTypeWithArgsAndStateVar typeident st
