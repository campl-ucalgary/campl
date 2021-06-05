{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module AMPLSymbolTable where

import AMPLConstructBag
import AMPLFunIDGen
import AMPLAST

import Language.ParAMPL
import Language.LexAMPL
import Language.AbsAMPL
import Language.ErrM
import Language.LayoutAMPL

import Control.Monad.MonadUnique

import Data.List 
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Function
import Data.Foldable
import Data.Maybe
import Control.Arrow
import Control.Monad
import Data.Coerce

import AMPLTypes
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Data.Map (Map)
import qualified Data.Map as Map

data SymEntry =
    SymProtocol ProtocolInfo
    | SymCoprotocol CoprotocolInfo
    | SymData DataInfo
    | SymCodata CodataInfo
    | SymProcessInfo (ProcessInfo FunID)
    | SymFunctionInfo (FunctionInfo FunID)
  deriving (Show, Eq)

class HasAmbiguousLookupError a where
    symAmbiguousLookup :: (String, [RowColPos]) -> a

newtype SymAmbiguousLookup = SymAmbiguousLookup (String, [RowColPos])
    -- ^ error: Name of identifier, positions of occurences
  deriving Show

instance HasAmbiguousLookupError SymAmbiguousLookup where
    symAmbiguousLookup = SymAmbiguousLookup

-- | Default way to create a symbol table
makeSymbolTable :: 
    ( HasAmbiguousLookupError e ) =>
    AmplConstructsBag -> 
    Map String (Either e SymEntry)
makeSymbolTable n = fst (runUnique (makeSymbolTable' n) (initFunID :: FunID))

-- | Makes the symbol table while threading the explicit state of
-- which FunID to give to processes and functions
makeSymbolTable' :: 
    ( HasAmbiguousLookupError e 
    , AMPLFunIDGen s
    , MonadUnique s m ) =>
    AmplConstructsBag -> 
    m (Map String (Either e SymEntry))
makeSymbolTable' AmplConstructsBag{ 
    protocolInfo     = protocolInfo 
    , coprotocolInfo = coprotocolInfo 
    , dataInfo     = dataInfo 
    , codataInfo   = codataInfo 
    , processInfo  = processInfo 
    , functionInfo = functionInfo 
    } = do
            processInfo'' <- processInfo' 
            functionInfo'' <- functionInfo'
            return $ Map.fromList $ concat
                [ protocolInfo'
                , coprotocolInfo' 
                , dataInfo' 
                , codataInfo'
                , processInfo'' 
                , functionInfo'' ]
                -- NOTE -- this does not permit recursively calling main!
  where
    protocolInfo' = foldl (typeToSymEntryAssocList SymProtocol) [] $ queryDuplicateLists protocolInfo
    coprotocolInfo' = foldl (typeToSymEntryAssocList SymCoprotocol)  [] $ queryDuplicateLists coprotocolInfo
    dataInfo' = foldl (typeToSymEntryAssocList SymData) [] $ queryDuplicateLists dataInfo
    codataInfo' = foldl (typeToSymEntryAssocList SymCodata) [] $ queryDuplicateLists codataInfo
    processInfo' = foldlM processToSymEntryAssocList [] $ queryDuplicateLists processInfo
    functionInfo' = foldlM functionToSymEntryAssocList [] $ queryDuplicateLists functionInfo

    processToSymEntryAssocList :: 
        ( HasAmbiguousLookupError e 
        , AMPLFunIDGen s 
        , MonadUnique s m ) =>
        [(String, Either e SymEntry)] -> 
        NonEmpty (String, ProcessInfo [ACom]) -> 
        m [(String, Either e SymEntry)]
    processToSymEntryAssocList acc ( (name, (rowcolpos, (seqvs, inchs, outchs, _))) :| [] ) = do
        s <- fresh
        return $ (name, Right $ SymProcessInfo (rowcolpos, (seqvs, inchs, outchs, getFunID s))) : acc
    processToSymEntryAssocList acc ( (name, val) :| rs ) = 
        return $ (name, Left $ symAmbiguousLookup (name, fst val : map (fst . snd) rs)) : acc

    functionToSymEntryAssocList :: 
        ( HasAmbiguousLookupError e 
        , AMPLFunIDGen s 
        , MonadUnique s m ) =>
        [(String, Either e SymEntry)] -> 
        NonEmpty (String, FunctionInfo [ACom]) -> 
        m [(String, Either e SymEntry)]
    functionToSymEntryAssocList acc ( (name, (rowcolpos, (seqvs, _))) :| [] ) = do
        s <- fresh
        return $ (name, Right $ SymFunctionInfo (rowcolpos, (seqvs, getFunID s))) : acc
    functionToSymEntryAssocList acc ( (name, val) :| rs ) = 
        return $ (name, Left $ symAmbiguousLookup (name, fst val : map (fst . snd) rs)) : acc

    typeToSymEntryAssocList :: 
        HasAmbiguousLookupError e =>
            -- | function to construct the sym table entry from the corresponding entry in the bag
        ((RowColPos, b) -> SymEntry) -> 
        [(String, Either e SymEntry)] -> 
        NonEmpty (String, (RowColPos, b)) -> 
        [(String, Either e SymEntry)]
    typeToSymEntryAssocList f acc ( (name, val) :| [] ) = (name, Right $ f val) : acc
    typeToSymEntryAssocList f acc ( (name, val) :| rs ) = (name, Left $ symAmbiguousLookup (name, fst val : map (fst . snd) rs)) : acc

    queryDuplicateLists :: (Eq a, Ord a) => [(a,b)] -> [NonEmpty (a,b)]
    queryDuplicateLists = NE.groupWith fst . sortBy (compare `on` fst)

-- Mostly duplicated code of way to lookup things in the symbol table..
class HasProtocolLookupError e where
    protocolDoesNotExist :: Ident -> e
    handleDoesNotExist :: Ident -> Ident -> e
    notProtocol :: Ident -> SymEntry -> e

data ProtocolLookupError =
    ProtocolDoesNotExist Ident
    | HandleDoesNotExist Ident Ident 
    | NotProtocol Ident SymEntry
  deriving Show

instance HasProtocolLookupError ProtocolLookupError where
    protocolDoesNotExist = ProtocolDoesNotExist
    handleDoesNotExist = HandleDoesNotExist
    notProtocol = NotProtocol

class HasCoprotocolLookupError e where
    coprotocolDoesNotExist :: Ident -> e
    cohandleDoesNotExist :: Ident -> Ident -> e
    notCoprotocol :: Ident -> SymEntry -> e

data CoprotocolLookupError =
    CoprotocolDoesNotExist Ident
    | CohandleDoesNotExist Ident Ident 
    | NotCoprotocol Ident SymEntry
  deriving Show

instance HasCoprotocolLookupError CoprotocolLookupError where
    coprotocolDoesNotExist = CoprotocolDoesNotExist
    cohandleDoesNotExist = CohandleDoesNotExist
    notCoprotocol = NotCoprotocol

class HasDataLookupError e where
    dataDoesNotExist :: Ident -> e
    constructorDoesNotExist :: Ident -> Ident -> e
    notData :: Ident -> SymEntry -> e 

data DataLookupError =
    DataDoesNotExist Ident
    | ConstructorDoesNotExist Ident Ident 
    | NotData Ident SymEntry
  deriving Show

instance HasDataLookupError DataLookupError where
    dataDoesNotExist = DataDoesNotExist
    constructorDoesNotExist = ConstructorDoesNotExist
    notData = NotData

class HasCodataLookupError e where
    codataDoesNotExist :: Ident -> e
    destructorDoesNotExist :: Ident -> Ident -> e
    notCodata :: Ident -> SymEntry -> e 

data CodataLookupError =
    CodataDoesNotExist Ident
    | DestructorDoesNotExist Ident Ident 
    | NotCodata Ident SymEntry
  deriving Show

instance HasCodataLookupError CodataLookupError where
    codataDoesNotExist = CodataDoesNotExist
    destructorDoesNotExist = DestructorDoesNotExist
    notCodata = NotCodata

lookupProtocolAndHandle :: 
    ( HasAmbiguousLookupError e
    , HasProtocolLookupError e ) =>
    Ident ->    
    Ident -> 
    Map String (Either e SymEntry) ->
    Either e (RowColPos, (Ident, HCaseIx))
lookupProtocolAndHandle protocol handle map = 
    typeLookupHelper (protocolDoesNotExist, relookup) protocol handle map
  where
    relookup = Left ||| (\case SymProtocol info -> handleLookup info ; n -> Left (notProtocol protocol n) )

    handleLookup (pos, handles) = maybe 
        (Left $ handleDoesNotExist protocol handle) 
        (Right . (pos,))
        $ find ( (==fst handle) . fst . fst ) handles

lookupCoprotocolAndCohandle :: 
    ( HasAmbiguousLookupError e
    , HasCoprotocolLookupError e ) =>
    Ident ->    
    Ident -> 
    Map String (Either e SymEntry) ->
    Either e (RowColPos, (Ident, HCaseIx))
lookupCoprotocolAndCohandle coprotocol cohandle map = 
    typeLookupHelper (coprotocolDoesNotExist, relookup) coprotocol cohandle map
  where
    relookup = Left ||| (\case SymCoprotocol info -> cohandleLookup info ; n -> Left (notCoprotocol coprotocol n) )

    cohandleLookup (pos, cohandles) = maybe 
        (Left $ cohandleDoesNotExist coprotocol cohandle) 
        ( Right . (pos,) )
        $ find ( (==fst cohandle) . fst . fst ) cohandles

lookupDataAndConstructor :: 
    ( HasAmbiguousLookupError e
    , HasDataLookupError e ) =>
    Ident ->    
    Ident -> 
    Map String (Either e SymEntry) ->
    Either e (RowColPos, (Ident, (ConsIx, Word)))
lookupDataAndConstructor dataa constructor map = 
    typeLookupHelper (dataDoesNotExist, relookup) dataa constructor map
  where
    relookup = Left ||| (\case SymData info -> dataLookup info ; n -> Left (notData dataa n) )

    dataLookup (pos, datas) = maybe 
        (Left $ constructorDoesNotExist dataa constructor) 
        (Right . (pos,)) 
        $ find ( (==fst constructor) . fst . fst ) datas

lookupCodataAndDestructor :: 
    ( HasAmbiguousLookupError e
    , HasCodataLookupError e ) =>
    Ident ->    
    Ident -> 
    Map String (Either e SymEntry) ->
    Either e (RowColPos, (Ident, (DesIx, Word)))
lookupCodataAndDestructor codata destructor map = 
    typeLookupHelper (codataDoesNotExist, relookup) codata destructor map
  where
    relookup = Left ||| (\case SymCodata info -> codataLookup info ; n -> Left (notCodata codata n) )

    codataLookup (pos, codatas) = maybe 
        (Left $ destructorDoesNotExist codata destructor) 
        (Right . (pos,))
        $ find ( (==fst destructor) . fst . fst ) codatas

-- | unsafe type version
lookupCodataAndDestructor' :: 
    ( HasAmbiguousLookupError e
    , HasCodataLookupError e ) =>
    Ident ->    
    Ident -> 
    Map String (Either e SymEntry) ->
    Either e (RowColPos, (Ident, (Word, Word)))
lookupCodataAndDestructor' codata dest map = (coerce :: (RowColPos, (Ident, (DesIx, Word))) -> (RowColPos, (Ident, (Word, Word)))) <$> lookupCodataAndDestructor codata dest map

typeLookupHelper ::
    ( Ident -> e
        --  Type (data / codata / protocol / coprotocol )does not exist error function 
    , Either e SymEntry -> Either e b ) ->
        --  relook up function (for the subtype){
    -- | Protocol
    Ident ->    
    -- | Constructor
    Ident -> 
    -- | map to lookup
    Map String (Either e SymEntry) ->
    -- | result
    Either e b
typeLookupHelper (mainTypeDoesNotExist, relookup) mainType subtype map = maybe 
    (Left (mainTypeDoesNotExist mainType))
    relookup
    $ Map.lookup (fst mainType) map

class HasLookupFunctionError e where
    functionDoesNotExist :: Ident -> e
    notFunction :: Ident -> SymEntry -> e 

data FunctionLookupError =
    FunctionDoesNotExist Ident
    | NotFunction Ident SymEntry
  deriving Show

instance HasLookupFunctionError FunctionLookupError where
    functionDoesNotExist = FunctionDoesNotExist
    notFunction = NotFunction

class HasLookupProcessError e where
    processDoesNotExist :: Ident -> e
    notProcess :: Ident -> SymEntry -> e 

data ProcessLookupError =
    ProcessDoesNotExist Ident
    | NotProcess Ident SymEntry
  deriving Show

instance HasLookupProcessError ProcessLookupError where
    processDoesNotExist = ProcessDoesNotExist
    notProcess = NotProcess

lookupFunction :: 
    ( HasAmbiguousLookupError e
    , HasLookupFunctionError e ) =>
    Ident ->
    Map String (Either e SymEntry) ->
    Either e (FunctionInfo FunID)
lookupFunction var map = maybe 
    (Left $ functionDoesNotExist var) 
    lookupHelper 
    $ Map.lookup (fst var) map
  where
    lookupHelper = Left ||| (\case SymFunctionInfo info -> Right info ; n -> Left $ notFunction var n)

lookupProcess :: 
    ( HasAmbiguousLookupError e
    , HasLookupProcessError e ) =>
    Ident ->
    Map String (Either e SymEntry) ->
    Either e (ProcessInfo FunID)
lookupProcess var map = maybe 
    (Left $ processDoesNotExist var) 
    lookupHelper 
    $ Map.lookup (fst var) map
  where
    lookupHelper = Left ||| (\case SymProcessInfo info -> Right info ; n -> Left $ notProcess var n)

