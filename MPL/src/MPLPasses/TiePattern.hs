{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module MPLPasses.TiePattern where

import Optics 
import Optics.State
import Optics.State.Operators

import Control.Applicative 

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLPasses.SymbolTable
import MPLPasses.ToGraphTypes
import MPLPasses.ToGraphErrors
import MPLPasses.TieTermUtils

import MPLPasses.Unification

import Data.Functor.Foldable

import Data.Map ( Map (..) )
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Maybe

import Control.Arrow

patternsIToGraph :: 
    TagTypeMap -> 
    [(TypeTag, PatternI BnfcIdent)] -> 
    StateT ToGraphState 
        (Either ToGraphErrors) 
        [ ( (SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag])
        , PatternG TaggedBnfcIdent TypeTag) ]
patternsIToGraph tagmap tagspatts = traverse (uncurry (patternIToGraph tagmap)) tagspatts

unwrappedPatternsIToGraph ::
    TagTypeMap -> 
    [(TypeTag, PatternI BnfcIdent)] -> 
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( (SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag])
        , [PatternG TaggedBnfcIdent TypeTag])
unwrappedPatternsIToGraph tagmap patts = 
    first (concat *** concat <<< unzip) . unzip 
    <$> patternsIToGraph tagmap patts

patternIToGraph :: 
    TagTypeMap -> 
    TypeTag -> 
    PatternI BnfcIdent -> 
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( (SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag])
        , PatternG TaggedBnfcIdent TypeTag)
patternIToGraph tagmap ttype pattern = 
    f ttype pattern
  where
    f ttype (PConstructor ident () ctsargs ()) = do
        (tag, phraseg) <- lookupSeqPhrase ident

        -- check if it is a data type
        unless 
            (has _DataObj (phraseg ^. phraseGObjType)) 
            (throwError $ liftToGraphErrors (_ExpectedDataConstructor # ident))
        -- check arity
        let expectedarity = length (phraseg ^. typePhraseFrom)
            actualarity = length ctsargs
        unless
            (expectedarity == actualarity)
            (throwError $ liftToGraphErrors 
                (_ArityMismatch # (ident, expectedarity, actualarity)))

        --  query the phrase substitutions from the graph
        (clausetype, ttypeargs, clausesubstitutions) <- 
                clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)

        -- fresh type vars for the constructor args
        ttypectsargs <- traverse (const freshTypeTag) ctsargs

        ((ctsargssym, ctsargstypeeqs), ctsargspatts) <- 
            unwrappedPatternsIToGraph tagmap $ zip ttypectsargs ctsargs

        let syms = []  
            typeeqs = TypeEqnsExist (ttypectsargs ++ ttypeargs) $
                [ TypeEqnsEq ( _TypeVar # (ttype, [])
                    , clausetype ) ] 
                ++ zipWith g ttypectsargs (phraseg ^. typePhraseFrom)
                ++ ctsargstypeeqs
            g typetag ctsargtype = TypeEqnsEq
                ( _TypeVar # (typetag, [])
                , fromJust $ substitutesTypeGToTypeGTypeTag 
                    clausesubstitutions ctsargtype )
            pat' = PConstructor 
                (_TaggedBnfcIdent # (ident, tag)) 
                phraseg ctsargspatts (fromJust 
                    $ Map.lookup ttype tagmap)
        return 
            ( (syms ++ ctsargssym, [typeeqs] )
            , pat')

    f ttype (PUnit ident ()) = do
        -- tag doesn't matter for constants...
        ident' <- tagBnfcIdent ident
        return 
            ( ([], [TypeEqnsEq (_TypeVar # (ttype, []), TypeSeq $ TypeUnitF ident')] )
            , PUnit ident' $ fromJust $ Map.lookup ttype tagmap )

    f ttype (PRecord recordphrases ()) = do
        (phrasestags, phrasesg@(focusedphraseg :| rstphraseg)) <- NE.unzip <$> 
            traverse (lookupSeqPhrase . fst) recordphrases

        -- the focused clause (should be the same of all the 
        -- phrases ideally....) we check this immediately after
        let focusedclauseg = focusedphraseg ^. typePhraseContext % phraseParent

        -- check if all destructors
        unless (all (CodataObj==) $ fmap (view phraseGObjType) phrasesg)
            (throwError $ liftToGraphErrors 
                (_ExpectedCodataDestructor # fmap fst recordphrases))

        -- check if all from the same codata clause
        unless (all (focusedclauseg ^. typeClauseName ==) $
            map (view $ typePhraseContext % phraseParent % typeClauseName) rstphraseg)
            (throwError $ liftToGraphErrors 
                (_ExpectedDestructorsFromSameClause # fmap fst recordphrases))

        -- check if the records (phrases) match the declaration in the 
        -- codata clause..
        unless (and $ 
                zipWith (\a b -> a ^. typePhraseName % taggedBnfcIdentName 
                                == b ^. _1 % bnfcIdentName)
                    (focusedclauseg ^. typeClausePhrases) 
                    (NE.toList recordphrases))
            (throwError $ liftToGraphErrors 
                (_IllegalRecordPhrases # recordphrases)) 

        -- get the required substitutions from a codata
        (clausetype, ttypeargs, clausesubstitutions) <- 
                clauseSubstitutions focusedclauseg
        -- fresh type variables for the phrases...
        ttypedtsargs <- traverse (const freshTypeTag) $ NE.toList phrasesg 

        ((dtsargsym, dtsargstypeeqs), dtsargspatts) <- unwrappedPatternsIToGraph tagmap 
            $ zip ttypedtsargs 
            $ map (snd . snd) 
            $ NE.toList recordphrases

        let syms = dtsargsym
            typeeqs = TypeEqnsExist (ttypedtsargs ++ ttypeargs) $
                [ _TypeEqnsEq # (_TypeVar # (ttype, []), clausetype ) ]
                ++ zipWith g ttypedtsargs (NE.toList phrasesg)
                ++ dtsargstypeeqs
            g ttypedts phraseg = _TypeEqnsEq # 
                ( _TypeVar # (ttypedts, [])
                , _TypeSeq # _TypeSeqArrF # 
                    (  fromJust $ traverse (substitutesTypeGToTypeGTypeTag 
                        clausesubstitutions) (phraseg ^. typePhraseFrom)
                    , fromJust $ substitutesTypeGToTypeGTypeTag 
                        clausesubstitutions (phraseg ^. typePhraseTo)
                    )
                )

            pat' = PRecord 
                ( NE.fromList $ getZipList $ 
                    (\ident tag phraseg destpat -> 
                        ( _TaggedBnfcIdent # (ident, tag)
                        , (phraseg, destpat)))
                    <$> ZipList (map fst $ NE.toList recordphrases)
                    <*> ZipList (NE.toList phrasestags)
                    <*> ZipList (NE.toList phrasesg)
                    <*> ZipList dtsargspatts
                )
                $ fromJust $ Map.lookup ttype tagmap

        return ( (syms, [typeeqs] ) , pat' )

    f ttype ( PTuple tuple@(tuple1, tuple2 :| tuples) () ) = do
        -- note: all these partial patterns are okay because
        -- we know for certain that these lists contain at least 2
        -- elements since we have (val, non empty list of values)...
        let tupleelems = tuple1:tuple2:tuples
        ttypetuples <- traverse (const freshTypeTag) tupleelems
        ((symtab, eqns), patts) <- unwrappedPatternsIToGraph tagmap 
            (zip ttypetuples tupleelems)
        let syms = symtab
            ttypetuple1:ttypetuplerest = ttypetuples
            typeeqs = TypeEqnsExist ttypetuples $
                [TypeEqnsEq 
                    ( TypeVar ttype []
                    , TypeSeq $ TypeTupleF 
                        ( TypeVar ttypetuple1 []
                        , NE.fromList $ map (flip TypeVar []) ttypetuplerest)
                    ) ]
                ++ eqns
            tuplepatt1:tuplepatts = patts
            pat' = PTuple (tuplepatt1, NE.fromList tuplepatts) 
                $ fromJust $ Map.lookup ttype tagmap
        return ( (syms, [typeeqs]), pat' )

    f ttype ( PVar ident () ) = do
        ident' <- tagBnfcIdent ident
        let syms = [
                ( ident' ^. taggedBnfcIdentName
                , SymEntry (ident' ^. uniqueTag) $ SymLocalSeqVar ttype ) ]
            typeeqs = []
            pat' = PVar ident' $ fromJust $ Map.lookup ttype tagmap
        return 
            ( ( syms , typeeqs )
            , pat' )
    f ttype (PNull ident ()) = do
        -- tags for null tags do not matter
        ident' <- tagBnfcIdent ident
        return (([],[]), PNull ident' $ fromJust $ Map.lookup ttype tagmap)

    -- TODO
    f ttype n = error $ "ERROR not implemented yet:" ++ show n

