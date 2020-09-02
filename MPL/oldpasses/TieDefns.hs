{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module MPLPasses.TieDefns where

import Optics 
import Optics.State
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST

import MPLPasses.GraphGenCore
import MPLPasses.TieTypeClause
import MPLPasses.TypeClauseSanityErrors
import MPLPasses.Unification
import MPLPasses.InferExprType
import MPLPasses.TiePattern
import MPLPasses.SymbolTable
import MPLPasses.TieDefnsTypes
import MPLPasses.TieDefnsErrors 
import MPLPasses.TieDefnsUtils
import MPLPasses.CallErrors
import MPLPasses.ProcessErrors

import MPLUtil.Data.Tuple.Optics
import MPLUtil.Data.Either.AccumEither

import Data.Maybe
import Data.List
import Data.Bool
import Data.Foldable
import Data.Tuple
import Control.Monad.RWS
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Bifunctor as Bifunctor
import Control.Arrow
import Data.Either
import Data.Function
import Data.Semigroup
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Base 

import Data.Map ( Map (..) )
import qualified Data.Map as Map
import Data.Set ( Set (..) )
import qualified Data.Set as Set

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import Debug.Trace 



progInterfaceToGraph :: 
    (SymbolTable, GraphGenCoreEnv, GraphGenCoreState) ->
    Prog (DefnI BnfcIdent) ->
    Either [TieDefnsError] (Prog (DefnG TaggedBnfcIdent TypeTag TaggedChIdent))
progInterfaceToGraph initstate (Prog stmts) = Prog <$> res 
  where
    res :: Either [TieDefnsError] [Stmt (DefnG TaggedBnfcIdent TypeTag TaggedChIdent)]
    res = accumEithers $ f initstate stmts

    f :: 
        (SymbolTable, GraphGenCoreEnv, GraphGenCoreState) -> 
        [Stmt (DefnI BnfcIdent)] -> 
        [Either [TieDefnsError] (Stmt (DefnG TaggedBnfcIdent TypeTag TaggedChIdent))]
    f (sym, env, corest) [] = []
    f (sym, env, corest) (stmt:rst) = 
         stmt' : f (sym', env, corest') rst
      where
        (stmtg, corest', errlg) = 
            runRWS (unGraphGenCore $ runReaderT (stmtIToGraph stmt) sym) env corest
        stmt' = bool (Left errlg) (Right stmtg) (null errlg)
        sym' = either (const []) collectSymEntries stmt' ++ sym


stmtIToGraph ::
    Stmt (DefnI BnfcIdent) -> 
    ReaderT SymbolTable GraphGenCore (Stmt (DefnG TaggedBnfcIdent TypeTag TaggedChIdent))
stmtIToGraph (Stmt defs wstmts) = do
    -- compute new where bindings first ..
    wstmts' <- traverse stmtIToGraph wstmts
    let wsyms = mconcat . map collectSymEntries $ wstmts'

    symtab <- ask
    tag <- freshUniqueTag
    -- compute the definitions graph
    rec ((), st, defs') <- lift $ 
            runTieDefnsT 
            tagmap
            (TieDefnsTState (wsyms++symtab) mempty)
            (defnsToGraph (NE.toList defs)) 
        let eqnspkg = st ^. tieDefnsTypeEqnsPkg
            eqns = TypeEqnsForall 
                (eqnspkg ^. tieDefnsTypeForall)
                [ TypeEqnsExist 
                    (eqnspkg ^. tieDefnsTypeExist) 
                    (eqnspkg ^. tieDefnsTypeEqns) ]
            -- seqpkg = solveTypeEq (trace (pprint seqeqns) seqeqns)
            pkg = solveTypeEq eqns
            Right pkg' = pkg

            -- concpkg = solveTypeEq (uncurry TypeEqnsExist $ first (map (view exprTtypeInternal)) concdmyeqns)
            -- Right concpkg' = concpkg

            tagmap = packageToTagMap pkg' -- <> concpkg'

    tell $ either pure (const []) pkg
    
    return $ Stmt (NE.fromList defs') wstmts'

defnsToGraph :: 
    [DefnI BnfcIdent] ->
    TieDefns ()
defnsToGraph [] = return mempty
defnsToGraph (a:as) = case a ^. unDefnI of 
    DataDefn ival -> mdo
        lift $ typeClauseArgsSanityCheck ival
        lift $ phraseToVarsAreStateVarCheck ival

        clauseg <- lift $ tieTypeClauseGraph (querySeqClauses symtab) DataObj ival
        let defn' = ObjectG clauseg :: DefnG TaggedBnfcIdent TypeTag TaggedChIdent

        tieDefnsStateSymbolTable %= ((collectSymEntries defn')++)

        tell [defn']

        defnsToGraph as
        symtab <- guse tieDefnsStateSymbolTable

        return () 

    -- mostly duplicated code...
    CodataDefn ival -> mdo  
        lift $ typeClauseArgsSanityCheck ival
        -- codata has a different check
        lift $ codataStateVarOccurenceCheck ival

        clauseg <- lift $ tieTypeClauseGraph (querySeqClauses symtab) CodataObj ival
        let defn' = ObjectG clauseg :: DefnG TaggedBnfcIdent TypeTag TaggedChIdent

        tieDefnsStateSymbolTable %= ((collectSymEntries defn')++)

        tell [defn']

        defnsToGraph as
        symtab <- guse tieDefnsStateSymbolTable

        return () 

    -- mostly duplciated code...
    ProtocolDefn ival -> mdo
        lift $ typeClauseArgsSanityCheck ival
        -- protocols has a different check
        lift $ phraseToVarsAreStateVarCheck ival
        lift $ exactlyOnePhraseFromVarsCheck ival

        clauseg <- lift $ tieTypeClauseGraph (queryClauses symtab) ProtocolObj ival
        let defn' = ObjectG clauseg :: DefnG TaggedBnfcIdent TypeTag TaggedChIdent

        tieDefnsStateSymbolTable %= ((collectSymEntries defn')++)

        tell [defn']

        defnsToGraph as
        symtab <- guse tieDefnsStateSymbolTable
        return ()

    -- mostly duplciated code...
    CoprotocolDefn ival -> mdo
        lift $ typeClauseArgsSanityCheck ival
        -- coprotocls have a different check
        lift $ phraseFromVarsAreStateVarCheck ival

        clauseg <- lift $ 
            tieTypeClauseGraph (queryClauses symtab) CoprotocolObj ival
        let defn' = ObjectG clauseg :: DefnG TaggedBnfcIdent TypeTag TaggedChIdent

        tieDefnsStateSymbolTable %= ((collectSymEntries defn')++)

        tell [defn']

        defnsToGraph as
        symtab <- guse tieDefnsStateSymbolTable

        return ()

    FunctionDecDefn (FunctionDefn funident funtype funbody) -> mdo  
        funident' <- lift $ tagBnfcIdent funident
        let funbody' = NE.toList funbody
        ttypes <- lift freshExprTypeTags

        funtype' <- lift $ case funtype of
            Just funtype -> Just 
                <$> evalStateT (annotateTypeIToTypeGAndScopeFreeVars 
                $ (TypeSeq . uncurry TypeSeqArrF) funtype) (querySeqClauses symtab)
            Nothing -> return Nothing


        (forallvars, forallsubs) <- lift . join . fmap writer . splitGraphGenCore $ do
                mfuntypefreevarsandsubs <- traverse genTypeGSubs funtype'
                return $ fromMaybe ([],[]) mfuntypefreevarsandsubs 

        let ttype = ttypes ^. exprTtype
            ttypeinternal = ttypes ^. exprTtypeInternal

        tagmap <- gview equality 

        tieDefnsStateSymbolTable %= (
            ( funident' ^. taggedBnfcIdentName
            , _SymEntry # 
                ( funident' ^. uniqueTag
                , funident' ^. taggedBnfcIdentPos
                , _SymSeqCall # _SymCall # 
                -- if we have an explicit type, just use that, otherwise
                -- give it the dummy type... (better error messages lower down where
                -- the issue really is..
                    ( maybe (SymCallDummyTypeVar ttype) SymCallExplicitType funtype'
                    , FunctionKnot fun')
                )
            ):)

        ttypespattsandexps <- lift $ traverse (const freshExprTypeTags) funbody'

        ~((pattsgexpsg, eqns), _) <- lift 
            $ runTieFun (TieExprEnv ttypes tagmap) symtab 
            $ fmap unzip <$> 
                traverse 
                    (\(tag, pattsexp) -> 
                        local (set tieExprEnvTypeTags tag) $ patternsIAndExprIToGraph pattsexp) 
                    $ zip ttypespattsandexps funbody'

        let fun' = FunctionDefn 
                funident' 
                (fromJust $ Map.lookup ttypeinternal tagmap) 
                (NE.fromList pattsgexpsg)

            ttypephrases = map (view exprTtype) ttypespattsandexps
            typeeqns = 
                TypeEqnsStableRef (ttypeinternal, TypeVar ttype []):
                -- if there is for all quantification, we need to include the equality to the
                -- user provided type
                toList (fmap (TypeEqnsEq . (TypeVar ttype [],))
                    $ join 
                    $ substitutesTypeGToTypeGTypeTag forallsubs 
                    <$> funtype') 
                -- Ensure that each of the type phrases is equal to the parent type.
                ++ map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypephrases 
                -- collected equations context 
                ++ eqns

        tieDefnsTypeEqnsPkg % tieDefnsTypeForall %= (++forallvars)
        tieDefnsTypeEqnsPkg % tieDefnsTypeExist  %= (++ttypephrases)
        tieDefnsTypeEqnsPkg % tieDefnsTypeEqns   %= (++typeeqns)

        tell [FunctionDecDefG fun']

        defnsToGraph as

        symtab <- guse tieDefnsStateSymbolTable 

        return ()

    -- more or less duplciated code from a function as well..
    ProcessDecDefn (ProcessDefn ident proctype procbody) -> mdo
        procident' <- lift $ tagBnfcIdent ident
        ttypes <- lift freshExprTypeTags
        let ttype = ttypes ^. exprTtype
            ttypeinternal = ttypes ^. exprTtypeInternal

        let procbody' = NE.toList procbody

        proctype' <- lift $ case proctype of
            Just proctype -> Just 
                <$> evalStateT (annotateTypeIToTypeGAndScopeFreeVars 
                $ TypeConc $ _TypeConcArrF # proctype) (queryClauses symtab)
            Nothing -> return Nothing

        -- splitting is not needed here...
        (forallvars, forallsubs) <- lift . join . fmap writer . splitGraphGenCore $ do
                mfuntypefreevarsandsubs <- traverse genTypeGSubs proctype'
                return $ fromMaybe ([],[]) mfuntypefreevarsandsubs 


        tagmap <- gview equality 

        tieDefnsStateSymbolTable %= (
            ( procident' ^. taggedBnfcIdentName
            , _SymEntry # 
                ( procident' ^. uniqueTag
                , procident' ^. taggedBnfcIdentPos
                , _SymConcCall # _SymCall # 
                -- if we have an explicit type, just use that, otherwise
                -- give it the dummy type... (better error messages lower down where
                -- the issue really is..
                    ( maybe (SymCallDummyTypeVar ttype) SymCallExplicitType proctype'
                    , ProcessKnot proc')
                )
            ):)

        ttypespattschscmds <- lift $ traverse (const freshExprTypeTags) procbody'

        ~((pattsgchscmdsg, eqns), _) <- lift
            $ runTieProc tagmap (TieProcSt symtab mempty)
            $ fmap unzip 
            $ traverse (\(tag, pattschscmds) -> patternsChsCmdIToGraph tag pattschscmds)
            $ zip ttypespattschscmds procbody'

        let proc' = ProcessDefn procident' (fromJust $ Map.lookup ttypeinternal tagmap) 
                $ NE.fromList pattsgchscmdsg
            ttypephrases = map (view exprTtype) ttypespattschscmds 
            typeeqns = TypeEqnsStableRef (ttypeinternal, TypeVar ttype []):
                -- if there is for all quantification, we need to include the equality to the
                -- user provided type
                toList (fmap (TypeEqnsEq . (TypeVar ttype [],))
                    $ join 
                    $ substitutesTypeGToTypeGTypeTag forallsubs 
                    <$> proctype') 
                -- Ensure that each of the type phrases is equal to the parent type.
                ++ map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypephrases 
                -- collected equations context 
                ++ eqns
                
        
        tieDefnsTypeEqnsPkg % tieDefnsTypeForall %= (++forallvars)
        tieDefnsTypeEqnsPkg % tieDefnsTypeExist  %= (++ttypephrases)
        tieDefnsTypeEqnsPkg % tieDefnsTypeEqns   %= (++typeeqns)

        tell [ ProcessDecDefG  proc' ]

        defnsToGraph as

        symtab <- guse tieDefnsStateSymbolTable
        
        return ()

----------------------------
-- Processes
----------------------------
patternsChsCmdIToGraph ::
    ExprTypeTags ->
    (([PatternI BnfcIdent], [BnfcIdent], [BnfcIdent]), ProcessCommandsI BnfcIdent) ->
    TieProc ( ( ([PatternG TaggedBnfcIdent TypeTag], [TaggedChIdent], [TaggedChIdent])
          , NonEmpty (ProcessCommandG TaggedBnfcIdent TypeTag TaggedChIdent))
        , TypeEqns TaggedBnfcIdent TypeTag )
patternsChsCmdIToGraph ttypes ((patts, inchs, outchs), cmds) = do
    let ttype = ttypes ^. exprTtype
        ttypeinternal = ttypes ^. exprTtypeInternal

    tagmap <- gview equality
    symtab <- guse tieProcSymTab 

    -- get the pattern tags..
    ttypespatts <- lift $ traverse (const freshExprTypeTags) patts

    -- tag the local channels // add them to the map..
    inchs' <- lift $ traverse (fmap (review _TaggedChIdent . (,Input)) . tagBnfcIdent) inchs
    ttypesinchs <- lift $ traverse (const freshExprTypeTags) inchs'
    outchs' <- lift $ traverse (fmap (review _TaggedChIdent . (,Output)) . tagBnfcIdent) outchs
    ttypesoutchs <- lift $ traverse (const freshExprTypeTags) outchs'

    let inchssyms = zipWith chssymgen inchs' ttypesinchs
        outchssyms = zipWith chssymgen outchs' ttypesoutchs
        chssymgen n ttypes = 
            ( n ^. bnfcIdentName
            , _SymEntry # 
                    ( n ^. uniqueTag
                    , n ^. bnfcIdentPos
                    , SymChInfo (ttypes ^. exprTtype) (n ^. taggedChIdentPolarity))
            )
            

    -- TODO need to do overlapping declarations check..
    tieProcChCxt %= (inchssyms<>)
    tieProcChCxt %= (outchssyms<>)

    -- compile the patterns
    ((pattseqns, pattsg), symtab') <- lift $ runTieFun 
        (_TieExprEnv # (ttypes, tagmap))
        symtab
        $ unwrappedPatternsIToGraph $ zip ttypespatts patts

    tieProcSymTab .= symtab'

    -- compilation of the cmds
    ~(cmds', cmdeqns) <- cmdsIToGraph 
        <=< lift . traverse 
            ( (`evalStateT` (Set.fromList (inchs ++ outchs), Set.empty))
            . processCommandComputeVariableDeclarations) $ cmds

    tieProcSymTab .= symtab

    let ttypepatts = ttypespatts & mapped %~ view exprTtype
        ttypeeqn = TypeEqnsExist (ttypepatts ++ map (view exprTtype) (ttypesinchs ++ ttypesoutchs)) $
            [ TypeEqnsEq
                ( TypeVar ttype []
                , TypeConc $ TypeConcArrF
                    (map (flip TypeVar []) ttypepatts) 
                    (map (flip TypeVar [] . view exprTtype) ttypesinchs)
                    (map (flip TypeVar [] . view exprTtype) ttypesoutchs)
                )
            , TypeEqnsStableRef
                (ttypeinternal, TypeVar ttype[])
            ]
            ++ map (review _TypeEqnsStableRef <<< view exprTtypeInternal &&& flip TypeVar [] . view exprTtype)
                ttypesinchs
            ++ map (review _TypeEqnsStableRef <<< view exprTtypeInternal &&& flip TypeVar [] . view exprTtype)
                ttypesoutchs
            ++ pattseqns
            ++ cmdeqns

    -- return (((pattsg, inchs', outchs'), NE.fromList cmds'), ttypeeqn)
    return (((pattsg, inchs', outchs'), cmds'), ttypeeqn)

cmdIToGraph :: 
    ProcessCommandI BnfcIdent -> 
    TieProc
        ( ProcessCommandG TaggedBnfcIdent TypeTag TaggedChIdent
        , [TypeEqns TaggedBnfcIdent TypeTag] )
cmdIToGraph = f
  where
    f :: ProcessCommandI BnfcIdent -> TieProc
        ( ProcessCommandG TaggedBnfcIdent TypeTag TaggedChIdent
        , [TypeEqns TaggedBnfcIdent TypeTag] )
    f (CHalt ch) = do
        chcxt <- guse tieProcChCxt 
        let lkupch = lookup (ch ^. bnfcIdentName) chcxt
            lkupch'  = fromJust lkupch
        tieProcChCxt %= deleteSymTab (ch^.bnfcIdentName)

        lift $ tell $ bool [] [_NotInScope # ch] $ isNothing lkupch

        topbot <- lift $ tagConcTypeF $ _BnfcTypeTopBotF # ()
        let eqn = TypeEqnsEq 
                ( _TypeVar # (lkupch' ^. symEntryInfo % symChTypeTag, [])
                , TypeConc topbot )
            cmd' = CHalt $ (ch ^. bnfcIdentName, lkupch') ^. taggedChIdent
            
        return (cmd' , bool [] [eqn] $ isJust lkupch)
    f (CClose ch) = do
        chcxt <- guse tieProcChCxt 
        let lkupch = lookup (ch ^. bnfcIdentName) chcxt
            lkupch'  = fromJust lkupch
        tieProcChCxt %= deleteSymTab (ch ^. bnfcIdentName)

        lift $ tell $ bool [] [_NotInScope # ch] $ isNothing lkupch

        topbot <- lift $ tagConcTypeF $ _BnfcTypeTopBotF # ()
        let eqn = TypeEqnsEq 
                ( _TypeVar # (lkupch' ^. symEntryInfo % symChTypeTag, [])
                , TypeConc topbot )
            cmd' = CClose $ (ch ^. bnfcIdentName, lkupch') ^. taggedChIdent
            
        return (cmd' , bool [] [eqn] $ isJust lkupch)

    f (CGet patt ch) = do
        chcxt <- guse tieProcChCxt 
        symtab <- guse tieProcSymTab
        tagmap <- gview equality

        -- compile the pattern first..
        ttypespatt <- lift freshExprTypeTags
        ((patteqns, patt'), symtab') <- lift 
            $ join . fmap writer . splitGraphGenCore 
            $ runTieFun (TieExprEnv ttypespatt tagmap) symtab 
            $ patternIToGraph patt
        tieProcSymTab .= symtab'

        -- then now the channel..
        ttypech' <- lift freshTypeTag
        let lkupch = lookup (ch ^. bnfcIdentName) chcxt
            lkupch' = fromJust lkupch
            ch' = (ch ^. bnfcIdentName, lkupch') ^. taggedChIdent

            ttypech = lkupch' ^. symEntryInfo % symChTypeTag
            chpol = lkupch' ^. symEntryInfo % symChPolarity

        tieProcChCxt %= 
            (maybeToList (((ch ^. bnfcIdentName,) . set (symEntryInfo % symChTypeTag) ttypech') <$> lkupch)
                <>) . deleteSymTab (ch ^. bnfcIdentName)

        lift $ tell $ bool [] [_NotInScope # ch] $ isNothing lkupch

        eqtype <- lift 
            $ fmap fst . splitGraphGenCore 
            $ tagConcTypeF 
            $ polarity chpol 
                ( _BnfcTypePutF # (TypeVar (ttypespatt ^. exprTtype) []
                , TypeVar ttypech' []) )
                ( _BnfcTypeGetF # (TypeVar (ttypespatt ^. exprTtype) []
                , TypeVar ttypech' []))
        let eqns = TypeEqnsExist [ttypespatt ^. exprTtype, ttypech'] $ 
                [ TypeEqnsEq (TypeVar ttypech [], TypeConc eqtype) 
                , TypeEqnsStableRef (ttypespatt ^. exprTtypeInternal, TypeVar (ttypespatt ^. exprTtype) [])]
                ++ patteqns

            cmd' = CGet patt' ch'

        return (cmd' , bool [] [eqns] $ isJust lkupch)

    f (CPut expr ch) = do
        chcxt <- guse tieProcChCxt 
        symtab <- guse tieProcSymTab
        tagmap <- gview equality

        -- compile the expression first..
        ttypesexpr <- lift freshExprTypeTags

        -- TODO We need a higher order check here...
        ((expr', expreqns), symtab') <- lift 
            $ join . fmap writer . splitGraphGenCore 
            $ runTieFun (TieExprEnv ttypesexpr tagmap) symtab 
            $ exprIToGraph expr
        tieProcSymTab .= symtab'

        -- then now the channel..
        ttypech' <- lift freshTypeTag
        let lkupch = lookup (ch ^. bnfcIdentName) chcxt
            lkupch' = fromJust lkupch
            ch' = (ch ^. bnfcIdentName, lkupch') ^. taggedChIdent

            ttypech = lkupch' ^. symEntryInfo % symChTypeTag
            chpol = lkupch' ^. symEntryInfo % symChPolarity

        tieProcChCxt %= 
            (maybeToList (((ch ^. bnfcIdentName,) . set (symEntryInfo % symChTypeTag) ttypech') <$> lkupch)
                <>) . deleteSymTab (ch ^. bnfcIdentName) 

        lift $ tell $ bool [] [_NotInScope # ch] $ isNothing lkupch

        eqtype <- lift 
            $ fmap fst . splitGraphGenCore 
            $ tagConcTypeF 
            $ polarity chpol 
                ( _BnfcTypeGetF # (TypeVar (ttypesexpr ^. exprTtype) []
                , TypeVar ttypech' []))
                ( _BnfcTypePutF # (TypeVar (ttypesexpr ^. exprTtype) []
                , TypeVar ttypech' []) )
        let eqns = TypeEqnsExist [ttypesexpr ^. exprTtype, ttypech'] $ 
                [ TypeEqnsEq (TypeVar ttypech [], TypeConc eqtype) 
                , TypeEqnsStableRef (ttypesexpr ^. exprTtypeInternal, TypeVar (ttypesexpr ^. exprTtype) [])]
                ++ expreqns

            cmd' = CPut expr' ch'

        return (cmd' , bool [] [eqns] $ isJust lkupch)

    f (CSplit ch (ch0,ch1) ) = do
        chcxt <- guse tieProcChCxt 
        symtab <- guse tieProcSymTab
        tagmap <- gview equality

        ch0' <- lift $ tagBnfcIdent ch0
        ch1' <- lift $ tagBnfcIdent ch1
        ttypech0 <- lift freshTypeTag
        ttypech1 <- lift freshTypeTag

        let lkupch = lookup (ch ^. bnfcIdentName) chcxt
            lkupch' = fromJust lkupch
            ch' = (ch ^. bnfcIdentName, lkupch') ^. taggedChIdent

            ttypech = lkupch' ^. symEntryInfo % symChTypeTag
            chpol = lkupch' ^. symEntryInfo % symChPolarity

        tieProcChCxt %= (
                [ ( ch0' ^. bnfcIdentName
                    , _SymEntry # 
                        ( ch0' ^. uniqueTag
                        , ch0' ^. bnfcIdentPos
                        , SymChInfo ttypech0 chpol 
                        ) 
                    ) 
                , ( ch1' ^. bnfcIdentName
                    , _SymEntry # 
                        ( ch1' ^. uniqueTag
                        , ch1' ^. bnfcIdentPos
                        , SymChInfo ttypech1 chpol 
                        ) 
                    )
                ]<>) . deleteSymTab (ch ^. bnfcIdentName) 

        eqtype <- lift 
            $ fmap fst . splitGraphGenCore 
            $ tagConcTypeF 
            $ polarity chpol
                (_BnfcTypeTensorF # (TypeVar ttypech0 [], TypeVar ttypech1 [])) 
                (_BnfcTypeParF # (TypeVar ttypech0 [], TypeVar ttypech1 []))
            
        let cmd' = CSplit ch' (TaggedChIdent ch0' chpol, TaggedChIdent ch1' chpol)
            eqns = TypeEqnsExist [ttypech0, ttypech1] $
                [ TypeEqnsEq (TypeVar ttypech [], TypeConc eqtype) ]
                
        lift $ tell $ bool [] [_NotInScope # ch] $ isNothing lkupch

        return (cmd' , bool [] [eqns] $ isJust lkupch)

    f (CFork ch ((ch0, ch0with, cmds0), (ch1, ch1with, cmds1))) = do
        chcxt <- guse tieProcChCxt 
        symtab <- guse tieProcSymTab
        tagmap <- gview equality 

        let lkupch = lookup (ch ^. bnfcIdentName) chcxt
            lkupch' = fromJust lkupch
            ch' = (ch ^. bnfcIdentName, lkupch') ^. taggedChIdent

            -- this is actually useless..
            lkupch0with = traverse (\n -> (n ^. bnfcIdentName,) <$> lookup (n ^. bnfcIdentName) chcxt) ch0with
            lkupch1with = traverse (\n -> (n ^. bnfcIdentName,) <$> lookup (n ^. bnfcIdentName) chcxt) ch1with
            lkupch0with' = fromJust lkupch0with
            lkupch1with' = fromJust lkupch1with

            ttypech = lkupch' ^. symEntryInfo % symChTypeTag
            chpol = lkupch' ^. symEntryInfo % symChPolarity

        lift $ tell $ bool [] [_NotInScope # ch] $ isNothing lkupch

        ~ch0' <- lift $ flip TaggedChIdent chpol <$> tagBnfcIdent ch0
        ~ch1' <- lift $ flip TaggedChIdent chpol <$> tagBnfcIdent ch1
        ttypech0 <- lift freshTypeTag
        ttypech1 <- lift freshTypeTag

        tieProcChCxt %= deleteSymTab (ch ^. bnfcIdentName) 
        tieProcChCxt %= bool id (const lkupch0with') (isJust lkupch0with)
        tieProcChCxt %= (
                [ ( ch0' ^. bnfcIdentName
                    , _SymEntry # 
                        ( ch0' ^. uniqueTag
                        , ch0' ^. bnfcIdentPos
                        , SymChInfo ttypech0 chpol 
                        ) 
                    ) ] <>)
        (cmds0', cmds0eqns) <- cmdsIToGraph cmds0

        -- tieProcChCxt .= lkupch1with'
        tieProcChCxt %= bool id (const lkupch1with') (isJust lkupch1with)
        tieProcChCxt %= (
                [ ( ch1' ^. bnfcIdentName
                    , _SymEntry # 
                        ( ch1' ^. uniqueTag
                        , ch1' ^. bnfcIdentPos
                        , SymChInfo ttypech1 chpol 
                        ) 
                    ) ] <>)

        (cmds1', cmds1eqns) <- cmdsIToGraph cmds1

        eqtype <- lift 
            $ fmap fst . splitGraphGenCore 
            $ tagConcTypeF 
            $ polarity chpol
                (_BnfcTypeParF # (TypeVar ttypech0 [], TypeVar ttypech1 []))
                (_BnfcTypeTensorF # (TypeVar ttypech0 [], TypeVar ttypech1 [])) 

        let cmd' = CFork ch'
                ( ( ch0'
                    , zipWith (\ch0 -> view (_2 % taggedChIdent) . (ch0 ^. bnfcIdentName,)) 
                        ch0with lkupch0with'
                    , cmds0' )
                , ( ch1'
                    , zipWith (\ch1 ->  view (_2 % taggedChIdent) . (ch1 ^. bnfcIdentName,)) 
                        ch1with lkupch1with'
                    , cmds1' )
                )
            eqns = TypeEqnsExist [ttypech0, ttypech1] $
                [ TypeEqnsEq (TypeVar ttypech [], TypeConc eqtype) ]
                ++ cmds0eqns
                ++ cmds1eqns

        return (cmd', bool [] [eqns] $ isJust lkupch && isJust lkupch0with && isJust lkupch1with)
    f (CHPut ident () ch) = do
        chcxt <- guse tieProcChCxt 
        symtab <- guse tieProcSymTab
        tagmap <- gview equality 

            -- lookup phrase
        let lkup = lookupBnfcIdent ident $ mapMaybe 
                    ( traverseOf (_2 % symEntryInfo)
                        (preview (_SymConcCall % _SymPhrase))) 
                    symtab
            lkup' = fromJust lkup
            phraseg = lkup' ^. symEntryInfo 
            phrasetype = phraseg ^. phraseGObjType
            clause =  phraseg ^. typePhraseContext % phraseParent

            -- channel lookup
            lkupch = lookup (ch ^. bnfcIdentName) chcxt
            lkupch' = fromJust lkupch
            ch' = (ch ^. bnfcIdentName, lkupch') ^. taggedChIdent
            ttypech = lkupch' ^. symEntryInfo % symChTypeTag
            chpol = lkupch' ^. symEntryInfo % symChPolarity

            lkupexist = isJust lkup && isJust lkupch

        ttypech' <- lift freshTypeTag
        tieProcChCxt %= 
            (maybeToList (((ch ^. bnfcIdentName,) . set (symEntryInfo % symChTypeTag) ttypech') <$> lkupch) <>)
            . deleteSymTab (ch ^. bnfcIdentName) 

        -- not in scope tests
        lift $ tell $ bool [] [_NotInScope # ident] $ isNothing lkup
        lift $ tell $ bool [] [_NotInScope # ch] $ isNothing lkupch

        -- polarity checks
        lift $ tell $ bool [] [_HPutOutputPolarityExpectsProtocol # (ch, ident)] $ 
            lkupexist && has _ProtocolObj phrasetype && has _Input chpol
        lift $ tell $ bool [] [_HPutOutputPolarityExpectsProtocol # (ch, ident)] $ 
            lkupexist && has _CoprotocolObj phrasetype && has _Output chpol

        -- get the substitutions..
        ~(clausetype, ttypeargs, clausesubstitutions) <- lift . fmap fst . splitGraphGenCore $ 
                clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)

        let eqns = TypeEqnsExist (ttypech' : ttypeargs) $
                [ TypeEqnsEq (TypeVar ttypech [], clausetype)
                , TypeEqnsEq ( TypeVar ttypech' []
                    , fromJust $ substitutesTypeGToTypeGTypeTag clausesubstitutions $ 
                        polarity chpol 
                            (phraseg ^. typePhraseTo)
                            (fromJust $ phraseg ^? typePhraseFrom % _Cons % _1) 
                        ) ]
            cmd' = CHPut ((ident ^. bnfcIdentName, lkup') ^. taggedBnfcIdent) (ProtocolCoprotocolKnot phraseg) ch'

        return (cmd', bool [] [eqns] $ lkupexist)
    f (CHCase ch cases) = do
        tagmap <- gview equality 
        chcxt <- guse tieProcChCxt 
        symtab <- guse tieProcSymTab

            -- channel lookup
        let lkupch = lookup (ch ^. bnfcIdentName) chcxt
            lkupch' = fromJust lkupch
            ch' = (ch ^. bnfcIdentName, lkupch') ^. taggedChIdent
            ttypech = lkupch' ^. symEntryInfo % symChTypeTag
            chpol = lkupch' ^. symEntryInfo % symChPolarity

            -- phrase lookup
            phraseidents = fmap (view _1) cases
            lkups  = lookupsConcPhrases phraseidents symtab
            lkups' = sequenceA lkups
            (phraseg :| _) = fromJust lkups'

            casecmds = fmap (view _3) cases

        -- not in scope check..
        lift $ tell $ bool [] [_NotInScope # ch] $ isNothing lkupch
        lift $ tell $ fold $ zipWith (\ident -> maybe [ _NotInScope # ident] (const [])) 
                (NE.toList phraseidents) (NE.toList lkups)

        lift $ tell $ bool [] (hCaseChecks $ fromJust lkups') $ isJust lkups'

        -- get the substitutions..
        ~(clausetype, ttypeargs, clausesubstitutions) <- lift . fmap fst . splitGraphGenCore $ 
                clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)


        (cases', (ttypechs', phrasesg, cmdeqns)) <- fmap (second unzip3 . unzip) $
            traverse 
                (\(~(cmds, ~(Just phraseg))) -> do 
                    st <- guse equality

                    ttypech' <- lift freshTypeTag 
                    tieProcChCxt %= 
                        ((ch ^. bnfcIdentName, lkupch' & symEntryInfo % symChTypeTag .~ ttypech'):)
                        . deleteSymTab (ch ^. bnfcIdentName)
                    ~(cmds', cmdeqns) <- cmdsIToGraph cmds

                    equality .= st

                    return (cmds', (ttypech', phraseg, cmdeqns))
                ) 
            $ NE.toList $ NE.zip casecmds lkups

        tieProcChCxt .= mempty

        let eqns = TypeEqnsExist (ttypechs' ++ ttypeargs) $
                [ TypeEqnsEq (TypeVar ttypech [], clausetype) ] 
                ++ zipWith (\ttypech' phraseg -> 
                    TypeEqnsEq 
                        ( TypeVar ttypech' []
                        , fromJust 
                            $ substitutesTypeGToTypeGTypeTag clausesubstitutions 
                            $ polarity chpol 
                                (fromJust $ phraseg ^? typePhraseFrom % _Cons % _1) 
                                (phraseg ^. typePhraseTo)
                            ) 
                        ) 
                    ttypechs' phrasesg
                ++ concat cmdeqns
            cmd' = CHCase ch' 
                $ NE.fromList 
                $ zipWith3 (\(ident,_,_) phraseg c' -> 
                    ( phraseg ^. typePhraseName & taggedBnfcIdentPos .~ ident ^. bnfcIdentPos
                    , ProtocolCoprotocolKnot phraseg
                    , c') ) 
                        (NE.toList cases) phrasesg cases' 

        return (cmd' , bool [] [eqns] $ isJust lkupch && isJust lkups' )

    f (CId ch0 ch1) = do
        tagmap <- gview equality 
        chcxt <- guse tieProcChCxt 
        symtab <- guse tieProcSymTab

        let lkupch0 = lookup (ch0 ^. bnfcIdentName) chcxt
            lkupch0' = fromJust lkupch0
            ch0' = (ch0 ^. bnfcIdentName, lkupch0') ^. taggedChIdent
            ttypech0 = lkupch0' ^. symEntryInfo % symChTypeTag
            ch0pol = lkupch0' ^. symEntryInfo % symChPolarity

        let lkupch1 = lookup (ch1 ^. bnfcIdentName) chcxt
            lkupch1' = fromJust lkupch1
            ch1' = (ch1 ^. bnfcIdentName, lkupch1') ^. taggedChIdent
            ttypech1 = lkupch1' ^. symEntryInfo % symChTypeTag
            ch1pol = lkupch1' ^. symEntryInfo % symChPolarity

        lift $ tell $ bool [] [_NotInScope # ch0] $ isNothing lkupch0
        lift $ tell $ bool [] [_NotInScope # ch1] $ isNothing lkupch1

        lift $ tell $ bool [] [_IdExpectedDifferentPolarity # (ch0, ch1)] $ ch0pol == ch1pol

        let cmd' = CId ch0' ch1'
            eqns = TypeEqnsEq (TypeVar ttypech0 [], TypeVar ttypech1 [])
        
        tieProcChCxt .= mempty

        return (cmd', bool [] [eqns] $ isJust lkupch0 && isJust lkupch1)

    f (CIdNeg ch0 ch1) = do
        tagmap <- gview equality 
        chcxt <- guse tieProcChCxt 
        symtab <- guse tieProcSymTab

        let lkupch0 = lookup (ch0 ^. bnfcIdentName) chcxt
            lkupch0' = fromJust lkupch0
            ch0' = (ch0 ^. bnfcIdentName, lkupch0') ^. taggedChIdent
            ttypech0 = lkupch0' ^. symEntryInfo % symChTypeTag
            ch0pol = lkupch0' ^. symEntryInfo % symChPolarity

        let lkupch1 = lookup (ch1 ^. bnfcIdentName) chcxt
            lkupch1' = fromJust lkupch1
            ch1' = (ch1 ^. bnfcIdentName, lkupch1') ^. taggedChIdent
            ttypech1 = lkupch1' ^. symEntryInfo % symChTypeTag
            ch1pol = lkupch1' ^. symEntryInfo % symChPolarity

        lift $ tell $ bool [] [_NotInScope # ch0] $ isNothing lkupch0
        lift $ tell $ bool [] [_NotInScope # ch1] $ isNothing lkupch1

        lift $ tell $ bool [] [_IdExpectedDifferentPolarity # (ch0, ch1)] $ ch0pol /= ch1pol

        let cmd' = CIdNeg ch0' ch1'
            eqns = TypeEqnsEq (TypeVar ttypech0 [], TypeVar ttypech1 [])
        
        tieProcChCxt .= mempty

        return (cmd', bool [] [eqns] $ isJust lkupch0 && isJust lkupch1)

    f (CPlug plugs cmd0 cmd1 cmds) = do
        undefined
      where
        g s t [] = h s t 
        g s t (r:rs) = (<>) <$> (h s t) <*> g t r rs

        h :: (_, _) -> (_,_) -> TieProc [((_,_), (TypeTag, [TypeEqns TaggedBnfcIdent TypeTag]))]
        h (with0, cmds0) (with1, cmds1) = do
            symtab <- guse tieProcChCxt

            let common = with0 `intersect` with1 `intersect` plugs
            common' <- lift $ traverse tagBnfcIdent common

            let common0 = map (flip TaggedChIdent Output) common'
                common1 = map (flip TaggedChIdent Input) common'
            ttypescommon0 <- lift $ traverse (const freshTypeTag) common0
            ttypescommon1 <- lift $ traverse (const freshTypeTag) common1

            st <- guse equality 

            let lkups0 = map (flip lookup symtab . view bnfcIdentName) with0

            lift $ tell $ fold $ zipWith (\ident -> maybe [ _NotInScope # ident] (const [])) 
                with0 lkups0

            tieProcChCxt %= (
                    (zipWith (\nvar ttype -> 
                            ( nvar ^. bnfcIdentName
                            , nvar ^. toSymEntryChInfo ttype
                            )
                        ) 
                        common0 ttypescommon0)<>
                    )
            (cmds0', eqns0) <- cmdsIToGraph cmds0

            equality .= st

            tieProcChCxt %= (
                    (zipWith (\nvar ttype -> 
                            ( nvar ^. bnfcIdentName
                            , nvar ^. toSymEntryChInfo ttype
                            )
                        ) 
                        common1 ttypescommon1)<>
                    )
            (cmds1', eqns1) <- cmdsIToGraph cmds1

            -- return [ (cmds0', (ttypescommon0, eqns0)), (cmds1', (ttypescommon1, eqns1))]
            undefined

            

cmdsIToGraph ::
    NonEmpty (ProcessCommandI BnfcIdent) -> 
    TieProc
        ( NonEmpty (ProcessCommandG TaggedBnfcIdent TypeTag TaggedChIdent)
        , [TypeEqns TaggedBnfcIdent TypeTag] )
cmdsIToGraph = cata f
  where
    f (NonEmptyF cmd Nothing) = do
        (cmd', eqns)<- case cmd of
            CHalt _ -> cmdIToGraph cmd
            CFork _ _ -> cmdIToGraph cmd
            CHCase _ _ -> cmdIToGraph cmd
            CId _ _ -> cmdIToGraph cmd
            _ -> lift (tell [_IllegalLastInstruction # cmd]) >> cmdIToGraph cmd
        chcxt <- guse tieProcChCxt
        lift $ tell $ unclosedChannelsCheck chcxt
        return (cmd' :| [], eqns)
    f (NonEmptyF cmd (Just rst)) = do
        case cmd of
            CFork _ _ -> lift $ tell [_IllegalNonLastInstruction # cmd]
            CId _ _ -> lift $ tell [_IllegalNonLastInstruction # cmd]
            _ -> return ()


        (cmd', cmdeqns) <- cmdIToGraph cmd
        (rst', eqns) <- rst

        return 
            ( cmd' :| NE.toList rst'
            , bool 
                (cmdeqns ++ eqns) 
                (cmdeqns & mapped % _TypeEqnsExist % _2 %~ (++eqns))
                (folded % _TypeEqnsExist `has` cmdeqns)
            )

----------------------------
-- Functions
----------------------------
patternsIAndExprIToGraph ::
    ([PatternI BnfcIdent], ExprI BnfcIdent) ->
    TieFun ( ([PatternG TaggedBnfcIdent TypeTag], ExprG TaggedBnfcIdent TypeTag TaggedChIdent)
        , TypeEqns TaggedBnfcIdent TypeTag )
patternsIAndExprIToGraph (patts, expr) = do
    symtab <- guse equality

    ttype <- gview exprTtype
    ttypeinternal <- gview exprTtypeInternal
    tagmap <- gview tieExprEnvTagTypeMap

    -- tag the patterns expression with a type..
    (ttypespatts, (ttypesexpr, expr)) <- lift $ do
            ttypespatts <- traverse (const freshExprTypeTags) patts
            ttypesexpr <- freshExprTypeTags
            return (zip ttypespatts patts, (ttypesexpr, expr))

    -- equations, and graph pattern from the type patterns
    -- Note: this modifies the symbol table..
    (patteqns, pattsg) <- unwrappedPatternsIToGraph ttypespatts

    (exprg, expreqns) <- local (set tieExprEnvTypeTags ttypesexpr) (exprIToGraph expr)

    equality .= symtab

    let ttypepatts = map (view exprTtype . fst ) ttypespatts
        ttypeexpr = ttypesexpr ^. exprTtype
        ttypeeqn = TypeEqnsExist (ttypeexpr : ttypepatts) $ 
                [ TypeEqnsEq 
                    ( TypeVar ttype []
                    , TypeSeq $ TypeSeqArrF 
                        (map (flip TypeVar []) ttypepatts) 
                        (TypeVar ttypeexpr []) 
                    ) 
                , TypeEqnsStableRef 
                    ( ttypeinternal, TypeVar ttype [] ) 
                    ]
                ++ patteqns ++ expreqns

    return ((pattsg, exprg), ttypeeqn)

exprsIToGraph :: 
    -- | Expression to convert to a graph
    [(ExprTypeTags, ExprI BnfcIdent)] ->
    -- | result
    TieFun [( ExprG TaggedBnfcIdent TypeTag TaggedChIdent, [TypeEqns TaggedBnfcIdent TypeTag] )]
exprsIToGraph ttypesexprs = do
    traverse 
        (\(tag, expr) -> local (set tieExprEnvTypeTags tag) $ exprIToGraph expr ) 
        ttypesexprs 

unwrappedExprsIToGraph ::
    -- | Expression to convert to a graph
    [(ExprTypeTags, ExprI BnfcIdent)] ->
    -- | result
    TieFun
        ( [ExprG TaggedBnfcIdent TypeTag TaggedChIdent]
        , [TypeEqns TaggedBnfcIdent TypeTag])
unwrappedExprsIToGraph ttypesexprs =  
    second mconcat . unzip <$> exprsIToGraph ttypesexprs

------------------------
-- compiling expressions...
------------------------
exprIToGraph :: 
    -- | Expression to convert to a graph
    ExprI BnfcIdent ->
    -- | result
    TieFun ( ExprG TaggedBnfcIdent TypeTag TaggedChIdent, [TypeEqns TaggedBnfcIdent TypeTag] )
exprIToGraph expr = 
    f expr
  where 
    -- think carefully about how we will do the built in types...
    -- right now, the implementation is bad...
    {-
    f ttype (EIf eif ethen eelse ()) = do
        ttypeif <- freshTypeTag
        ttypethen <- freshTypeTag
        ttypeelse <- freshTypeTag

        (eifg, eifeqns) <- f ttypeif eif
        (etheng, etheneqns) <- f ttypethen ethen
        (eelseg, eelseeqns) <- f ttypeelse eelse

        return 
            ( EIf eifg etheng eelseg $ fromJust $ Map.lookup ttype tagmap
            , TypeEqnsExist 
                [ttypeif, ttypethen, ttypeelse] $ 
                [ TypeEqnsEq (TypeVar ttypeif [], TypeSeq TypeBoolF)
                , TypeEqnsEq (TypeVar ttype [], TypeVar ttypethen [])
                , TypeEqnsEq (TypeVar ttype [], TypeVar ttypeelse [])
                , eifeqns
                , etheneqns
                , eelseeqns                
                ]
            )
    f ttype (EOp larg op rarg ()) = 
        -- TODO
        error "operators not implemented in type checking yet"
        -}

    f (ELet letdef letexpr ()) = do
        symtable <- guse equality 
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        -- compiling the let statemtens..
        letdef' <- lift . join . fmap writer . splitGraphGenCore 
            $ runReaderT (traverse stmtIToGraph letdef) symtable 
        let nsyms = concatMap collectSymEntries letdef'

        -- updating the symbol table
        equality %= (nsyms++)

        -- updating the expression
        ttypesletexpr <- lift freshExprTypeTags
        (letexprg, exprgeq) <- local (set tieExprEnvTypeTags ttypesletexpr) $ exprIToGraph letexpr

        let ttypeletexpr = ttypesletexpr ^. exprTtype
            eqns = TypeEqnsExist [ttypeletexpr] $
                [ TypeEqnsStableRef ( ttypeinternal , TypeVar ttype [] )
                , TypeEqnsEq ( TypeVar ttype [] 
                             , TypeVar ttypeletexpr []) ]
                ++ exprgeq

        return 
            ( ELet letdef' letexprg $ fromJust $ Map.lookup ttypeinternal tagmap
            , [eqns] )

    f (EConstructorDestructor ident () args ()) = do
        symtab <- guse equality

        let lkup = lookupBnfcIdent ident 
                $ mapMaybe 
                    ( traverseOf (_2 % symEntryInfo)
                        (preview (_SymSeqCall % _SymPhrase))) 
                symtab

            SymEntry tag pos phraseg = fromJust lkup

        ttype <- gview exprTtype 
        ttypeinternal <- gview exprTtypeInternal 

        tagmap <- gview tieExprEnvTagTypeMap 

        lift $ tell $ bool [] [_NotInScope # ident] (isNothing lkup)

        -- arity check
        let expectedarity = length (phraseg ^. typePhraseFrom)
            actualarity   = length args
        lift $ tell $ bool 
                [_ArityMismatch # (ident, expectedarity, actualarity)]
                []
                (isNothing lkup || expectedarity == actualarity)

        --  query the phrase substitutions from the graph
        ~(clausetype, ttypeargs, clausesubstitutions) <- lift . fmap fst . splitGraphGenCore $ 
                clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)
        
        -- fresh type vars for the constructor args
        ttypesctsargs <- lift $ traverse (const freshExprTypeTags) args

        (argsexprgs, argseqns) <- unwrappedExprsIToGraph $ zip ttypesctsargs args

        let ttypectsargs = map (view exprTtype) ttypesctsargs
            typeeqs = TypeEqnsExist (ttypectsargs ++ ttypeargs) $
                [ TypeEqnsEq ( _TypeVar # (ttype, []) 
                    , fromJust 
                        $ substitutesTypeGToTypeGTypeTag clausesubstitutions 
                        $ phraseg ^. typePhraseTo
                    )
                , TypeEqnsStableRef (ttypeinternal, TypeVar ttype [])] 
                ++ zipWith g ttypectsargs (phraseg ^. typePhraseFrom)
                ++ argseqns
            g typetag ctsargtype = TypeEqnsEq
                ( TypeVar typetag []
                , fromJust $ substitutesTypeGToTypeGTypeTag 
                    clausesubstitutions ctsargtype )
            expr' = EConstructorDestructor 
                (_TaggedBnfcIdent # (ident, tag)) 
                (ConstructorDestructorKnot phraseg)
                -- TODO, we can get more type safety here!
                -- we know that this will always be a 
                -- constructor / destructor
                argsexprgs 
                $ fromJust $ Map.lookup ttypeinternal tagmap
        return ( expr', bool [typeeqs] [] $ isNothing lkup )

    f (ECase caseon cases ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        -- converting the case on to a graph...
        -- ttypecaseon <- lift freshTypeTag
        ttypescaseon <- lift freshExprTypeTags 
        (caseong, caseongeqns) <- local (set tieExprEnvTypeTags ttypescaseon) $ exprIToGraph caseon

        -- converting the case phrases to a graph..
        ttypetaggedcases <- lift $ traverse 
            (\(patt, exp) -> (,) 
                <$> ((,patt) <$> freshExprTypeTags) 
                <*> ((,exp)  <$> freshExprTypeTags)) 
            cases

        (pattsgexprsg, eqns) <- unzip <$> traverse 
            (\((ttypepatt, patt), (ttypeexpr, expr)) -> do
                symtable <- guse equality

                (patteqns, pattsg) <- local (set tieExprEnvTypeTags ttypepatt) $ patternIToGraph patt
                (exprg, expreqn) <- local (set tieExprEnvTypeTags ttypeexpr) $ exprIToGraph expr

                equality .= symtable

                -- return ((pattsg, exprg), expreqn:patteqns) )
                return ((pattsg, exprg), expreqn ++ patteqns) )

                (NE.toList ttypetaggedcases)


        let expr' = ECase caseong (NE.fromList pattsgexprsg) $ fromJust $ Map.lookup ttypeinternal tagmap
            -- patt typing info
            ttypepatts  = NE.toList $ fmap (view exprTtype . fst . fst) ttypetaggedcases
            ttypeexprs  = NE.toList $ fmap (view exprTtype . fst . snd) ttypetaggedcases
            ttypecaseon = ttypescaseon ^. exprTtype
            typeeqs = TypeEqnsExist 
                (ttypecaseon : ttypepatts ++ ttypeexprs)
                $ [ TypeEqnsStableRef ( ttypeinternal , TypeVar ttype [] ) ]
                ++ map (TypeEqnsEq . (TypeVar ttypecaseon [],) . flip TypeVar []) ttypepatts
                    -- ^ the type being cased on must be the same as the patterns 
                ++ map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypeexprs
                    -- ^ the type of the expression is the same as the result of the case
                ++ caseongeqns
                ++ concat eqns

        return (expr', [typeeqs])

    f (EVar ident ()) = f (ECall ident () [] ())
        -- remark:
        -- Indeed, a variable could really be referring to a destructor
        -- call from pattern matching against a codata type.. Hence, 
        -- we let the ECall case resolve this issue...
        {-
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtable <- guse equality

        lkup <- lift $ 
            runSymbolTableQuery 
            (queryBnfcIdentName ident >> queryLocalSeqVar >> queryChecks >> symbolTableQuery) 
            symtable
        let lkup = findOf ((ident ^. bnfcIdentName==) . fst) 
                $ mapMaybe (preview _SymSeqCal) symtable
            ~(SymEntry tag pos ~(SymSeqConcType ttypelkup)) = fromJust lkup 
            eqns = 
                [ TypeEqnsEq (TypeVar ttype [], TypeVar ttypelkup [])
                , TypeEqnsStableRef (ttypeinternal, TypeVar ttype []) ] 
        lift tell $ bool [] [_NotInScope # ident] (isNothing lkup)

        return 
            ( EVar (TaggedBnfcIdent ident tag) $ fromJust $ Map.lookup ttypeinternal tagmap
            , bool eqns [] (isNothing lkup) )
            -}

    f (ECall ident () args ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtab <- guse equality

        let lkup = lookupBnfcIdent ident 
                $ mapMaybe
                    ( traverseOf (_2 % symEntryInfo)
                        ( preview (_SymSeqCall % _SymCall) )
                    )
                symtab
            SymEntry tag pos (calltype, calldef) = fromJust lkup

        -- check if in scope
        lift $ tell $ bool [] [_NotInScope # ident ] $ isNothing lkup

        ~(calltype', callttypeargs) <- lift . fmap fst . splitGraphGenCore $ case calltype of
            SymCallDummyTypeVar t -> pure $ (TypeVar t [], [])
            SymCallExplicitType t -> do
                (args, argssubs ) <- genTypeGSubs t
                return (fromJust $ substitutesTypeGToTypeGTypeTag argssubs t, args)

        -- compile the arguments..
        ttypesargs <- lift $ traverse (const freshExprTypeTags) args
        (argsexprgs, argseqns) <- unwrappedExprsIToGraph $ zip ttypesargs args

        -- (calltypevars, calltypesubs) <- lift . fmap fst . splitGraphGenCore $ genTypeGSubs (traceShowId calltype)

        let ttypeargs = map (view exprTtype) ttypesargs
            eqns = TypeEqnsExist (ttypeargs ++ callttypeargs) $
                [ TypeEqnsEq 
                    ( calltype'
                    , simplifyArrow 
                        $ TypeSeq 
                        $ TypeSeqArrF (map (flip TypeVar []) ttypeargs) (TypeVar ttype []) )
                , TypeEqnsStableRef ( ttypeinternal, TypeVar ttype [] )
                    ]
                ++ argseqns
        return 
            ( ECall 
                (TaggedBnfcIdent ident tag) 
                calldef argsexprgs 
                $ fromJust $ Map.lookup ttypeinternal tagmap
            , bool [eqns] [] (isNothing lkup) )

    f (ERecord recordphrases ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtab <- guse equality

        -- mostly duplicated code from record patterns
        let recordphraseidents = fmap fst recordphrases
            lkups = lookupsSeqPhrases recordphraseidents symtab
            lkups' = sequenceA lkups
            phrasesg = fromJust lkups'
            ~(focusedclauseg :| _) = fmap (view (typePhraseContext % phraseParent)) phrasesg

        -- not in scope checks...
        lift $ tell $ fold $ NE.zipWith (\(ident, _) -> maybe [ _NotInScope # ident] (const [])) 
                recordphrases lkups

        -- sanity checks for valid record phrases
        lift $ tell $ maybe [] (const (validRecordPhrasesCheck phrasesg)) lkups'

        --  query the phrase substitutions from the graph
        ~(clausetype, ttypeclauseargs, clausesubstitutions) <- lift . fmap fst . splitGraphGenCore $ 
            clauseSubstitutions focusedclauseg

        (recordphrases', (ttypesexprs, destypes, exprseqns)) <- second unzip3 . unzip . catMaybes <$> traverse 
            (\((ident, ((), pattsexp)), phrasegdef) -> do
                    ttypesexpr <- lift freshExprTypeTags
                    (pattsexp', typeeqns) <- local 
                        (set tieExprEnvTypeTags ttypesexpr) 
                            $ patternsIAndExprIToGraph pattsexp
                
                    return $ case phrasegdef of
                        Just phrasegdef -> Just 
                            ( ( TaggedBnfcIdent ident $ phrasegdef ^. typePhraseName % uniqueTag
                              , (ConstructorDestructorKnot phrasegdef, pattsexp') )
                            , ( ttypesexpr
                              , fromJust 
                                    $ substitutesTypeGToTypeGTypeTag clausesubstitutions 
                                    -- note that we do not simplify the arrow here!
                                    -- indeed, we just match this against the destructor...
                                    -- $ simplifyArrow 
                                    $ TypeSeq 
                                    $ TypeSeqArrF 
                                        (init $ phrasegdef ^. typePhraseFrom) 
                                        -- init is needed to remove the last type variable
                                        (phrasegdef ^. typePhraseTo)
                              , typeeqns ) 
                            )  
                        Nothing -> Nothing
                    )
            (zip (NE.toList recordphrases) (NE.toList lkups) )

        let ttypeexprs = map (view exprTtype) ttypesexprs
            eqns = TypeEqnsExist (ttypeclauseargs ++ ttypeexprs) $
                [ TypeEqnsEq (TypeVar ttype [], clausetype) 
                , TypeEqnsStableRef (ttypeinternal, TypeVar ttype []) ]
                ++ zipWith (\ttypeexpr -> TypeEqnsEq . (TypeVar ttypeexpr [],)) 
                    ttypeexprs destypes
                -- retain the accumlated type equations
                ++ exprseqns
            expr = ERecord 
                (NE.fromList recordphrases')
                $ fromJust $ Map.lookup ttypeinternal tagmap

        return (expr, bool [eqns] [] (isNothing lkups'))

    f (EFold foldon phrases ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtab <- guse equality

        -- compile the expression to be folded on first..
        ttypesfoldon <- lift freshExprTypeTags
        let ttypefoldon = ttypesfoldon ^. exprTtype
        (foldon', ttypefoldoneqns) <- local (set tieExprEnvTypeTags ttypesfoldon) (exprIToGraph foldon)

        -- compiling the phrases
        let phraseidents = fmap (view foldPhraseIdentF) phrases
            lkups = lookupsSeqPhrases phraseidents symtab
            lkups' = sequenceA lkups
            phrasesg@(focusedphrase :| _) = fromJust lkups'
            graph = focusedphrase ^. phraseGClausesGraph

        -- not in scope check..
        lift $ tell $ fold $ zipWith (\ident -> maybe [ _NotInScope # ident] (const [])) 
                (NE.toList phraseidents) (NE.toList lkups)

        -- not in scope check..
        lift $ tell $ bool [] (validFoldTypePhrasesCheck phrasesg) (isJust lkups')
                
        -- compute the type variables and the appropriate substitutions
        ~(ttypegraphs, substitutions) <- lift . fmap fst . splitGraphGenCore $  
                clauseGraphFreshSubstitutions graph

        (ttypesfoldphrases, phrasetypes, phraseseqns, foldphrases') <- 
                foldPhrasesWithTypePhrasesToGraph (NE.toList phrases) (NE.toList lkups)

        let ttypefoldon = ttypesfoldon ^. exprTtype
            ttypefoldphrases = map (view exprTtype) ttypesfoldphrases

            -- from Prashant, we know that the type of the expression being folded on
            -- foldon is the type clause of the first phrase...
            focusedclauseg = focusedphrase ^. typePhraseContext % phraseParent
            focusedclausegtype = _TypeWithArgs # 
                ( focusedclauseg ^. typeClauseName
                , TypeClauseCallDefKnot focusedclauseg 
                , map (review _TypeVar . (,[])) $ focusedclauseg ^. typeClauseArgs )
            focusedclausegtype' = fromJust $ substitutesTypeGToTypeGTypeTag substitutions focusedclausegtype 
            ttypefocusedclause = fromJust $ lookup (focusedclauseg ^. typeClauseStateVar) substitutions

            eqns = TypeEqnsExist (ttypefoldon : ttypegraphs ++ ttypefoldphrases) $ 
                [ TypeEqnsEq (TypeVar ttypefoldon [], focusedclausegtype')
                , TypeEqnsEq (TypeVar ttype [],  ttypefocusedclause)
                , TypeEqnsStableRef (ttypeinternal, TypeVar ttype []) ]
                ++ zipWith (\foldphrasettypes -> TypeEqnsEq 
                        . (TypeVar (foldphrasettypes ^. exprTtype) [],)
                        . fromJust
                        . substitutesTypeGToTypeGTypeTag substitutions
                    )
                    ttypesfoldphrases phrasetypes 
                ++ ttypefoldoneqns
                ++ phraseseqns
                
            expr = EFold foldon' (NE.fromList foldphrases') $ fromJust $ Map.lookup ttypeinternal tagmap
        return (expr, bool [] [eqns] (isJust lkups'))

    f (EUnfold unfoldon unfoldphrases ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtab <- guse equality

        -- compile the expression to be folded on first..
        ttypesunfoldon <- lift freshExprTypeTags
        let ttypeunfoldon = ttypesunfoldon ^. exprTtype

        (unfoldon', ttypeunfoldeqns) <- local (set tieExprEnvTypeTags ttypesunfoldon) (exprIToGraph unfoldon)

        let foldphrases       = fmap (view unfoldPhraseFolds) unfoldphrases
            foldphrasesidents = fmap (fmap (view foldPhraseIdentF)) foldphrases 
            foldphraseslkups  = fmap (`lookupsSeqPhrases` symtab) foldphrasesidents 
            foldphraseslkups' = sequenceA $ fmap sequenceA foldphraseslkups  
            foldphraseslkups''@((focusedphraseg :| _) :| _) = fromJust $ foldphraseslkups'
            focusedclauseg = focusedphraseg ^. typePhraseContext % phraseParent
                -- the type of the unfold expression clause type of the first phrase

        -- not in scope check..
        lift $ traverse (\(phraseidents, lkups) -> tell $ fold $ 
            zipWith (\ident -> maybe [ _NotInScope # ident] (const [])) 
                (NE.toList phraseidents) (NE.toList lkups))
            $ zip (NE.toList foldphrasesidents) (NE.toList foldphraseslkups)

        -- error checking..
        lift $ tell $ bool []
            (validUnfoldTypePhrasesCheck foldphraseslkups'')
                (isJust foldphraseslkups')

        ~(ttypegraph, subs) <- lift . fmap fst . splitGraphGenCore 
            $ clauseGraphFreshSubstitutions (focusedphraseg ^. phraseGClausesGraph)
        let focusedclausetype = fromJust 
                $ substitutesTypeGToTypeGTypeTag subs
                $ TypeWithArgs 
                    (focusedclauseg ^. typeClauseName)
                    (TypeClauseCallDefKnot focusedclauseg)
                    (focusedclauseg ^.. typeClauseArgs % traversed % to (review _TypeVar . (,[])) )
            ttypefocusedstatevar = fromJust $ lookup (focusedclauseg ^. typeClauseStateVar) subs

        (ttypespatts, ttypeclausestatevars, patteqns, ttypesfoldphrases, phrasetypes, phraseseqns, unfoldphrases') <- fmap unzip7 $ traverse   
            (\(UnfoldPhraseF patt foldphrases, lkups) -> do
                symtab <- guse equality

                ttypespatt <- lift freshExprTypeTags
                (patteqns, patt') <- local (set tieExprEnvTypeTags ttypespatt) $ patternIToGraph patt

                let lkups' = sequenceA lkups
                    phrasesg@(focusedphrase :| _) = fromJust lkups'
                    graph = focusedphrase ^. phraseGClausesGraph

                (ttypesfoldphrases, phrasetypes, phraseseqns, foldphrases') <- 
                        foldPhrasesWithTypePhrasesToGraph (NE.toList foldphrases) (NE.toList lkups)
                
                equality .= symtab

                return 
                    ( ttypespatt 
                    , fromJust $ 
                        lookup (focusedphrase ^. typePhraseContext % phraseParent % typeClauseStateVar) 
                            subs
                    , patteqns
                    , ttypesfoldphrases
                    -- we need eliminate the last state variable in the input args
                    -- since we are building up the records with an unfold -- we do this
                    -- with init which is safe since we build all codata with at least
                    -- one element and check to see if the last element is a state variable
                    , phrasetypes & traversed % _TypeSeq % _TypeSeqArrF % _1 %~ init
                    , phraseseqns
                    , UnfoldPhraseF patt' (NE.fromList foldphrases')
                    )
                )
            $ zip (NE.toList unfoldphrases) (NE.toList foldphraseslkups)

        let ttypepatts = ttypespatts & mapped %~ view exprTtype
            ttypefoldphrases = ttypesfoldphrases & mapped % mapped %~ view exprTtype
            eqns = TypeEqnsExist (ttypeunfoldon : ttypegraph ++ ttypepatts ++ mconcat ttypefoldphrases) $
                [ TypeEqnsEq (TypeVar ttype [], focusedclausetype) 
                , TypeEqnsStableRef (ttypeinternal, TypeVar ttype []) 
                -- similar to what Prashant did for the fold, we say that
                -- the type of the seed should be the same as the focused first
                -- clause...
                , TypeEqnsEq (TypeVar ttypeunfoldon [], ttypefocusedstatevar) ]

                -- the types of the patterns should match the state variables for the clauses which
                -- they focus on..
                ++ zipWith (\ttypepatt -> TypeEqnsEq . (TypeVar ttypepatt [],)) 
                        ttypepatts ttypeclausestatevars
                
                -- set the foldphrases to be equal to the the type phrase type...
                ++ concat (zipWith (zipWith 
                    (\ttypefoldphrase -> 
                        TypeEqnsEq 
                        . (TypeVar ttypefoldphrase [],)
                        . fromJust 
                        . substitutesTypeGToTypeGTypeTag subs)
                        ) ttypefoldphrases phrasetypes)
                -- accumlate the unfoldon equations
                ++ ttypeunfoldeqns
                -- accumlate the patterns from the unfold
                ++ concat patteqns
                -- accumlate equations from the phrases...
                ++ concat phraseseqns
            expr = EUnfold unfoldon' (NE.fromList unfoldphrases') $ fromJust $ Map.lookup ttypeinternal tagmap

        return (expr, bool [] [eqns] (isJust foldphraseslkups'))

    f n = error $ show n

-- Takes an foldphrase and the corresponding Maybe TypePhrase (if it exists)
-- and computes the: ttype, graph type of the typephrase, type equations, and graph
-- foldphrase 
foldPhrasesWithTypePhrasesToGraph foldphrases typephrases = fmap (unzip4 . catMaybes) 
    $ traverse f 
    $ zip foldphrases typephrases
  where
    f (FoldPhraseF ident () pattargs fexpr, lkup) = do
        ttypesfoldphrase <- lift freshExprTypeTags

        ((pattargs', fexpr'), eqns) <- local 
                (set tieExprEnvTypeTags ttypesfoldphrase) 
                $ patternsIAndExprIToGraph (pattargs, fexpr)

        return $ case lkup of
            Just phraseg -> Just
                ( ttypesfoldphrase
                , TypeSeq $ TypeSeqArrF (phraseg ^. typePhraseFrom) (phraseg ^. typePhraseTo)
                , eqns 
                , FoldPhraseF 
                    (TaggedBnfcIdent ident (phraseg ^. typePhraseName % uniqueTag))
                    (ConstructorDestructorKnot phraseg) 
                    pattargs' 
                    fexpr' )
            Nothing -> Nothing

--------------------------------------------------
-- pattern compilation
--------------------------------------------------
patternsIToGraph :: 
    [(ExprTypeTags, PatternI BnfcIdent)] -> 
    TieFun [ ( [TypeEqns TaggedBnfcIdent TypeTag] , PatternG TaggedBnfcIdent TypeTag) ]
patternsIToGraph tagspatts = traverse 
    (\(tag, patt) -> 
        local (set tieExprEnvTypeTags tag) (patternIToGraph patt)) 
    tagspatts

unwrappedPatternsIToGraph ::
    [(ExprTypeTags, PatternI BnfcIdent)] -> 
    TieFun ( [TypeEqns TaggedBnfcIdent TypeTag] , [PatternG TaggedBnfcIdent TypeTag] )
unwrappedPatternsIToGraph patts = 
    (first mconcat <<< unzip) <$> patternsIToGraph patts

patternIToGraph :: 
    PatternI BnfcIdent -> 
    TieFun 
        ( [TypeEqns TaggedBnfcIdent TypeTag]
        , PatternG TaggedBnfcIdent TypeTag )
patternIToGraph pattern = 
    f pattern
  where
    f :: PatternI BnfcIdent -> TieFun ([TypeEqns TaggedBnfcIdent TypeTag] , PatternG TaggedBnfcIdent TypeTag)
    f (PConstructor ident () ctsargs ()) = do
        symtab <- guse equality

        let lkup = lookupBnfcIdent ident 
                $ mapMaybe 
                    ( traverseOf (_2 % symEntryInfo)
                        (preview (_SymSeqCall % _SymPhrase))) 
                $ symtab
            SymEntry tag pos phraseg = fromJust lkup

        lift $ tell $ bool [] [_NotInScope # ident] (isNothing lkup)

        -- check if it is a data type
        lift $ tell $ 
            bool [_ExpectedDataConstructor # ident] []
                (isNothing lkup || has _DataObj (phraseg ^. phraseGObjType))

        let expectedarity = length (phraseg ^. typePhraseFrom)
            actualarity = length ctsargs
        lift $ tell $ 
            bool [_ArityMismatch # (ident, expectedarity, actualarity)] []
                (isNothing lkup || expectedarity == actualarity)

        -- fresh type vars for the constructor args
        ttypesctsargs <- lift $ traverse (const freshExprTypeTags) ctsargs
        let ttypectsargs = map (view exprTtype) ttypesctsargs

        --  query the phrase substitutions from the graph
        ~(clausetype, ttypeargs, clausesubstitutions) <- lift . fmap fst . splitGraphGenCore $ 
            clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)

        (ctsargstypeeqs, ctsargspatts) <- 
            unwrappedPatternsIToGraph $ zip ttypesctsargs ctsargs

        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap


        let typeeqs = TypeEqnsExist (ttypectsargs ++ ttypeargs) $
                [ TypeEqnsEq ( TypeVar ttype [] , clausetype )
                , TypeEqnsStableRef ( ttypeinternal, TypeVar ttype []) ] 
                ++ zipWith g ttypectsargs (phraseg ^. typePhraseFrom)
                ++ ctsargstypeeqs
            g typetag ctsargtype = TypeEqnsEq
                ( _TypeVar # (typetag, [])
                , fromJust $ substitutesTypeGToTypeGTypeTag 
                    clausesubstitutions ctsargtype )
            pat' = PConstructor 
                (_TaggedBnfcIdent # (ident, tag)) 
                phraseg ctsargspatts (fromJust $ Map.lookup ttypeinternal tagmap)

        return ( bool [typeeqs] [] (isNothing lkup), pat' )

        {-
    f ttype (PUnit ident ()) = do
        -- tag doesn't matter for constants...
        ident' <- tagBnfcIdent ident
        return 
            ( (mempty, [TypeEqnsEq (_TypeVar # (ttype, []), TypeSeq $ TypeUnitF ident')] )
            , PUnit ident' $ fromJust $ Map.lookup ttype tagmap )

        -}

    f (PRecord recordphrases ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtab <- guse equality

        -- lookup each of the record phrases
        -- i.e., given (Destructor1 := n1, .., Destructorp := np), it will
        -- look up the destructors Destructor1 .. Destructorp
        let recordphraseidents = fmap fst recordphrases
            lkups = lookupsSeqPhrases recordphraseidents symtab
            lkups' = sequenceA lkups
            phrasesg = fromJust lkups'
            -- get the type clause corresponding to the first phrase (which
            -- should be the same as the rest of the other phrases.....
            ~(focusedclauseg :| _) = fmap (view (typePhraseContext % phraseParent)) phrasesg

        -- not in scope checks...
        lift $ tell $ fold $ NE.zipWith (\(ident, _) -> maybe [ _NotInScope # ident] (const [])) 
                recordphrases lkups

        lift $ tell $ bool [] (validRecordPhrasesCheck phrasesg) (isJust lkups')
            
        --  query the phrase substitutions from the graph
        ~(clausetype, ttypeclauseargs, clausesubstitutions) <- lift . fmap fst . splitGraphGenCore $ 
            clauseSubstitutions focusedclauseg

        -- could use unwrappedPatternsIToGraph here...
        (recordphrases', (ttypespatts, destypes, pattseqns)) <- second unzip3 . unzip . catMaybes <$> traverse 
            (\((ident, ((), patt)), phrasegdef) -> do
                    ttypespatt <- lift freshExprTypeTags
                    (typeeqns, patt') <- local 
                        (set tieExprEnvTypeTags ttypespatt) 
                        (patternIToGraph patt)
                
                    return $ case phrasegdef of
                        Just phrasegdef -> Just 
                            ( ( TaggedBnfcIdent ident $ phrasegdef ^. typePhraseName % uniqueTag
                              , (phrasegdef, patt') )
                            , ( ttypespatt
                              , fromJust 
                                    $ substitutesTypeGToTypeGTypeTag clausesubstitutions 
                                    $ simplifyArrow 
                                    $ TypeSeq 
                                    $ TypeSeqArrF 
                                        (init $ phrasegdef ^. typePhraseFrom) 
                                        -- init is needed to remove the last type variable
                                        (phrasegdef ^. typePhraseTo)
                              , typeeqns ) 
                            )  
                        Nothing -> Nothing
                    )
            (zip (NE.toList recordphrases) (NE.toList lkups))

        let ttypepatts = map (view exprTtype) ttypespatts
            eqns = TypeEqnsExist (ttypepatts ++ ttypeclauseargs) $
                    [ TypeEqnsEq (TypeVar ttype [], clausetype)
                    , TypeEqnsStableRef (ttypeinternal, TypeVar ttype [])]
                    ++ zipWith 
                        (\ttypepat -> TypeEqnsEq . (TypeVar ttypepat [],)) 
                        ttypepatts 
                        destypes
                    -- accumlate all the previously accumlated equations..
                    ++ concat pattseqns
            patt' = PRecord (NE.fromList recordphrases') $ fromJust $ Map.lookup ttypeinternal tagmap

        return ( bool [eqns] [] (isNothing lkups') , patt' )

    {-
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
-}

    f ( PVar ident () ) = do
        ttype <- gview (exprTypeTags % exprTtype)
        ttypeinternal <- gview (exprTypeTags % exprTtypeInternal)
        tagmap <- gview tieExprEnvTagTypeMap

        ident' <- lift $ tagBnfcIdent ident
        let typeeqs = 
                [ TypeEqnsStableRef (ttypeinternal , TypeVar ttype []) ]
            pat' = PVar ident' $ fromJust $ Map.lookup ttypeinternal tagmap

        equality %= (
            ( ident' ^. taggedBnfcIdentName
            , SymEntry (ident' ^. uniqueTag) (ident' ^. taggedBnfcIdentPos) 
                $ _SymSeqCall # _SymCall # (SymCallDummyTypeVar ttype, LocalVar) ): )

        return ( typeeqs, pat' )

    f (PNull ident ()) = do
        -- tags for null tags do not matter
        ttype <- gview (exprTypeTags % exprTtype)
        ttypeinternal <- gview (exprTypeTags % exprTtypeInternal)
        tagmap <- gview tieExprEnvTagTypeMap

        ident' <- lift $ tagBnfcIdent ident
        return 
            ( [TypeEqnsStableRef (ttypeinternal, TypeVar ttype [])]
            , PNull ident' $ fromJust $ Map.lookup ttypeinternal tagmap)
