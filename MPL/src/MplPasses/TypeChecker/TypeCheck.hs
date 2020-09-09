{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.TypeCheck where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked 

import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckMplTypeSub 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeEqns
import MplPasses.TypeChecker.TypeCheckObj 
import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeCheckPatt 
import MplPasses.TypeChecker.TypeCheckPanic
import MplPasses.TypeChecker.TypeCheckErrorPkg
import MplPasses.TypeChecker.TypeCheckSymUtils 
import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Control.Arrow
import Control.Applicative
import Data.Semigroup
import Data.Maybe
import Data.Bool
import Data.Function
import Data.Traversable

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List
import Debug.Trace

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable

import Data.Functor.Foldable (Base, cata, para)
import Data.Tuple

runTypeCheck' ::
    ( AsAllTypeCheckErrors err ) =>
    (TopLevel, UniqueSupply) ->
    MplProg MplRenamed ->
    Either [err] (MplProg MplTypeChecked)
runTypeCheck' ~(top, sup) = 
    \case 
        (res, []) -> Right res
        (_, errs) -> Left errs
    . runWriter 
    . (`evalStateT` ( _Env # 
            ( top
            , lsup
            , mempty
            , TypeInfoEnv mempty tag mempty)
        )
      )
    . runTypeCheck
  where
    ~(lsup, rsup) = split sup
    tag = evalState freshTypeTag rsup

runTypeCheck ::
    forall e m0 m1 symm n. 
    ( AsAllTypeCheckErrors e

    , MonadWriter [e] n 
    , MonadWriter [e] symm

    , MonadFix n 

    , Zoom symm n SymTab TypeCheckEnv
    , SymZooms m0 m1 symm
    ) =>
    MplProg MplRenamed -> n (MplProg MplTypeChecked)
runTypeCheck (MplProg stmts) = MplProg <$> traverse typeCheckStmt stmts

typeCheckStmt ::
    forall e m0 m1 symm n. 
    ( AsAllTypeCheckErrors e

    , MonadWriter [e] n 
    , MonadWriter [e] symm

    , MonadFix n 

    , Zoom symm n SymTab TypeCheckEnv
    , SymZooms m0 m1 symm
    ) =>
    MplStmt MplRenamed -> n (MplStmt MplTypeChecked)
typeCheckStmt (MplStmt defns wheres) = do
    wheres' <- traverse typeCheckStmt wheres

    envLcl % typeInfoSymTab .= mempty
    rec envLcl % typeInfoEnvMap .= tagmap
        ~((defns', eqns), errpkg) <- fmap (first (NE.unzip . NE.fromList)) 
            $ runWriterT 
            $ typeCheckDefns 
            $ NE.toList defns
        let terrs = collectPkgErrors errpkg
            erroccured = hasn't _Empty terrs

            foralls = foldMap (view _1) eqns
            exists = foldMap (view _2) eqns
            subs = foldMap (view _3) eqns
            eqns' = TypeEqnsForall foralls $ [TypeEqnsExist exists subs]
            -- pkgtest = runExcept (solveTypeEqns eqns') :: Either (TypeUnificationError MplTypeSub) (Package MplTypeSub)
            -- pkg = runExcept $ bool (return ()) (throwError []) erroccured 
                -- >> withExceptT pure (solveTypeEqns eqns') 
            pkg = runExcept $ bool (return ()) (throwError mempty) erroccured 
                    >> withExceptT pure (solveTypeEqns eqns') 

        ~tagmap <- packageToTypeTagMap (either mempty id (pkg :: Either [e] (Package MplTypeSub)))

    tell terrs
    traceM $ bool [] (show eqns') $ null terrs
    tell $ either id mempty pkg

    -- need to replace definitions in the symbol table here for
    -- functions. Moreover, illegally called functoins need listening..
    zoom envGbl $ 
        bool (traverse eliminateSymTabDefn defns') (traverse recollectSymTabDefn defns') (has _Right pkg)

    -- swap the buffers..
    symtab' <- guse envGbl
    envLcl % typeInfoSymTab .= symtab'

    return $ MplStmt defns' wheres'

typeCheckDefns ::
    TypeCheck
        [MplDefn MplRenamed] 
        [ ( MplDefn MplTypeChecked
          , ([TypeP MplTypeSub], [TypeP MplTypeSub], [TypeEqns MplTypeSub])) ]
-- same as the rename step.. need to do the magic recursive do in order
-- to get the recursive declarations together properly.
typeCheckDefns (defn : defns) = do
    rec ~(defn', eqns) <- envLcl % typeInfoSymTab .= symtab 
                    >> fmap snd (withFreshTypeTag (typeCheckDefn defn))
        ~nsyms <- zoom (envLcl % typeInfoSymTab) $ collectSymTabDefn defn'
        envGbl %= (nsyms<>)
        ~defns' <- typeCheckDefns defns
        ~symtab <- guse envGbl
    return $ (defn', eqns) : defns'
typeCheckDefns [] = return []


typeCheckDefn ::
    TypeCheck (MplDefn MplRenamed) 
            ( (MplDefn MplTypeChecked), ([TypeP MplTypeSub], [TypeP MplTypeSub], [TypeEqns MplTypeSub]))
typeCheckDefn (ObjectDefn obj) = (((,mempty) . ObjectDefn)) <$> case obj of
        SeqObjDefn obj -> SeqObjDefn <$> case obj of
            DataDefn n -> DataDefn <$> typeCheckTypeClauseSpine n
            CodataDefn n -> CodataDefn <$> typeCheckTypeClauseSpine n
        ConcObjDefn obj -> ConcObjDefn <$> case obj of
            ProtocolDefn n -> ProtocolDefn <$> typeCheckTypeClauseSpine n
            CoprotocolDefn n -> CoprotocolDefn <$> typeCheckTypeClauseSpine n
typeCheckDefn (FunctionDefn fun@(MplFunction name funtype defn)) = do
    ((foralls, funtype'), symtp) <- case funtype of    
        Just tp -> do
            tp <- kindCheckFunType tp
            arrenv <- freshInstantiateArrEnv
            let (inst, insttp) = runInstantiateArrType
                    (instantiateArrType (_Just % _TypeAnnFun # fun) tp)
                    arrenv
            -- return ((inst, insttp), SymFun tp)
            return ((inst, insttp), _SymExplicit # tp)
        Nothing -> do
            tag <- freshTypeTag
            let tp = typePtoTypeVar $ annotateTypeTag tag fun
            -- return $ (([], tp), SymSub tp)
            return $ (([], tp), _SymImplicit # tp)
    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypestable <- freshTypeTag
    ttypemap <- guse (envLcl % typeInfoEnvMap)
    
    rec let funsymentry = _SymEntry # (symtp, _SymSeqCall % _ExprCallFun # fun')
        envLcl % typeInfoSymTab % symTabExpr % at (name ^. uniqueTag) ?= funsymentry

        ~(ttypephrases, (defn', acceqns)) <- second NE.unzip . NE.unzip <$> 
            traverse (withFreshTypeTag . typeCheckFunBody ) defn

        let fun' = MplFunction name (fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq) defn'
            ttypep =  annotateTypeTag ttype fun
            ttypep' = typePtoTypeVar $ ttypep
            ttypephrases' = annotateTypeTags (NE.toList ttypephrases) $ NE.toList defn
            eqns = -- TypeEqnsExist ttypephrases' $
                [ TypeEqnsEq (ttypep', funtype') ]
                <> [ genStableEqn ttypestable ttypep ]
                <> map (TypeEqnsEq . (ttypep',) . typePtoTypeVar ) ttypephrases'
                <> (sconcat acceqns)

    return (FunctionDefn fun', (foralls, ttypep : ttypephrases', eqns))

-- some duplciated code...
typeCheckDefn (ProcessDefn procc@(MplProcess name proctype defn)) = do
    ((foralls, proctype'), symtp) <- case proctype of    
        Just tp -> do
            ~tp <- fmap fromJust $ kindCheckProcessType tp
            arrenv <- freshInstantiateArrEnv
            let (inst, insttp) = runInstantiateArrType     
                    (instantiateArrType (_Just % _TypeAnnProc # procc) tp)
                    arrenv 
            return ((inst, insttp), _SymExplicit # tp)
        Nothing -> do
            tag <- freshTypeTag
            let tp = typePtoTypeVar $ annotateTypeTag tag procc
            return $ (([], tp), _SymImplicit # tp)

    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypestable <- freshTypeTag
    ttypemap <- guse (envLcl % typeInfoEnvMap)

    envLcl % typeInfoSymTab % symTabCh .= mempty
    
    rec let procsymentry = _SymEntry # (symtp, _SymRunInfo # procc')
        envLcl % typeInfoSymTab % symTabConc % at (name ^. uniqueTag) ?= procsymentry

        (ttypephrases, (defn', acceqns)) <- second NE.unzip . NE.unzip <$> 
            traverse (withFreshTypeTag . typeCheckProcessBody) defn

        let procc' = MplProcess name (fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeConc) defn'
            ttypep = annotateTypeTag ttype procc
            ttypephrases' = annotateTypeTags (NE.toList ttypephrases) $ NE.toList defn
            eqns = -- TypeEqnsExist ttypephrases' $
                [ TypeEqnsEq (typePtoTypeVar ttypep, proctype') ]
                -- <> map (TypeEqnsEq . (ttype',)) (annotateTypeTags (NE.toList ttypephrases) $ ttypephrases' )
                <> map (TypeEqnsEq . (typePtoTypeVar ttypep,) . typePtoTypeVar) ttypephrases'
                <> [ genStableEqn ttypestable ttypep ]
                <> (sconcat acceqns)

    return $ (ProcessDefn procc', (foralls, ttypep : ttypephrases', eqns)) 

-------------------------
-- Type checking expressions
-------------------------
typeCheckFunBody ::
    TypeCheck
        ([MplPattern MplRenamed], MplExpr MplRenamed)
        (([MplPattern MplTypeChecked], MplExpr MplTypeChecked), 
            [TypeEqns MplTypeSub] )
typeCheckFunBody bdy@(patts, expr) = do
    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypestable <- freshTypeTag
    ttypemap <- guse (envLcl % typeInfoEnvMap)

    ~(ttypepatts, (patts', pattacceqns)) <- second NE.unzip . NE.unzip <$> 
        traverse (withFreshTypeTag . typeCheckPattern) patts 

    ~(ttypeexpr, (expr', expracceqn)) <- withFreshTypeTag . typeCheckExpr $ expr

    let ttypep = annotateTypeTag ttype bdy
        ttypepexpr = annotateTypeTag ttypeexpr expr
        ttypeppatts = annotateTypeTags ttypepatts patts

        eqn = TypeEqnsExist (ttypepexpr:ttypeppatts) $
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , mkTypeSubSeqArr 
                        ( map typePtoTypeVar ttypeppatts
                        , typePtoTypeVar ttypepexpr )
                    )
                ]
                <> concat pattacceqns
                <> expracceqn

    return ((patts', expr'), [eqn])


typeCheckExpr ::
    TypeCheck
        (MplExpr MplRenamed)
        (MplExpr MplTypeChecked, [TypeEqns MplTypeSub])
typeCheckExpr = para f
  where
    f :: Base (MplExpr MplRenamed) (MplExpr MplRenamed, _ (MplExpr MplTypeChecked, [TypeEqns MplTypeSub])) ->
        _ (MplExpr MplTypeChecked, [TypeEqns MplTypeSub])
    f (EVarF cxt n) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry lkuptp (SymSeqCall lkupdef)) <- fmap fromJust $ zoom (envLcl % typeInfoSymTab ) $ lookupSymExpr n

        arrenv <- freshInstantiateArrEnv
        let ann = _EVar # (cxt, n) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ann
            
            -- lkuptp' = fromJust $ lkuptp ^? _SymSeqCallType % _SymImplicit
            {- Note: This is a little peculiar! we can have a program like:
             - fun undefined :: -> A =
             -      -> undefined
             - So, this is a valid call because it takes no arguments ( undefined is the same as undefined() )
             - But this indeed has an explicit type... indeed, this is identical to the ECallF case..
            -}
            (ttypeargs, lkuptp') = (`runInstantiateArrType`arrenv)
                $ fromJust 
                $ instantiateArrType (_Just % _TypeAnnExpr # ann) <$> lkuptp ^? _SymSeqCallType

            eqn = TypeEqnsExist ttypeargs $ 
                [ TypeEqnsEq (typePtoTypeVar ttypep , lkuptp') ] 
                <> [ genStableEqn ttypestable ttypep ]

        return (EVar (lkupdef, fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq ) n, [eqn])

    f (EPOpsF _ _ _ _ ) = panicNotImplemented
    f (EBuiltInOpF _ _ _ _ ) = panicNotImplemented
    f (EIntF _ _ ) = panicNotImplemented
    f (ECharF _ _ ) = panicNotImplemented
    f (EDoubleF _ _ ) = panicNotImplemented
    f (EListF _ _ ) = panicNotImplemented
    f (EStringF _ _ ) = panicNotImplemented
    f (EUnitF _ ) = panicNotImplemented
    f (ETupleF _ _ ) = panicNotImplemented
    f (EIfF _ _ _ _) = panicNotImplemented
    f (ELetF cxt lets (_, mexpr)) = do
        st <- guse equality
        sup <- freshUniqueSupply

        let ~(MplProg lets', errs) = runWriter 
                $ flip evalStateT 
                    -- some awkwardness here that we need to update the 
                    -- global symbol table...
                    ( st & uniqueSupply .~ sup 
                         & envGbl .~ st ^. envLcl % typeInfoSymTab ) 
                $ runTypeCheck
                $ MplProg (NE.toList lets)

        tell $ review _ExternalError $ errs

        zoom (envLcl % typeInfoSymTab) $ do
            nsyms <- traverse (traverse collectSymTabDefn . view stmtDefns) lets'
            equality %= (foldOf (folded % folded) nsyms<>)
            traverse_ (traverse_ recollectSymTabDefn . view stmtDefns) lets'

        (expr', expreqns) <- mexpr

        return ( _ELet # (cxt, NE.fromList lets', expr') , expreqns) 

    f (EFoldF cxt (foldonexpr, foldon) (phrase :| phrases)) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let foldexpr = EFold cxt foldonexpr (over (mapped % _4) fst $ phrase :| phrases) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype foldexpr

        (ttypefoldon, (foldon', foldoneqns)) <- withFreshTypeTag foldon
        let ttypepfoldon = annotateTypeTag ttypefoldon foldonexpr 
        {- Remarks..
         - Given a fold expression:
         - fold a of
         -  Constructor0 : a b c -> expr0
         -  Constructor1 : a b c -> expr1
         -  Constructor2 : a b c -> expr2
         -
         - The type of the overall expression is the type of the state 
         - variable for THE FIRST CONSTRUCTOR even given a mutually 
         - recursive case....
         -}

        arrenv <- freshInstantiateArrEnv
        ((phrases', phraseeqns), instt) <- fmap 
            (second (toListOf (instantiateArrEnvInstantiated % folded)))
            $ flip runStateT arrenv $ do
                -- the first phrase is the type of the overall expresion... 
                let (cxt, ident, patts, (expr, mexpr)) = phrase

                ~(SymEntry lkuptp seqdef) <- lift $ fmap fromJust 
                    $ zoom (envLcl % typeInfoSymTab) 
                    $ lookupSymExprDataPhrase (ident, expr)

                (ttypepfroms, ttypestvar, ttypeclause) <- state $ runState $ do
                    let (freevars, froms, to) = fromJust $ lkuptp ^? _SymDataPhrase % originalType 
                    subs <- updateInstantiatedAndGetSubs freevars
                       
                    return ( fromJust $ traverse (instantiateTypeWithSubs subs) froms
                           , fromJust $ instantiateTypeWithSubs subs to
                           , seqdef ^. typePhraseExt % Optics.to 
                                ( fromJust 
                                . instantiateTypeWithSubs subs
                                . typeClauseToMplType 
                                )
                           )

                (ttypepatts, (patts', pattseqns)) <- lift 
                    $ fmap (second unzip <<< unzip) 
                    $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                (ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpr

                let reachablephrases = foldMapOf 
                        ( typePhraseExt 
                        % typeClauseExt 
                        % typeClauseSpineClauses 
                        % folded 
                        % typeClausePhrases 
                        % folded 
                        % typePhraseName ) (:[]) seqdef
                    phrase' = (seqdef, ident, patts', expr')
                    ttypeppatts = annotateTypeTags ttypepatts patts
                    ttypepexpr = annotateTypeTag ttypeexpr expr
                    phraseeqn = TypeEqnsExist (ttypepexpr : ttypeppatts) $ 
                        -- the type of the overall expression is the type of 
                        -- the first state variable that is being folded to
                        [ TypeEqnsEq (typePtoTypeVar ttypep, ttypestvar) 
                        -- moreover, the fold on should be the same type as this
                        -- as the clause of the first fold phrase....
                        , TypeEqnsEq (typePtoTypeVar ttypepfoldon,  ttypeclause) 
                        , TypeEqnsEq 
                            ( mkTypeSubSeqArr (map typePtoTypeVar ttypeppatts, typePtoTypeVar ttypepexpr)  
                            , mkTypeSubSeqArr (ttypepfroms, ttypestvar)  
                            )
                        ] 
                        <> expreqns
                        <> fold pattseqns

                (phrases', phraseseqns) <- fmap unzip 
                    $ for phrases 
                    -- duplciated code, but we remove the requirement of the 
                    -- state variable being the type of the overall expression
                    -- REMARK: We should really test if each of the phrases 
                    -- are indeed from the same data graph here!! TODO in the
                    -- future (THIS REALLY SHOULD BE DONE)!
                    $ \(cxt, ident, patts, (expr, mexpr)) -> do
                        ~(SymEntry lkuptp seqdef) <- lift $ fmap fromJust 
                            $ zoom (envLcl % typeInfoSymTab) 
                            $ lookupSymExprDataPhrase (ident, expr)

                        tell $ review _ExternalError $ bool
                            []
                            [_ExpectedFoldPhraseToBeEitherButGot # (reachablephrases, seqdef ^. typePhraseName)]
                            $ seqdef ^. typePhraseName `notElem` reachablephrases

                        (ttypepfroms, ttypestvar) <- state $ runState $ do
                            let (freevars, froms, to) = fromJust $ lkuptp ^? _SymDataPhrase % originalType 
                            subs <- updateInstantiatedAndGetSubs freevars
                               
                            return ( fromJust $ traverse (instantiateTypeWithSubs subs) froms
                                   , fromJust $ instantiateTypeWithSubs subs to)

                        (ttypepatts, (patts', pattseqns)) <- lift 
                            $ fmap (second unzip <<< unzip) 
                            $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                        (ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpr

                        let phrase' = (seqdef, ident, patts', expr')
                            ttypeppatts = annotateTypeTags ttypepatts patts
                            ttypepexpr = annotateTypeTag ttypeexpr expr
                            phraseeqn = TypeEqnsExist (ttypepexpr : ttypeppatts) $ 
                                -- patterns should match the type of constructor without 
                                [ TypeEqnsEq 
                                    ( mkTypeSubSeqArr (map typePtoTypeVar ttypeppatts, typePtoTypeVar ttypepexpr)  
                                    , mkTypeSubSeqArr (ttypepfroms, ttypestvar)  
                                    )
                                ] 
                                <> expreqns
                                <> fold pattseqns
                        return (phrase', phraseeqn)

                return (phrase' :| phrases', phraseeqn : phraseseqns)

        let eqns = TypeEqnsExist (ttypepfoldon : instt) $
                [ genStableEqn ttypestable ttypep ]
                <> phraseeqns 
                <> foldoneqns
        return 
            ( _EFold # 
                ( fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq
                , foldon'
                , phrases'
                )
            , [eqns]
            )

    f (EUnfoldF cxt (unfoldorigexpr, unfoldonexpr) (phrase :| phrases)) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let unfoldexpr = EUnfold cxt unfoldorigexpr 
                (over (mapped % _3 % mapped % _4) fst $ phrase :| phrases) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype unfoldexpr

        (ttypeunfoldonexpr, (unfoldonexpr', unfoldonexpreqns)) <- withFreshTypeTag unfoldonexpr
        let ttypepunfoldonexpr = annotateTypeTag ttypeunfoldonexpr unfoldorigexpr 
        {- Remarks..
         - Given an unfold expression:
         - unfold a of
         -      pattern0 of
         -          Destructor0A : a b c -> expr0A
         -          Destructor0B : a b c -> expr0B
         -      pattern1 of
         -          Destructor1A : a b c -> expr1A
         -          Destructor1B : a b c -> expr1B
         -
         - The type of the overall expression is the type of the state 
         - variable for THE FIRST unfold phrase, and the type of the type
         - of the whole expression is the type of the clause in the FIRST
         - unfold phrase
         -
         - TODO -- we need to check for unreachable unfold phrases! 
         -}

        arrenv <- freshInstantiateArrEnv
        ((phrases', phraseeqns), instt) <- fmap 
            (second (toListOf (instantiateArrEnvInstantiated % folded)))
            $ flip runStateT arrenv $ do
                -- the first phrase is the type of the overall expresion... 
                let (cxt, unfoldonpatt, phrasesubphrases@((_, ident, _ ,_):|_)) = phrase

                -- get rid of this at the end
                (ttypeunfoldonpatt, (unfoldonpatt', unfoldonpatteqns)) <- lift $ withFreshTypeTag $ typeCheckPattern unfoldonpatt
                let ttypepunfoldonpatt = annotateTypeTag ttypeunfoldonpatt unfoldonpatt

                ttypesubphrase <- freshTypeTag
                let ttypepsubphrase = annotateTypeTag ttypesubphrase ()

                -- need to compute the reachable phrases..
                ~seqdef <- lift 
                    $ fmap (fromJust . fromJust)
                    $ zoom (envLcl % typeInfoSymTab) 
                    $ runZoomedLookup
                    $ zoomSymExpr ident 
                    $ zoomSymExprSeqPhrase 
                    $ guses equality (preview _CodataDefn)

                let reachablephrases = foldMapOf 
                        ( typePhraseExt 
                        % typeClauseExt 
                        % typeClauseSpineClauses 
                        % folded 
                        % typeClausePhrases 
                        % folded 
                        % typePhraseName ) (:[]) seqdef
                
                (phrasesubphrases', phrasesubphraseseqns) <- fmap NE.unzip $ for phrasesubphrases $ \(cxt, ident, patts, (expr, mexpr)) -> do
                    ~(SymEntry lkuptp seqdef) <- lift $ fmap fromJust 
                        $ zoom (envLcl % typeInfoSymTab) 
                        $ lookupSymExprCodataPhrase (ident, expr)

                    tell $ review _ExternalError $ bool
                        []
                        [ _ExpectedUnfoldPhraseToBeEitherButGot # 
                            ( reachablephrases
                            , seqdef ^. typePhraseName)
                            ]
                        $ seqdef ^. typePhraseName `notElem` reachablephrases

                    (ttypepfroms, ttypestvar, ttypeto, ttypeclause) <- state $ runState $ do
                        let (freevars, (froms, st), to) = fromJust $ lkuptp ^? _SymCodataPhrase % originalType 
                        subs <- updateInstantiatedAndGetSubs freevars
                           
                        return ( fromJust $ traverse (instantiateTypeWithSubs subs) froms
                               , fromJust $ instantiateTypeWithSubs subs st
                               , fromJust $ instantiateTypeWithSubs subs to
                               , seqdef ^. typePhraseExt % Optics.to 
                                    ( fromJust 
                                    . instantiateTypeWithSubs subs
                                    . typeClauseToMplType 
                                    )
                               )

                    (ttypepatts, (patts', pattseqns)) <- lift 
                        $ fmap (second unzip <<< unzip) 
                        $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                    (ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpr

                    let subphrase' = (seqdef, ident, patts', expr')
                        ttypeppatts = annotateTypeTags ttypepatts patts
                        ttypepexpr = annotateTypeTag ttypeexpr expr
                        subphraseeqn = TypeEqnsExist (ttypepexpr : ttypepunfoldonpatt : ttypeppatts) $ 
                            -- the type of the overall expression is the type of 
                            -- the clause of the first unfold subphrase
                            [ TypeEqnsEq (typePtoTypeVar ttypep, ttypeclause) 
                            -- The type of the state var is the same
                            -- as the unfoldon pattern 
                            , TypeEqnsEq (typePtoTypeVar ttypepunfoldonpatt, ttypestvar) 
                            -- The type of the type of the unfoldonpatt is the same as
                            -- the as the unfoldonexpr
                            , TypeEqnsEq (typePtoTypeVar ttypepunfoldonpatt, typePtoTypeVar ttypepunfoldonexpr) 
                            -- of course, we need the patternsand the expression sto match
                            , TypeEqnsEq 
                                ( mkTypeSubSeqArr (map typePtoTypeVar ttypeppatts, typePtoTypeVar ttypepexpr)  
                                , mkTypeSubSeqArr (ttypepfroms, ttypeto)  
                                )
                            -- Think of this subphrase like case.. Each of these 
                            -- expressions must be the same..
                            , TypeEqnsEq
                                ( typePtoTypeVar ttypepsubphrase
                                , ttypeclause
                                )
                            ] 
                            <> unfoldonpatteqns
                            <> expreqns
                            <> fold pattseqns
                    return (subphrase', subphraseeqn)

                let phraseeqn = unfoldonexpreqns <> NE.toList phrasesubphraseseqns
                    phrase' = (cxt, unfoldonpatt', phrasesubphrases')

                (phrases', (phraseseqns, ttypepsubphrases)) <- 
                    fmap (second unzip . unzip) 
                    $ for phrases $ \(cxt, unfoldonpatt, phrasesubphrases) -> do
                        (ttypeunfoldonpatt, (unfoldonpatt', unfoldonpatteqns)) <- lift $ withFreshTypeTag $ typeCheckPattern unfoldonpatt
                        -- duplciated code..
                        let ttypepunfoldonpatt = annotateTypeTag ttypeunfoldonpatt unfoldonpatt

                        ttypesubphrase <- freshTypeTag
                        let ttypepsubphrase = annotateTypeTag ttypesubphrase ()
                        
                        (phrasesubphrases', phrasesubphraseseqns) <- fmap NE.unzip $ for phrasesubphrases $ \(cxt, ident, patts, (expr, mexpr)) -> do
                            ~(SymEntry lkuptp seqdef) <- lift $ fmap fromJust 
                                $ zoom (envLcl % typeInfoSymTab) 
                                $ lookupSymExprCodataPhrase (ident, expr)

                            tell $ review _ExternalError $ bool
                                []
                                [ _ExpectedUnfoldPhraseToBeEitherButGot # 
                                    ( reachablephrases
                                    , seqdef ^. typePhraseName)
                                    ]
                                $ seqdef ^. typePhraseName `notElem` reachablephrases

                            (ttypepfroms, ttypestvar, ttypeto, ttypeclause) <- state $ runState $ do
                                let (freevars, (froms, st), to) = fromJust $ lkuptp ^? _SymCodataPhrase % originalType 
                                subs <- updateInstantiatedAndGetSubs freevars
                                   
                                return ( fromJust $ traverse (instantiateTypeWithSubs subs) froms
                                       , fromJust $ instantiateTypeWithSubs subs st
                                       , fromJust $ instantiateTypeWithSubs subs to
                                       , seqdef ^. typePhraseExt % Optics.to 
                                            ( fromJust 
                                            . instantiateTypeWithSubs subs
                                            . typeClauseToMplType 
                                            )
                                       )

                            (ttypepatts, (patts', pattseqns)) <- lift 
                                $ fmap (second unzip <<< unzip) 
                                $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                            (ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpr

                            let subphrase' = (seqdef, ident, patts', expr')
                                ttypeppatts = annotateTypeTags ttypepatts patts
                                ttypepexpr = annotateTypeTag ttypeexpr expr
                                phraseeqn = TypeEqnsExist (ttypepunfoldonpatt : ttypepexpr : ttypeppatts) $ 
                                    -- The type of the state var is the same
                                    -- as the unfoldon pattern 
                                    [ TypeEqnsEq (typePtoTypeVar ttypepunfoldonpatt, ttypestvar) 
                                    -- of course, we need the patternsand the expression sto match
                                    , TypeEqnsEq 
                                        ( mkTypeSubSeqArr (map typePtoTypeVar ttypeppatts, typePtoTypeVar ttypepexpr)  
                                        , mkTypeSubSeqArr (ttypepfroms, ttypeto)  
                                        )
                                    -- Think of this subphrase like case.. Each of these 
                                    -- expressions must be the same..
                                    , TypeEqnsEq
                                        ( typePtoTypeVar ttypepsubphrase
                                        , ttypeclause
                                        )
                                    ] 
                                    <> expreqns
                                    <> fold pattseqns
                                    <> unfoldonpatteqns

                            return (subphrase', phraseeqn)

                        let phrase' = (cxt, unfoldonpatt', phrasesubphrases')
                            eqns = TypeEqnsExist [ttypepunfoldonpatt, ttypepsubphrase] $ unfoldonpatteqns <> NE.toList phrasesubphraseseqns
                        return (phrase', ([eqns], ttypepsubphrase))

                let nunfoldphrases' = phrase' :| phrases'
                    eqns = TypeEqnsExist (ttypepsubphrase : ttypepsubphrases) $
                            phraseeqn <> fold phraseseqns

                return (nunfoldphrases', [eqns] )

        let eqns = TypeEqnsExist (ttypepunfoldonexpr:instt) $
                [ genStableEqn ttypestable ttypep ]
                <> unfoldonexpreqns
                <> phraseeqns

        return 
            ( _EUnfold # 
                ( fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq
                , unfoldonexpr'
                , phrases' 
                )
            , [eqns]
            )

    f (ESwitchF _ _) = panicNotImplemented

    f (ECaseF cxt (caseon, mcaseon) cases) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        (ttypecaseon, (caseon', caseoneqn)) <- withFreshTypeTag mcaseon

        ((ttypepatts, ttypeexprs), (pattsexprs', acceqns)) <- 
            fmap ((NE.unzip *** NE.unzip) <<< NE.unzip)
            $ for cases $ \(patt, (_, mexpreqn)) -> do
                (ttypepatt, (patt', patteqn)) <- withFreshTypeTag $ typeCheckPattern patt
                (ttypeexpr, (expr', expreqn)) <- withFreshTypeTag mexpreqn
                return ((ttypepatt, ttypeexpr), ((patt', expr'), patteqn <> expreqn))

        let ttypep = annotateTypeTag ttype (_ECase # (cxt, caseon, fmap (second fst) cases) :: MplExpr MplRenamed)
            ttypepcaseon = annotateTypeTag ttypecaseon caseon

            ttypeppatts = annotateTypeTags (NE.toList ttypepatts) (NE.toList $ fmap fst cases)
            ttypepexprs = annotateTypeTags (NE.toList ttypeexprs) (NE.toList $ fmap (fst . snd) cases)

            eqns = TypeEqnsExist (ttypepcaseon : ttypeppatts ++ ttypepexprs) $
                [ genStableEqn ttypestable ttypep ]
                -- the case on should be the same as all the patterns
                <> map (TypeEqnsEq . (typePtoTypeVar ttypepcaseon,) . typePtoTypeVar) ttypeppatts
                -- the resulting type should be the same as all expressions
                -- on the other side of the case
                <> map (TypeEqnsEq . (typePtoTypeVar ttypep,) . typePtoTypeVar) ttypepexprs
                -- accumulate the old equations of course.
                <> caseoneqn
                <> concat acceqns

        return 
            ( _ECase # 
             ( fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq
             , caseon'
             , pattsexprs' )
            , [eqns])
        
    f (EObjCallF cxt ident args) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry lkuptp (SymSeqPhraseCall seqdef)) <- fmap fromJust 
            $ zoom (envLcl % typeInfoSymTab) 
            $ lookupSymExpr ident

        (ttypeargs, (args', argseqns)) <- fmap (second unzip <<< unzip) $
            for args $ \(_, mexpreqns) -> withFreshTypeTag mexpreqns

        let expr = _EObjCall # (cxt, ident, map fst args) :: MplExpr MplRenamed

        -- Actually, we can type a destructor and just call it...
        -- tell $ review _ExternalError $ flip (maybe mempty) (seqdef ^? _CodataDefn) $ \defn ->
            -- [_IllegalExprDataCallGotCodataInstead # (expr, defn) ]

        arrenv <- freshInstantiateArrEnv
        let ttypep = annotateTypeTag ttype expr
            ttypepargs = annotateTypeTags ttypeargs $ map fst args

            ann = _Just % _TypeAnnExpr # expr
            ~(ttypesphrase, lkuptp') = (`runInstantiateArrType`arrenv)
                $ fromJust 
                $ instantiateArrType ann
                    <$> lkuptp ^? _SymDataPhrase % noStateVarsType
                <|> instantiateArrType ann
                    <$> lkuptp ^? _SymCodataPhrase % noStateVarsType

            eqns = TypeEqnsExist (ttypesphrase ++ ttypepargs) $
                [ TypeEqnsEq 
                    ( mkTypeSubSeqArr (map typePtoTypeVar ttypepargs, typePtoTypeVar ttypep)  
                    , lkuptp') 
                ] 
                <> [genStableEqn ttypestable ttypep]
                <> concat argseqns

        return 
            ( _EObjCall # 
              ( fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq
              , ident
              , args' ) 
            , [eqns] )

    -- lots of duplicated code..
    f (ECallF cxt ident args ) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry lkuptp (SymSeqCall seqdef)) <- fmap fromJust $ zoom (envLcl % typeInfoSymTab) $ lookupSymExpr ident

        (ttypeargs, (args', argseqns)) <- fmap (second unzip <<< unzip) $
            for args $ \(_, mexpreqns) -> withFreshTypeTag mexpreqns

        arrenv <- freshInstantiateArrEnv
        let expr = _ECall # (cxt, ident, map fst args) :: MplExpr MplRenamed

            ttypep = annotateTypeTag ttype expr
            ttypepargs = annotateTypeTags ttypeargs $ map fst args

            ann = _Just % _TypeAnnExpr # expr
            ~(ttypesphrase, lkuptp') = (`runInstantiateArrType`arrenv)
                $ fromJust 
                $ instantiateArrType ann <$> lkuptp ^? _SymSeqCallType

            eqns = TypeEqnsExist (ttypesphrase ++ ttypepargs) $
                [ TypeEqnsEq 
                    ( mkTypeSubSeqArr (map typePtoTypeVar ttypepargs, typePtoTypeVar ttypep)  
                    , lkuptp') 
                ] 
                <> [genStableEqn ttypestable ttypep]
                <> concat argseqns

        return 
            ( _ECall # 
              ( fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq
              , ident
              , args' ) 
            , [eqns] )


    f (ERecordF cxt phrases) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        
        let expr = _ERecord # (cxt, phrases & mapped % _3 % _2 %~ fst) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype expr

        {-
         -- STILL NEED TO CHECK IF THE CLAUSE IS EXHAUSTIVE
         -- Not really a type checking thing to do? Think of this as
         -- a non exhaustive patterns error which happens in compilation
         -- of pattern matching.....

        tell $ review _ExternalError $ bool 
            [] [_RecordConstructionErrorGotPhrasesButExpected # 
                ( recordphraseidents
                , clausephrases ) ]
            $ length recordphraseidents /= length clausephrases || 
                any id (zipWith (\a b -> a ^. uniqueTag /= b ^. typePhraseName % uniqueTag ) 
                    (NE.toList recordphraseidents) clausephrases)
        -}

        arrenv <- freshInstantiateArrEnv
        ~(((ttypeppatts, ttypepexpr), (phrases', phraseseqns)), ttypepinst) <- fmap 
            (( unzip *** unzip <<< unzip <<< NE.toList) 
                *** (toListOf (instantiateArrEnvInstantiated % folded)))
            $ flip runStateT arrenv
            $ for phrases $ \(_, ident, (patts, (expr, mexpreqn))) -> do
                ~(SymEntry lkuptp seqdef) <- fmap fromJust $ lift $ zoom (envLcl % typeInfoSymTab) $
                        lookupSymExprCodataPhrase (ident,expr)

                ~(ttypepatts, (patts', pattseqns)) <- lift 
                    $ fmap (second unzip <<< unzip) 
                    $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                ~(ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpreqn 

                -- Note: we need the strange ``state $ runState" call here to get the correct laziness
                (ttypepphrase, ttypeclause) <- state $ runState $ do
                    ttypepphrase <- instantiateArrType 
                        {- TODO, probably should include some sort of annotation
                        - information here... e.g. (_Just % TypeAnnPatt seqdef) -}
                        Nothing 
                        $ fromJust $ lkuptp ^? _SymCodataPhrase 
                                % noStateVarsType 
                                % to (over _2 fst)
                    subs <- getInstantiatedSubs
                    return 
                        ( ttypepphrase
                        , seqdef ^. typePhraseExt % to 
                            ( fromJust 
                            . instantiateTypeWithSubs subs
                            . typeClauseToMplType 
                            )
                        )

                let ttypeppatts = annotateTypeTags ttypepatts patts 
                    ttypepexpr = annotateTypeTag ttypeexpr expr
                    phraseeqns = 
                        -- the type of the patts and expression
                        -- must match the type phrase given in 
                        -- the clause
                        [ TypeEqnsEq 
                            ( mkTypeSubSeqArr (ttypeppatts, ttypepexpr) 
                            , ttypepphrase)
                        -- the type of this whole expression is the
                        -- same as the end result type
                        , TypeEqnsEq
                            ( ttypeclause
                            , typePtoTypeVar ttypep )
                        ]

                return 
                    ( (ttypeppatts, ttypepexpr)
                    , ((seqdef, ident, (patts', expr')), phraseeqns ++ concat pattseqns ++ expreqns)
                    )

        let eqns = TypeEqnsExist (concat ttypeppatts <> ttypepexpr <> ttypepinst ) $
                [ genStableEqn ttypestable ttypep ]
                <> concat phraseseqns

        return $ 
            ( _ERecord # 
              ( (cxt, fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq)
              , NE.fromList phrases') 
            , [eqns] )



-------------------------
-- Type checking process...
-------------------------
typeCheckProcessBody ::
    TypeCheck
    ( ([MplPattern MplRenamed], [ChIdentR], [ChIdentR])
        , NonEmpty (MplCmd MplRenamed) )
    ( (([MplPattern MplTypeChecked], [ChIdentT], [ChIdentT])
        , NonEmpty (MplCmd MplTypeChecked))
    , [TypeEqns MplTypeSub])
typeCheckProcessBody procbdy@((patts, ins, outs), cmds) = do
    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypestable <- freshTypeTag
    ttypemap <- guse (envLcl % typeInfoEnvMap)

    (ttypepatts, (patts', pattacceqns)) <- second NE.unzip . NE.unzip <$> 
        traverse (withFreshTypeTag . typeCheckPattern) patts 

    ttypeins <- traverse freshChTypeTag ins
    ttypeouts <- traverse freshChTypeTag outs

    -- ttypepattsstable <- traverse (const freshTypeTag) ttypepatts
    ttypeinsstable <- traverse (const freshTypeTag) ttypeins
    ttypeoutsstable <- traverse (const freshTypeTag) ttypeouts

    (cmds', acccmds) <- typeCheckCmds cmds

    let ttypep = annotateTypeTag ttype procbdy
        ttypeppatts = annotateTypeTags ttypepatts patts
        ttypepins = annotateTypeTags ttypeins ins
        ttypepouts = annotateTypeTags ttypeouts outs

        eqn = TypeEqnsExist (ttypeppatts ++ ttypepins ++ ttypepouts) $
                -- phrase equation
                [ TypeEqnsEq    
                    ( typePtoTypeVar ttypep
                    , _TypeConcArrF # 
                        ( _Just % _TypeAnnProcPhrase # procbdy
                        , map typePtoTypeVar ttypeppatts
                        , map typePtoTypeVar ttypepins
                        , map typePtoTypeVar ttypepouts ) 
                    ) 
                ]

                -- stable equations
                -- <> zipWith genStableEqn ttypepattsstable ttypeppatts
                <> zipWith genStableEqn ttypeinsstable ttypepins
                <> zipWith genStableEqn ttypeoutsstable ttypepouts

                -- accumulate the equations
                <> acccmds
                <> concat pattacceqns
        ins' = zipWith 
                (\stref -> review _ChIdentT . (,fromJust $ ttypemap ^? at stref % _Just % _SymTypeCh) )
                ttypeinsstable ins 
        outs' = zipWith 
                (\stref -> review _ChIdentT . (,fromJust $ ttypemap ^? at stref % _Just % _SymTypeCh) )
                ttypeoutsstable outs 

    return (((patts', ins', outs'), cmds'), [eqn])

typeCheckCmds ::
    TypeCheck
        (NonEmpty (MplCmd MplRenamed)) 
        (NonEmpty (MplCmd MplTypeChecked), [TypeEqns MplTypeSub])
typeCheckCmds (cmd :| []) = do
    tell $ review _ExternalError $ case cmd of
        (CClose cxt _) -> [_IllegalLastCommand # cxt]
        (CGet cxt _ _) -> [_IllegalLastCommand # cxt]
        (CPut cxt _ _) -> [_IllegalLastCommand # cxt]
        (CHPut cxt _ _) -> [_IllegalLastCommand # cxt]
        (CSplit cxt _ _) -> [_IllegalLastCommand # cxt]
        _ -> mempty

    (cmd', eqns) <- typeCheckCmd cmd

    openchs <- guses (envLcl % typeInfoSymTab % symTabCh) (toListOf (folded % symEntryInfo))
    tell $ review _ExternalError 
        $ bool [_AtLastCmdThereAreUnclosedChannels # (cmd, openchs)] [] 
        $ null openchs

    return (cmd' :| [], eqns)
typeCheckCmds (cmd :| rst) = do
    tell $ review _ExternalError $ case cmd of
        CFork cxt _ _ -> [_IllegalNonLastCommand # cxt]
        CId cxt _ -> [_IllegalNonLastCommand # cxt]
        CIdNeg cxt _ -> [_IllegalNonLastCommand # cxt]
        -- run commands are not techincally a keyword..
        CRun _ cxt _ _ _ -> [review _IllegalNonLastCommand $ cxt ^. identPNameOcc % to KeyWordNameOcc]
        CHCase cxt _ _ -> [_IllegalNonLastCommand # cxt]
        CHalt cxt _ -> [_IllegalNonLastCommand # cxt]
        CRace cxt _ -> [_IllegalNonLastCommand # cxt]
        _ -> []
    (cmd', eqn) <- typeCheckCmd cmd
    (rst', eqns) <- first NE.toList <$> typeCheckCmds (NE.fromList rst)
    return (cmd' :| rst', eqn <> eqns)

typeCheckCmd ::
    TypeCheck
        (MplCmd MplRenamed)
        (MplCmd MplTypeChecked, [TypeEqns MplTypeSub])
typeCheckCmd cmd = case cmd of 
    CRun cxt ident seqs ins outs -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry tp (SymRunInfo procc)) <- zoom (envLcl % typeInfoSymTab) $ lookupSymConc ident

        arrenv <- freshInstantiateArrEnv
        let (ttypepargs, ttypeproc) = (`runInstantiateArrType` arrenv) 
                $ fromJust 
                $ tp ^? _SymConcCallType % to (instantiateArrType (_Just % _TypeAnnProcCall # procc))

        ttypesins <- zoom (envLcl % typeInfoSymTab) $ traverse lookupSymCh ins 
        ttypesinsstables <- traverse (const freshTypeTag) ins

        ttypesouts <- zoom (envLcl % typeInfoSymTab) $ for outs $ lookupSymCh 
        ttypesoutsstables <- traverse (const freshTypeTag) outs

        tell $ review _ExternalError $ 
            foldMapOf (folded % symEntryInfo) expectedInputPolarity ttypesins
            <> foldMapOf (folded % symEntryInfo) expectedOutputPolarity ttypesouts


        (ttypeseqs, (seqs', seqseqns)) <- fmap (second unzip . unzip) 
            $ traverse (withFreshTypeTag . typeCheckExpr) seqs

        -- remove the entries from the symbol table
        zoom (envLcl % typeInfoSymTab) $ do
            for_ ins $ \ch -> symTabCh % at (ch ^. uniqueTag) .= Nothing
            for_ outs $ \ch -> symTabCh % at (ch ^. uniqueTag) .= Nothing

        let ttypespins = annotateTypeTags (map (view symEntryType) ttypesins) ins
            ttypespouts = annotateTypeTags (map (view symEntryType) ttypesouts) outs
            ttypespseqs = annotateTypeTags ttypeseqs seqs

            eqns = TypeEqnsExist (ttypepargs <> ttypespseqs) $
                    -- match the given types with the actual type of the process
                    [ TypeEqnsEq 
                        ( ttypeproc
                        , _TypeConcArrF # 
                            ( _Just % _TypeAnnCmd # cmd
                            , map typePtoTypeVar ttypespseqs
                            , map typePtoTypeVar ttypespins
                            , map typePtoTypeVar ttypespouts
                            ) 
                        ) 
                    ] 
                    -- match the new channel types with the old channel types
                    {-
                    <> genTypeEqEqns 
                        (map typePtoTypeVar (annotateTypeTags ttypesinsstables ins))
                        (map typePtoTypeVar ttypespins) 
                    <> genTypeEqEqns 
                        (map typePtoTypeVar (annotateTypeTags ttypesinsstables outs))
                        (map typePtoTypeVar ttypespouts) 
                    -}
                    -- stable equations
                    <> zipWith genStableEqn ttypesinsstables ttypespins
                    <> zipWith genStableEqn ttypesoutsstables ttypespouts
                    -- accumlate old equations
                    <> concat seqseqns

            ins' = zipWith (\chr tag -> _ChIdentT # (chr, fromJust $ ttypemap ^? at tag % _Just % _SymTypeCh)) 
                    ins ttypesinsstables
            outs' = zipWith (\chr tag -> _ChIdentT # (chr, fromJust $ ttypemap ^? at tag % _Just % _SymTypeCh))
                    outs ttypesoutsstables

        return (_CRun # (procc, ident, seqs', ins', outs'), [eqns] )
    
    CClose cxt ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag

        envLcl % typeInfoSymTab % symTabCh % at (ch ^. uniqueTag) .= Nothing

        let ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh )

            ttypepch = annotateTypeTag ttypech ch
            eqns = 
                [ genStableEqn ttypechstable ttypepch
                -- the type is top bot..
                , TypeEqnsEq
                    ( typePtoTypeVar ttypepch
                    , _TypeTopBotF # _TypeChAnnCmd # cmd )
                ]

        return (_CClose # (cxt, ch'), eqns)
    
    -- duplicated code from the CClose case...
    CHalt cxt ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag

        envLcl % typeInfoSymTab % symTabCh % at (ch ^. uniqueTag) .= Nothing

        let ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh )

            ttypepch = annotateTypeTag ttypech ch
            eqns = 
                [ genStableEqn ttypechstable ttypepch
                -- the type is top bot..
                , TypeEqnsEq
                    ( typePtoTypeVar ttypepch
                    , _TypeTopBotF # _TypeChAnnCmd # cmd )
                ]

        return (_CHalt # (cxt, ch'), eqns)

    CGet cxt patt ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag

        ttypech' <- freshChTypeTag ch

        (ttypepatt, (patt', patteqns)) <- withFreshTypeTag $ typeCheckPattern patt


        let ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh)
            ttypepch = annotateTypeTag ttypech ch
            ttypepch' = annotateTypeTag ttypech' ch

            ttypeppatt = annotateTypeTag ttypepatt patt

            eqn = TypeEqnsExist [ttypepch', ttypeppatt] $
                [ TypeEqnsEq
                    ( typePtoTypeVar ttypepch
                    , inputOutput (ch ^. polarity) 
                        (_TypePutF # 
                            ( _TypeChAnnCmd # cmd
                            , typePtoTypeVar ttypeppatt
                            , typePtoTypeVar ttypepch'))
                        (_TypeGetF # 
                            ( _TypeChAnnCmd # cmd
                            , typePtoTypeVar ttypeppatt
                            , typePtoTypeVar ttypepch'))
                    )

                , genStableEqn ttypechstable $ ttypepch
                ]
                <> patteqns

        return (_CGet # (cxt, patt', ch'), [eqn])

    -- duplciated (except changing the expression)
    CPut cxt expr ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag

        ttypech' <- freshChTypeTag ch

        (ttypeexpr, (expr', expreqns)) <- withFreshTypeTag $ typeCheckExpr expr


        let ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh)
            ttypepch = annotateTypeTag ttypech ch
            ttypepch' = annotateTypeTag ttypech' ch

            ttypepexpr = annotateTypeTag ttypeexpr expr

            eqn = TypeEqnsExist [ttypepch', ttypepexpr] $
                [ TypeEqnsEq
                    ( typePtoTypeVar ttypepch
                    , inputOutput (ch ^. polarity) 
                        (_TypeGetF # 
                            ( _TypeChAnnCmd # cmd
                            , typePtoTypeVar ttypepexpr
                            , typePtoTypeVar ttypepch'))
                        (_TypePutF # 
                            ( _TypeChAnnCmd # cmd
                            , typePtoTypeVar ttypepexpr
                            , typePtoTypeVar ttypepch'))
                    )

                , genStableEqn ttypechstable $ ttypepch
                ]
                <> expreqns

        return (_CPut # (cxt, expr', ch'), [eqn])

    CHCase cxt ch cases -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag

        let ttypepch = annotateTypeTag ttypech ch

        arrenv <- freshInstantiateArrEnv
        ((cases', caseseqns), ttypeinst) <- fmap 
            (NE.unzip *** toListOf (instantiateArrEnvInstantiated % folded) )
            $ (`runStateT` arrenv) 
            $ for cases $ \(cxt, ident, cmds) -> do
                ~(SymEntry clauselkuptp ~(SymConcPhraseCall def)) <- lift $ zoom (envLcl % typeInfoSymTab) $ do
                    res <- guse $ symTabConc % at (ident ^. uniqueTag)
                    tell $ review _InternalError $ maybe [_CannotCallTerm # ident] mempty res
                    return $ fromJust res

                tell $ review _ExternalError 
                    $ inputOutput (ch ^. polarity)
                        ( maybeToList 
                            $ def ^? _CoprotocolDefn 
                                % to (review _HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol . (ch,))
                            )
                        ( maybeToList 
                            $ def ^? _ProtocolDefn 
                                % to ( review _HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol 
                                     . (ch,)))

                let (ttypeargs, unwrappedtp) = fromJust $ clauselkuptp ^? _SymConcPhrase 

                subs <- state $ runState $ updateInstantiatedAndGetSubs ttypeargs

                (ttypech', (cmds', cmdseqns)) <- lift $ localEnvSt id $ do
                    ttypech' <- freshChTypeTag ch
                    (cmds', cmdseqns) <- typeCheckCmds cmds
                    return (ttypech', (cmds', cmdseqns))

                let ttypepunwrapped = fromJust $ instantiateTypeWithSubs subs $ unwrappedtp
                    ttypepclause = fromJust $ case def of
                        ProtocolDefn phrase -> 
                            phrase ^. typePhraseExt
                                % to ( instantiateTypeWithSubs subs 
                                     . typeClauseToMplType)
                        CoprotocolDefn phrase -> 
                            phrase ^. typePhraseExt
                                % to ( instantiateTypeWithSubs subs 
                                     . typeClauseToMplType)
                    ttypepch' = annotateTypeTag ttypech' ch
                    eqns = TypeEqnsExist [ttypepch'] $
                        [ TypeEqnsEq 
                            ( typePtoTypeVar ttypepch'
                            , ttypepunwrapped )
                        , TypeEqnsEq 
                            ( typePtoTypeVar ttypepch
                            , ttypepclause )
                        ]
                        <> cmdseqns

                return ((def, ident, cmds') , [eqns])

        let eqn = TypeEqnsExist ttypeinst $
                [genStableEqn ttypechstable ttypepch] <> fold caseseqns
            ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh)

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return (_CHCase # (cxt, ch', cases'), [eqn])

    CHPut cxt ident ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag
        let ttypepch = annotateTypeTag ttypech ch

        ~(SymEntry clauselkuptp (SymConcPhraseCall def)) <- zoom (envLcl % typeInfoSymTab) $ do
            res <- guse $ symTabConc % at (ident ^. uniqueTag)
            tell $ review _InternalError $ maybe [_CannotCallTerm # ident] mempty res
            return $ fromJust res

        tell $ review _ExternalError 
            $ inputOutput (ch ^. polarity)
                ( maybeToList 
                    $ def ^? _ProtocolDefn 
                        % to ( review _HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol 
                             . (cxt, ch,))
                    )
                ( maybeToList 
                    $ def ^? _CoprotocolDefn 
                        % to ( review _HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol 
                             . (cxt, ch,)))

        ttypech' <- freshChTypeTag ch
        let ttypepch' = annotateTypeTag ttypech' ch

        arrenv <- freshInstantiateArrEnv
        let (ttypeargs, unwrappedtp) = fromJust $ clauselkuptp ^? _SymConcPhrase 
            (ttypepargs, (ttypepclause, ttypepunwrapped)) = (`runInstantiateArrType` arrenv) $ do
                subs <- updateInstantiatedAndGetSubs ttypeargs
                return 
                    ( fromJust $ case def of
                        ProtocolDefn phrase -> 
                            phrase ^. typePhraseExt % to ( instantiateTypeWithSubs subs . typeClauseToMplType )
                        CoprotocolDefn phrase -> 
                            phrase ^. typePhraseExt % to ( instantiateTypeWithSubs subs . typeClauseToMplType )
                    , fromJust $ instantiateTypeWithSubs subs $ unwrappedtp)
            eqn = TypeEqnsExist ([ttypepch'] <> ttypepargs) $
                    [ TypeEqnsEq
                        ( typePtoTypeVar ttypepch
                        , ttypepclause )
                    , TypeEqnsEq
                        ( typePtoTypeVar ttypepch'
                        , ttypepunwrapped
                        )
                    , genStableEqn ttypechstable ttypepch
                    ]
            ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh)
        
        return (_CHPut # ((cxt, def), ident, ch'), [eqn])

    CSplit cxt ch (ch0, ch1) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag
        let ttypepch = annotateTypeTag ttypech ch

        ttypech0 <- freshChTypeTag ch0
        ttypech0stable <- freshTypeTag
        ttypech1 <- freshChTypeTag ch1
        ttypech1stable <- freshTypeTag

        envLcl % typeInfoSymTab % symTabCh % at (ch ^. uniqueTag) .= Nothing

        let ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh )
            ch0' = _ChIdentT # (ch0, fromJust $ ttypemap ^? at ttypech0stable % _Just % _SymTypeCh )
            ch1' = _ChIdentT # (ch1, fromJust $ ttypemap ^? at ttypech1stable % _Just % _SymTypeCh )

            ttypepch0 = annotateTypeTag ttypech0 ch0
            ttypepch1 = annotateTypeTag ttypech1 ch1

            eqn = TypeEqnsExist [ttypepch0, ttypepch1] $ 
                    [ TypeEqnsEq 
                        ( typePtoTypeVar ttypepch
                        , inputOutput (ch ^. polarity)
                            (_TypeTensorF # 
                                ( _TypeChAnnCmd # cmd
                                , typePtoTypeVar ttypepch0
                                , typePtoTypeVar ttypepch1
                                ) 
                            )
                            (_TypeParF # 
                                ( _TypeChAnnCmd # cmd
                                , typePtoTypeVar ttypepch0
                                , typePtoTypeVar ttypepch1
                                ) 
                            )
                        )
                    , genStableEqn ttypechstable ttypepch
                    , genStableEqn ttypech0stable ttypepch0
                    , genStableEqn ttypech1stable ttypepch1
                    ]

        return (_CSplit # (cxt, ch', (ch0', ch1')), [eqn])

    -- duplciated code from the split case...
    CFork cxt ch ((ch0, cxt0, cmds0), (ch1, cxt1, cmds1)) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag
        let ttypepch = annotateTypeTag ttypech ch

        scoped <- guses (envLcl % typeInfoSymTab % symTabCh) (toListOf (itraversed % symEntryInfo))
        let sharedchs = cxt0 `intersect` cxt1
        tell $ review _ExternalError $ bool 
            [_ForkExpectedDisjointChannelsButHasSharedChannels # (cxt, sharedchs)] 
            [] $ null sharedchs
        let nonexhaustive = scoped \\ (ch : cxt0 <> cxt1)
        tell $ review _ExternalError $ bool 
            [_ForkHasChannelsInScopeButContextsAreNonExhaustiveWith #
                ( cxt
                , scoped
                , (cxt0, cxt1)
                , nonexhaustive)
            ] 
            [] $ null nonexhaustive

        ttypecxt0 <- zoom (envLcl % typeInfoSymTab) $ traverse lookupSymCh cxt0
        ttypecxt1 <- zoom (envLcl % typeInfoSymTab) $ traverse lookupSymCh cxt1
        cxt0stables <- traverse (const freshTypeTag) cxt0
        cxt1stables <- traverse (const freshTypeTag) cxt1
        
        -- phrase 1...
        ttypech0 <- freshChTypeTag ch0
        ttypech0stable <- freshTypeTag
        let cxt0tags = ch0 ^. uniqueTag : map (view uniqueTag) cxt0
        (cmds0', cmds0eqns) <- localEnvSt 
            ( over 
                (envLcl % typeInfoSymTab % symTabCh) 
                (Map.filterWithKey (\k _ -> k `elem` cxt0tags))
            ) $ typeCheckCmds cmds0 

        -- phrase 2...
        ttypech1 <- freshChTypeTag ch1
        ttypech1stable <- freshTypeTag
        let cxt1tags = ch1 ^. uniqueTag : map (view uniqueTag) cxt1
        (cmds1', cmds1eqns) <- localEnvSt 
            ( over 
                (envLcl % typeInfoSymTab % symTabCh) 
                (Map.filterWithKey (\k _ -> k `elem` cxt1tags))
            ) $ typeCheckCmds cmds1 


        let ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh )
            ch0' = _ChIdentT # (ch0, fromJust $ ttypemap ^? at ttypech0stable % _Just % _SymTypeCh )
            ch1' = _ChIdentT # (ch1, fromJust $ ttypemap ^? at ttypech1stable % _Just % _SymTypeCh )

            ttypepch0 = annotateTypeTag ttypech0 ch0
            ttypepch1 = annotateTypeTag ttypech1 ch1

            cxt0' = zipWith 
                    (\stable ch -> 
                        _ChIdentT # (ch, fromJust $ ttypemap ^? at stable % _Just % _SymTypeCh )) 
                    cxt0stables cxt0
            cxt1' = zipWith 
                    (\stable ch -> 
                        _ChIdentT # (ch, fromJust $ ttypemap ^? at stable % _Just % _SymTypeCh ))
                    cxt1stables cxt1

            ttypepcxt0 = annotateTypeTags (map (view symEntryType) ttypecxt0) cxt0
            ttypepcxt1 = annotateTypeTags (map (view symEntryType) ttypecxt1) cxt1


            eqn = TypeEqnsExist [ttypepch0, ttypepch1] $ 
                    [ TypeEqnsEq 
                        ( typePtoTypeVar ttypepch
                        , inputOutput (ch ^. polarity)
                            (_TypeParF # 
                                ( _TypeChAnnCmd # cmd
                                , typePtoTypeVar ttypepch0
                                , typePtoTypeVar ttypepch1
                                ) 
                            )
                            (_TypeTensorF # 
                                ( _TypeChAnnCmd # cmd
                                , typePtoTypeVar ttypepch0
                                , typePtoTypeVar ttypepch1
                                ) 
                            )
                        )
                    , genStableEqn ttypechstable ttypepch
                    , genStableEqn ttypech0stable ttypepch0
                    , genStableEqn ttypech1stable ttypepch1
                    ]
                    <> zipWith genStableEqn cxt0stables ttypepcxt0
                    <> zipWith genStableEqn cxt1stables ttypepcxt1
                    <> cmds0eqns 
                    <> cmds1eqns 

        envLcl % typeInfoSymTab % symTabCh .=  mempty

        return 
            ( _CFork # 
                ( cxt
                , ch'
                , ( (ch0', cxt0', cmds0')
                  , (ch1', cxt1', cmds1')
                  ) 
                )
            , [eqn]) 

    CId cxt (ch0, ch1) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech0 info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch0
        ttypech0stable <- freshTypeTag
        let ttypepch0 = annotateTypeTag ttypech0 ch0

        ~(SymEntry ttypech1 info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch1
        ttypech1stable <- freshTypeTag
        let ttypepch1 = annotateTypeTag ttypech1 ch1

        tell $ review _ExternalError $ bool []
            [_IllegalIdGotChannelsOfTheSamePolarityButIdNeedsDifferentPolarity # (cxt, ch0, ch1) ] 
            $ ch0 ^. polarity == ch1 ^. polarity

        let ch0' = _ChIdentT # (ch0, fromJust $ ttypemap ^? at ttypech0stable % _Just % _SymTypeCh )
            ch1' = _ChIdentT # (ch1, fromJust $ ttypemap ^? at ttypech1stable % _Just % _SymTypeCh )

            eqns = 
                [ TypeEqnsEq (typePtoTypeVar ttypepch0, typePtoTypeVar ttypepch1)
                , genStableEqn ttypech0stable ttypepch0
                , genStableEqn ttypech1stable ttypepch1
                ]

        envLcl % typeInfoSymTab % symTabCh % at (ch0 ^. uniqueTag) .= Nothing
        envLcl % typeInfoSymTab % symTabCh % at (ch1 ^. uniqueTag) .= Nothing

        return ( _CId # (cxt, (ch0', ch1')), eqns )
    -- duplciated code..
    CIdNeg cxt (ch0, ch1) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech0 info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch0
        ttypech0stable <- freshTypeTag
        let ttypepch0 = annotateTypeTag ttypech0 ch0

        ~(SymEntry ttypech1 info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch1
        ttypech1stable <- freshTypeTag
        let ttypepch1 = annotateTypeTag ttypech1 ch1

        tell $ review _ExternalError $ bool []
            [ _IllegalIdNegGotChannelsOfDifferentPolarityButIdNegNeedsTheSamePolarity # (cxt, ch0, ch1) ] 
            $ ch0 ^. polarity /= ch1 ^. polarity

        let ch0' = _ChIdentT # (ch0, fromJust $ ttypemap ^? at ttypech0stable % _Just % _SymTypeCh )
            ch1' = _ChIdentT # (ch1, fromJust $ ttypemap ^? at ttypech1stable % _Just % _SymTypeCh )

            eqns = 
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypepch0
                    , _TypeNegF # (_TypeChAnnCmd # cmd, typePtoTypeVar ttypepch1))
                , genStableEqn ttypech0stable ttypepch0
                , genStableEqn ttypech1stable ttypepch1
                ] 

        envLcl % typeInfoSymTab % symTabCh % at (ch0 ^. uniqueTag) .= Nothing
        envLcl % typeInfoSymTab % symTabCh % at (ch1 ^. uniqueTag) .= Nothing

        return ( _CIdNeg # (cxt, (ch0', ch1')), eqns )
    CRace cxt ((ch, cmds) :| races) -> do
        {-
         - the unneccessary error checking?
         - neg (empty or empty) = not empty and not empty
        let inpoutchs = NE.partition (has (_1 % polarity % _Input)) races
        tell $ review _ExternalError $ bool []
            [ _IllegalIdNegGotChannelsOfDifferentPolarityButIdNegNeedsTheSamePolarity # (cxt, ch0, ch1) ] 
            $ inpoutchs
            ch0 ^. polarity /= ch1 ^. polarity
        - (ChIdentR, NonEmpty cmd)
        -}

        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypechstable <- freshTypeTag 
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ttypepch = annotateTypeTag ttypech ch

        (ttypepdummies, geteqn) <- instantiateRaceEqn ch ttypepch

        (cmds', cmdseqns) <- localEnvSt id $ typeCheckCmds cmds

        let ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh )
            raceeqn = TypeEqnsExist ttypepdummies $ 
                [ geteqn 
                , genStableEqn ttypechstable ttypepch ]
                <> cmdseqns

        (races', raceseqns) <- fmap unzip 
            $ flip evalStateT ttypepch
            $ for races $ \(ch, cmds) -> do
                -- duplciated code..
                ~(SymEntry ttypech info) <- lift $ zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
                ttypechstable <- lift $ freshTypeTag 
        
                let ttypepch = annotateTypeTag ttypech ch
        
                (ttypepdummies, geteqn) <- lift $ instantiateRaceEqn ch ttypepch
                (cmds', cmdseqns) <- lift $ localEnvSt id $ typeCheckCmds cmds

                oldch <- equality <<.= ttypepch

                let ch' = _ChIdentT # (ch, fromJust $ ttypemap ^? at ttypechstable % _Just % _SymTypeCh )
                    raceeqn = TypeEqnsExist ttypepdummies $ 
                        [ geteqn 
                        , genStableEqn ttypechstable ttypepch 
                        , TypeEqnsEq (typePtoTypeVar oldch, typePtoTypeVar ttypepch) ]
                        <> cmdseqns

                return ((ch', cmds'), raceeqn)
        let eqns = [ raceeqn ] <> raceseqns

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return (_CRace # (cxt, (ch',cmds') :| races'), eqns)
      where
        -- instantiates the race equations because races must have the type 
        -- be of get or put (depending on polarity)...
        instantiateRaceEqn ch ttypepch = do
            ttypechgetseq <- freshTypeTag
            ttypechgetconc <- freshTypeTag
            let ttypepchgetseq = annotateTypeTag ttypechgetseq cmd
                ttypepchgetconc = annotateTypeTag ttypechgetconc cmd
                eqn = TypeEqnsEq 
                        ( typePtoTypeVar ttypepch 
                        , inputOutput (ch ^. polarity)
                            (_TypePutF # 
                                ( _TypeChAnnCmd # cmd
                                , typePtoTypeVar ttypepchgetseq
                                , typePtoTypeVar ttypepchgetconc
                                ))
                            (_TypeGetF # 
                                ( _TypeChAnnCmd # cmd
                                , typePtoTypeVar ttypepchgetseq
                                , typePtoTypeVar ttypepchgetconc
                                ))
                        )
            return ([ttypepchgetseq, ttypepchgetconc], eqn)

    CPlugs (cxt, plugs) (phr1, phr2, phrs) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        -- see the cut condition things...
        let allphrases = phr1:phr2:phrs
            allphrasesgraph = map (view _2) allphrases
        tell $ review _ExternalError $ cutConditions allphrasesgraph
        tell $ review _ExternalError $ cutCycles $ NE.fromList allphrasesgraph

        -- Get the map for the stable equations
        let allchs = nubBy ((==) `on` view uniqueTag) 
                $ foldMapOf (folded % _2 % each % folded) (pure . view uniqueTag) 
                $ phr1:phr2:phrs
            allchsids = map (view uniqueTag) allchs
        ttypechsstable <- traverse (const freshTypeTag) allchs 
        let stablerefs = Map.fromList $ zip allchs ttypechsstable

        ~((phr1':phr2':phrs', acceqns), ttypesplugged) <- flip runStateT Map.empty $ fmap unzip $ for allphrases $ \(cxt, (ins, outs), cmds) -> do
            let insr = map (view identR) ins
                outsr = map (view identR) outs
                pluggedins = map (review _ChIdentR . (,Input)) $ filter (\identr -> identr `elem` insr ) plugs 
                pluggedouts = map (review _ChIdentR . (,Output)) $ filter (\identr -> identr `elem` outsr ) plugs 

                phrsechsids = map (view uniqueTag) $ ins <> outs

            for_ (pluggedins <> pluggedouts) $ \ch -> do
                ch' <- lift $ freshChTypeTag ch
                at (ch ^. uniqueTag) %= Just . maybe (ch' :| []) (NE.cons ch')

            lift $ tell $ review _ExternalError $ foldMap expectedInputPolarity ins
                <> foldMap expectedOutputPolarity outs

            (cmds', cmdseqns) <- lift 
                $ localEnvSt (over (envLcl % typeInfoSymTab % symTabCh) (Map.filterWithKey (\k _ -> k `elem` phrsechsids)) )
                $ typeCheckCmds cmds

            let eqns = cmdseqns
                ins' = map (\ch -> _ChIdentT # 
                        ( ch
                        , fromJust $ ttypemap ^? at (fromJust $ stablerefs ^. at (ch ^. uniqueTag)) % _Just % _SymTypeCh 
                        )) ins
                outs' = map (\ch -> _ChIdentT # 
                        ( ch
                        , fromJust $ ttypemap ^? at (fromJust $ stablerefs ^. at (ch ^. uniqueTag)) % _Just % _SymTypeCh 
                        )) outs

            return ((cxt, (ins', outs'), cmds'), eqns)

        let (exists, pluggedeqns) = second fold 
                $ unzip 
                $ flip concatMap plugs 
                $ (\ch -> 
                    let f (h :| hs) = (:) (annotateTypeTag h (), []) 
                            $ map (\(prev, curr) -> 
                                    ( annotateTypeTag curr ()
                                    , [ TypeEqnsEq 
                                        ( typePtoTypeVar $ annotateTypeTag prev ()
                                        , typePtoTypeVar $ annotateTypeTag curr ()) ]
                                    ) 
                                ) 
                            $ zip (h:hs) hs
                    in f . fromJust $ ttypesplugged ^. at (ch ^. uniqueTag))

            eqn = TypeEqnsExist exists $ pluggedeqns <> fold acceqns
            plugs' = map (\ch -> (ch, fromJust $ ttypemap ^? at (fromJust $ stablerefs ^. at (ch ^. uniqueTag)) % _Just % _SymTypeCh)) plugs

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return (_CPlugs # ((cxt, plugs'), (phr1', phr2',phrs')), [eqn])

    CCase cxt expr cases -> do
        (ttypeexpr, (expr', expreqn)) <- withFreshTypeTag $ typeCheckExpr expr
        ttypeexprstable <- freshTypeTag
        (ttypespatts, (cases', caseseqns)) <- fmap (second NE.unzip . NE.unzip)
            $ for cases $ \(patt, cmds) -> localEnvSt id $ do
                (ttypepatt, (patt', patteqn)) <- withFreshTypeTag $ typeCheckPattern patt
                (cmds', cmdseqns) <- typeCheckCmds cmds
                return (ttypepatt, ((patt', cmds'), patteqn <> cmdseqns))

        let ttypepexpr = annotateTypeTag ttypeexpr expr
            ttypesppatts = annotateTypeTags (NE.toList ttypespatts) (NE.toList $ fmap fst cases)
            eqn = TypeEqnsExist (ttypepexpr : ttypesppatts) $ 
                [ genStableEqn ttypeexprstable ttypepexpr ] 
                <> map (review _TypeEqnsEq . (typePtoTypeVar ttypepexpr,) . typePtoTypeVar) ttypesppatts
                <> expreqn
                <> fold caseseqns

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return (_CCase # (cxt, expr', cases'), [eqn])
    CSwitch cxt switches -> panicNotImplemented


-------------------------
-- Kind checking
-------------------------
kindCheckProcessType :: 
    TypeCheck 
        ([TypeP MplRenamed], [MplType MplRenamed], [MplType MplRenamed], [MplType MplRenamed]) 
        (Maybe ([TypeP MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked]))
kindCheckProcessType proctype@(varsyms, seqs, ins, outs) = do
    symtab <- guse (envLcl % typeInfoSymTab % symTabType)
    
    ~(res, st) <- 
        ( flip runStateT $ _KindCheckEnv #
            ( SeqKind ()
            , Map.fromList $ map (view uniqueTag &&& const Nothing ) varsyms
            ) )
        . ( flip runReaderT symtab) $ do
            seqs' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= SeqKind ()
                    primitiveKindCheck mpltype) seqs
            ins' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= ConcKind ()
                    primitiveKindCheck mpltype) ins
            outs' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= ConcKind ()
                    primitiveKindCheck mpltype) outs
            return $ (,,) <$> sequenceA seqs' <*> sequenceA ins' <*> sequenceA outs'

    return $ do
        ~(seqs',ins',outs') <- res
        return (map NamedType varsyms , seqs', ins', outs')

kindCheckFunType ::
    TypeCheck 
        ([TypeP MplRenamed], [MplType MplRenamed], MplType MplRenamed) 
        ([TypeP MplTypeChecked], [MplType MplTypeChecked], MplType MplTypeChecked)
kindCheckFunType proctype@(varsyms, froms, to) = do
    ~symtab <- guse (envLcl % typeInfoSymTab % symTabType)
    
    ~(froms', to') <- 
        ( flip evalStateT $ _KindCheckEnv # 
            ( SeqKind ()
            , Map.fromList $ map (view uniqueTag &&& const Nothing ) varsyms
            ) ) 
        . (flip runReaderT symtab) $ do
            ~froms' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= SeqKind ()
                    primitiveKindCheck mpltype)
                    froms

            kindCheckExpectedPrimitiveKind .= SeqKind ()
            to' <- primitiveKindCheck to
            return $ (map fromJust froms', fromJust to')

    return (map NamedType varsyms , froms', to')

-------------------------
-- Utilities
-------------------------
freshChTypeTag :: TypeCheck ChIdentR TypeTag
freshChTypeTag ch = do
    tag <- freshTypeTag
    envLcl % typeInfoSymTab % symTabCh % at (ch ^. uniqueTag) ?=
        _SymEntry # (tag, ch) 
    return tag
