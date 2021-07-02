{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.PatternCompiler.PatternCompile where

import Optics
import Optics.State.Operators

import Data.Function

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplPatternCompiled
import MplAST.MplProgUtil

import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow
import Control.Applicative.Lift
import Data.Functor.Compose
import Data.Kind
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Either


import MplPasses.PatternCompiler.PatternCompileErrors
import MplPasses.PatternCompiler.PatternCompileUtils
import MplPasses.PatternCompiler.PatternCompileMplExprSub
import MplPasses.PatternCompiler.PatternCompileUtils

import Data.Foldable
import qualified Data.Map as Map
import Data.Map ( Map (..) )

import Control.Exception

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Functor.Foldable (cata, embed)
import Data.Tuple
import Data.Traversable
import Data.Bool

import Debug.Trace
import Data.Proxy

import Unsafe.Coerce


runPatternCompile' ::
    AsPatternCompileErrors err => 
    (TopLevel, UniqueSupply) -> 
    MplProg MplTypeChecked ->
    Either [err] (MplProg MplPatternCompiled)
runPatternCompile' (top, sup) = flip evalState (_Env # (top, sup, mempty, mempty)) . runPatternCompile

runPatternCompile ::
    forall err m .
    ( MonadState PatternCompileEnv m 
    , AsPatternCompileErrors err )  => 
    MplProg MplTypeChecked ->
    m (Either [err] (MplProg MplPatternCompiled))
runPatternCompile (MplProg prog) = go 
  where
    go :: m (Either [err] (MplProg MplPatternCompiled))
    go = do 
        ~(v, lg) <- runWriterT $ traverse patternCompileStmt prog
        if null lg
            then return $ Right $ MplProg v
            else return $ Left $ lg
{-
runPatternCompile (MplProg prog) = fmap runErrors $ getCompose go
  where
    go :: Compose m (Errors [err]) (MplProg MplPatternCompiled)
    go = fmap MplProg $ traverse 
        ( Compose
        . fmap eitherToErrors 
        . runExceptT 
        . patternCompileStmt )  
        prog
-}

patternCompileStmt ::
    PatternCompile (MplStmt MplTypeChecked) (MplStmt MplPatternCompiled) 
patternCompileStmt stmt = do
    wheres' <- traverse patternCompileStmt wheres
    defns' <- traverse patternCompileDefn defns
    return $ MplStmt defns' wheres'
  where
    wheres = stmt ^. stmtWhereBindings
    defns = stmt ^. stmtDefns

patternCompileDefn :: 
    PatternCompile (MplDefn MplTypeChecked) (MplDefn MplPatternCompiled)
patternCompileDefn (ObjectDefn obj) = fmap ObjectDefn $ patternCompileObj obj
patternCompileDefn (FunctionDefn fun) = fmap FunctionDefn $ patternCompileFunDefn fun
patternCompileDefn (ProcessDefn proc) = fmap ProcessDefn $ patternCompileProcessDefn proc

-- | patternCompileObj.  This is essentially the identity function to juggle some types around.
-- TODO: I guess I should probabaly write out the translation instead of just using 'unsafeCoerce'
patternCompileObj :: 
    PatternCompile
        (MplObjectDefn MplTypeChecked)
        (MplObjectDefn MplPatternCompiled)
patternCompileObj inp = return $ unsafeCoerce inp


-- | patternCompileExpr. pattern compiles an expression -- note this is mostly just recursing through
-- the data structure, but it needs to fix @case@s given by the user; and this recurses through the 
-- expression
patternCompileExpr ::
    PatternCompile (MplExpr MplTypeChecked) (MplExpr MplPatternCompiled)
patternCompileExpr = cata go
  where
    go :: 
        MplExprF (MplPass 'TypeChecked) (_ (MplExpr MplPatternCompiled)) -> 
        _ (MplExpr MplPatternCompiled)
    go = \case
        EPOpsF ann primop l r -> EPOps ann primop <$> l <*> r
        EVarF ann ident -> pure $ EVar ann ident
        EIntF ann n -> pure $ EInt (snd ann) n
        ECharF ann v -> pure $ EChar (snd ann) v
        EDoubleF ann v -> pure $ EDouble (snd ann) v
        EBoolF ann v -> pure $ EBool (snd ann) v
        -- ECaseF ann expr cases -> ECase ann <$> expr <*> traverse sequenceA cases
        ECaseF ann expr cases -> do
            expr' <- expr
            cases' <- sequenceOf (traversed % _2) cases
            let caseslst' = fmap (over _1 pure) cases'
                exhstchk = patternCompileExhaustiveCheck $ fmap fst caseslst'
            tell $ bool [_NonExhaustiveECasePatt # ()] [] exhstchk

            ~([PVar _ u], nbdy) <- patternCompileSeqPatPhrases $ caseslst'
            return $ substituteVarIdentByExpr (u, expr') nbdy

        EObjCallF ann ident exprs -> EObjCall ann ident <$> sequenceA exprs
        -- TODO: we need to check if creating records is exhaustive..
        ERecordF ann phrases -> do
            phrases' <- for phrases $ \(phrase, identt, (patts, mexpr)) -> do
                let exhstchk = patternCompileExhaustiveCheck (patts :| [])
                tell $ bool [ _NonExhaustiveRecordPatt # identt] [] exhstchk
                expr <- mexpr
                (us, expr') <- patternCompileSeqPatPhrases ((patts, expr) :| [])

                return (phrase, identt, (us, expr')) 

            return $ _ERecord # (snd ann, phrases')

        ECallF ann ident exprs -> ECall ann ident <$> sequenceA exprs 
        EListF ann exprs -> EList (snd ann) <$> sequenceA exprs
        EStringF ann str -> pure $ EString (snd ann) str
        EUnitF ann -> pure $ EUnit $ snd ann
        ETupleF ann (mt0,mt1,mts) -> fmap (ETuple (snd ann)) $ (,,) <$> mt0 <*> mt1 <*> sequenceA mts
        EBuiltInOpF ann op l r -> EBuiltInOp (snd ann) op <$> l <*> r
        EIfF ann iff thenf elsef -> EIf ann <$> iff <*> thenf <*> elsef

        ESwitchF ann switches -> do
            sequenceOf (traversed % each) switches 
                >>= \switches' -> patternCompileExhaustiveESwitchCheck switches'
                    >>= return . foldr f (EIllegalInstr $ getExprType $ snd $ NE.head switches')
          where
            f :: 
                (MplExpr MplPatternCompiled, MplExpr MplPatternCompiled) -> 
                MplExpr MplPatternCompiled -> 
                MplExpr MplPatternCompiled
            f (bexpr, thenc) acceq = EIf (getExprType thenc) bexpr thenc acceq
            
        ELetF ann lets expr -> ELet ann <$> (traverse patternCompileStmt lets) <*> expr

        {- We gave up on folds and unfolds
        EFoldF ann foldon phrases -> EFold ann foldon phrases
        EUnfoldF ann expr phrases -> EUnfold ann expr phrases
        XExprF ann -> XExpr ann
        -}

-- | Pattern compiles a function definition.
patternCompileFunDefn :: 
    PatternCompile (XFunctionDefn MplTypeChecked) (XFunctionDefn MplPatternCompiled)
patternCompileFunDefn (MplFunction funName funTp funDefn) = do
    funDefn' <- traverseOf (traversed % _2) patternCompileExpr funDefn

    let exhstchk = patternCompileExhaustiveCheck $ fmap fst funDefn'
    tell $ bool [_NonExhaustiveFunPatt # funName] [] exhstchk

    (patts, pattexpr) <- patternCompileSeqPatPhrases funDefn'
    return $ MplFunction funName funTp $ (patts, pattexpr)  :| []

-- | Pattern compiles a process definition
patternCompileProcessDefn ::
    PatternCompile (XProcessDefn MplTypeChecked) (XProcessDefn MplPatternCompiled)
patternCompileProcessDefn (MplProcess procName procTp procDefn) = do
    cmds@(cmd :| rstcmds) <- traverseOf (traversed % _2) patternCompileCmds procDefn 
        :: _ (NonEmpty (([MplPattern MplTypeChecked], [ChIdentT], [ChIdentT]), NonEmpty (MplCmd MplPatternCompiled)))
    -- we need to rescope all the channels to use the same variable names...
    let cmds0ins = view (_1 % _2)  cmd
        cmds0outs = view (_1 % _3) cmd
        rescope ((patts, ins, outs), cmdblk) =  
            let subTo chs chs' blk = foldr (\sub -> fmap (substituteCh sub)) blk (zip chs chs')
            in subTo outs cmds0outs $ subTo ins cmds0ins cmdblk
        rstcmds' = fmap rescope rstcmds

        cmds' = view _2 cmd :| rstcmds'

        patts = fmap (view (_1 % _1)) procDefn


    tell $ bool [ _NonExhaustiveProcPatt # procName] [] $ patternCompileExhaustiveCheck patts

    (npatts, ncmds) <- patternCompileConcPatPhrases $ NE.zip patts cmds'

    return $ MplProcess procName procTp $ ((npatts, cmds0ins, cmds0outs), ncmds) :| []

patternCompileCmds :: 
    PatternCompile 
        (NonEmpty (MplCmd MplTypeChecked)) 
        (NonEmpty (MplCmd MplPatternCompiled))
patternCompileCmds = fmap NE.fromList . go . NE.toList
  where
    go :: PatternCompile [MplCmd MplTypeChecked] [MplCmd MplPatternCompiled]
    go [] = pure []
    go (cmd:cmds) = case cmd of
        CRun ann idp seqs ins outs -> do
            seqs' <- traverse patternCompileExpr seqs
            (CRun ann idp seqs' ins outs:) <$> go cmds
        CClose ann chp -> (CClose ann chp:) <$> go cmds
        CHalt ann chp -> (CHalt ann chp:) <$> go cmds
        CGet ann patt chp -> do
            let exhstchk = patternCompileExhaustiveCheck ([patt] :| [])
            tell $ bool [ _NonExhaustiveGet # ann ] [] exhstchk 
            u <- freshUIdP
            let utp = getPattType patt
                uexpr = _EVar # (utp, u) :: MplExpr MplPatternCompiled
                upatt = _PVar # (utp, u) :: MplPattern MplPatternCompiled

            cmds'' <- go cmds >>= \cmds' -> for cmds' $ \cmd -> do
                let k expr = do
                        ~([PVar _ u'], expr') <- patternCompileSeqPatPhrases (([patt], expr) :| [])
                        return $ substituteVarIdentByExpr (u', uexpr) expr'
                traverseMplExpr k cmd

            return $ CGet ann upatt chp : cmds''


        CPut ann expr chp -> do
            expr' <- patternCompileExpr expr
            (CPut ann expr' chp:) <$> go cmds
        CHCase ann chp phrases -> do
            phrases' <- traverseOf (traversed % _3) patternCompileCmds phrases
            -- TODO: should do some exhaustiveness checking? 
            (CHCase ann chp phrases':) <$> go cmds

        CHPut ann idp chp -> 
            (CHPut ann idp chp:) <$> go cmds

        CSplit ann chp chs ->
            (CSplit ann chp chs:) <$> go cmds
            
        CFork ann chp phrases -> do
            phrases' <- traverseOf (each % _3) patternCompileCmds phrases
            (CFork ann chp phrases':) <$> go cmds
            
        CId ann chs -> (CId ann chs:) <$> go cmds
        CIdNeg ann chs -> (CIdNeg ann chs:) <$> go cmds

        CRace ann races -> do
            races' <- traverseOf (traversed % _2 ) patternCompileCmds races
            -- (CRace ann races':) <$> go cmds
            return $ [CRace ann races']
            -- @go cmds@ should be empy here

        -- CPlug !(XCPlug x) (CPlugPhrase x, CPlugPhrase x)
        CPlugs ann (phrase0, phrase1, phrases) -> do
            ~(phrase0':phrase1':phrases') <- 
                traverseOf (traversed % _3) patternCompileCmds 
                $ phrase0:phrase1:phrases
            -- _ :: ((), ([ChIdentT], [ChIdentT]), NonEmpty (MplCmd MplTypeChecked))
            return $ [CPlugs ann (phrase0', phrase1', phrases')]
            -- return $ (CPlugs ann (phrase0', phrase1', phrases'):) <$>  go cmds
            -- @go cmds@ should always be empty here

        CCase ann expr pattscmds -> do
            expr' <- patternCompileExpr expr
            pattscmds' <- fmap (over (mapped % _1) pure) $ traverseOf (traversed % _2) patternCompileCmds pattscmds
            ~([PVar _ u], ncmds) <- patternCompileConcPatPhrases pattscmds'
            let ncmds' = fmap (substituteVarIdentByExpr (u, expr')) ncmds

            tell $ bool [ _NonExhaustiveCCasePatt  # () ] [] $ 
                patternCompileExhaustiveCheck $ fmap (view _1) pattscmds'

            -- (NE.toList ncmds' <>) <$> go cmds
            return $ NE.toList ncmds' 
            -- @go cmds@ should always be empty list here

        CSwitch ann switches -> do
            traverseOf (traversed % _2) patternCompileCmds switches
                >>= traverseOf (traversed % _1) patternCompileExpr
                >>= patternCompileExhaustiveCSwitchCheck 
                >>= return . NE.toList . foldr f (pure $ CIllegalInstr ())
            -- @go cmds@ should always be empty list here
          where
            f (bexpr, thenc) acceq = pure $ CIf () bexpr thenc acceq

        CIf ann cond cthen celse -> 
            fmap pure $ CIf () <$> patternCompileExpr cond <*> patternCompileCmds cthen <*> patternCompileCmds celse
            -- @go cmds@ should always be empty list here

{-
  CCase !(XCCase x)
          (XMplExpr x)
          (NonEmpty (XMplPattern x, NonEmpty (MplCmd x)))
  CSwitch !(XCSwitch x)
            (NonEmpty (XMplExpr x, NonEmpty (MplCmd x)))
-}

{- | Tests if a switch statement is exhaustive.
This will 'tell' an error if the pattern is non exhaustive, and return only the switches up to (and including)
the True.
-}
patternCompileExhaustiveESwitchCheck ::
    PatternCompile 
        (NonEmpty (MplExpr MplPatternCompiled, MplExpr MplPatternCompiled)) 
        (NonEmpty (MplExpr MplPatternCompiled, MplExpr MplPatternCompiled))
patternCompileExhaustiveESwitchCheck switches = 
     case trueandpast of
        [] -> tell [_NonExhaustiveSwitch # ()] >> return switches
        trueres:_ -> return $ NE.fromList $ notrue <> [trueres]
  where
    (notrue, trueandpast) = NE.break (fromMaybe False . preview (_1 % _EBool % _2)) switches

{- essentially duplciated code -}
patternCompileExhaustiveCSwitchCheck ::
    PatternCompile 
        (NonEmpty (MplExpr MplPatternCompiled, NonEmpty (MplCmd MplPatternCompiled))) 
        (NonEmpty (MplExpr MplPatternCompiled, NonEmpty (MplCmd MplPatternCompiled)))
patternCompileExhaustiveCSwitchCheck switches = 
     case trueandpast of
        [] -> tell [_NonExhaustiveCSwitch # ()] >> return switches
        trueres:_ -> return $ NE.fromList $ notrue <> [trueres]
  where
    (notrue, trueandpast) = NE.break (fromMaybe False . preview (_1 % _EBool % _2)) switches
    


{- | Tests if a pattern match is exhaustive i.e., tests if something like
@
    SomePattern(a,b,MorePatterns(c)), a,b -> < .. >
    f,a,b -> < .. >
@
is exhaustive. Returns true if it is, and false if it is not.
-}
patternCompileExhaustiveCheck ::
    NonEmpty [MplPattern MplTypeChecked] -> 
    Bool
patternCompileExhaustiveCheck = and . fmap go . transpose . NE.toList
  where
    go :: [MplPattern MplTypeChecked] -> Bool
    -- assert all patterns should be the same length
    go [] = True
    go patts@(a:as) = or 
        [ has (folded % _PVar) patts
        , has (folded % _PNull) patts

        -- each of the vertical lines of matches on records must be exhaustive as well.
        , and 
            [ isJust mdts
            , getAll $ foldMap (All . patternCompileExhaustiveCheck . NE.fromList . transpose . pure) dtspatts'
            ]

        -- tuple 
        , and 
            [ isJust mtuple
            , patternCompileExhaustiveCheck $ NE.fromList tuple
            ]

        -- we can actually exhaust all the bool types 
        -- (unlike Chars and Ints.. we just assume it is impossible to exhaust them completely)
        , and 
            [ isJust $ traverse  (preview _PBool) patts
            , orOf (folded % _PBool % _2) patts
            , orOf (folded % _PBool % _2 % to not) patts
            ]

        -- the built in list
        , and
            -- if all are lists, and we have a cons, and a null we are exhaustive 
            [ all (\patt -> has _PListCons patt || has _PList patt) patts
            , orOf (folded % _PListCons % to (const True)) patts
            , orOf (folded % _PList % _2 % to null) patts
            ]

        -- the built in unit test
        , and
            -- if all are units, we are done
            [ all (\patt -> has _PUnit patt) patts
            ]
            
        -- each constructor is present in this vertical line of patterns, and each of the patterns
        -- are itself exhaustive
        , and 
            [ isJust mcts
            , andOf (folded % typePhraseName % to (`elem` over mapped (view _2) cts)) allphrases
            , andOf (folded % to patternCompileExhaustiveCheck) (over (mapped % mapped) (view _3) groupedcts)
            ]
        ]
      where
        -- constructor
        mcts :: Maybe 
            [ 
                ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag) , XMplType MplTypeChecked)
                , IdentT
                , [MplPattern (MplPass 'TypeChecked)])
            ] 
        mcts = traverse (preview _PConstructor) patts
        cts = fromJust mcts
        
        groupedcts ::
            [ 
                NonEmpty
                ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag) , XMplType MplTypeChecked)
                , IdentT
                , [MplPattern (MplPass 'TypeChecked)]
                )
            ]
        groupedcts = NE.groupBy ((==) `on` view _2) $ sortBy (compare `on` view _2) cts
        tpclause = head cts ^. _1 % _1 % typePhraseExt :: MplTypeClause MplTypeChecked ('SeqObjTag 'DataDefnTag)
        allphrases = tpclause ^. typeClausePhrases

        -- destructors
        mdts :: Maybe
            [ 
                ( (Location, XMplType MplTypeChecked)
                , NonEmpty
                    ( MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag)
                    , IdentT
                    , MplPattern (MplPass 'TypeChecked)
                    )
                )
            ]
        mdts = traverse (preview _PRecord) patts
        dts = fromJust mdts

        {- Note: 
         -      - this will get the destructor name, and the patterns
         -      - then, it will put all the patterns for each destructor in a list
         -}
        dtspatts = dts 
            & mapped %~ view _2
            & mapped %~ NE.toList 
            & mapped % mapped %~ (view _2 &&& view _3)
        dtspatts' = Map.elems
            $ flip execState Map.empty
            $ forOf (traversed % traversed) dtspatts 
            $ \(dts, patt) -> modify (Map.alter (Just . maybe [patt] (patt:)) dts)

        -- tuple
        mtuple :: Maybe
               [((Location, XMplType MplTypeChecked),
                 (MplPattern (MplPass 'TypeChecked),
                  MplPattern (MplPass 'TypeChecked),
                  [MplPattern (MplPass 'TypeChecked)]))]
        mtuple = traverse (preview _PTuple) patts
        tuple = fromJust (traverse (preview _PTuple) patts)
            & mapped %~ view _2
            & mapped %~ \(a,b,c) -> a:b:c


{- | Pattern compiles a pattern phrase i.e., this will pattern compile something like
@
    SomePattern(a,b,MorePatterns(c)), a,b -> <final result expression>
    f,a,b -> <final result expression>
    ....
@
to replace the patterns with variables and translate them into cases in the final expression.

-}
patternCompileSeqPatPhrases ::
    forall s m .
    ( MonadState s m
    , HasUniqueSupply s ) =>
    (NonEmpty ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)) -> 
    m ([MplPattern MplPatternCompiled], MplExpr MplPatternCompiled)
patternCompileSeqPatPhrases pattphrases = 
    -- assert that all the patterns have the same length
    assert ((length $ NE.group $ fmap (length . fst) pattphrases) == 1) $ do
        -- get the number of patterns
        let numpats = length $ fst $ NE.head pattphrases 
        -- then, we want to replace all the patterns with these unique pattern variables
        us <- replicateM numpats freshUIdP 
        -- indeed, we maintain the type information
        let usandtp = zipWith (curry (second getPattType)) us (fst . NE.head $ pattphrases) 
            (patts :: [MplPattern MplPatternCompiled]) = fmap (review _PVar . swap) usandtp

        pattexpr <- go (map VarSub usandtp) (NE.toList pattphrases) (EIllegalInstr restp) 

        return (patts, pattexpr)  
  where
    -- the type of the codomain 
    restp :: XMplType MplTypeChecked
    restp = getExprType $ snd $ NE.head pattphrases

    go :: 
        [Substitutable]  -> 
        [([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)] -> 
        MplExpr MplPatternCompiled -> 
        _ (MplExpr MplPatternCompiled)
    go [] patts mexpr 
        | null patts = pure $ mexpr
        | otherwise = pure $ snd $ head patts 
    go (u:us) patts mexpr = case groupedpatts of
        -- the case when all patts are of the same type
        _ :| [] -> fmap (fromJust . asum) 
            $ sequenceA 
                [ varrule
                , ctersrule
                , dstersrule
                , tuplerule
                , intrule
                , boolrule
                , charrule
                , listrule
                , unitrule
                ]
          where
            varrule :: _ (Maybe (MplExpr MplPatternCompiled))
            varrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                match :: MplPattern MplTypeChecked -> Maybe (Either () IdentT)
                match patt
                    | Just identt <- patt ^? _PVar % _2 = _Just % _Right # identt
                    | Just _ <- patt ^? _PNull = _Just % _Left # ()
                    | otherwise = Nothing

                f :: NonEmpty (Either () IdentT) -> m (MplExpr MplPatternCompiled)
                f vars = go 
                    us 
                    (NE.toList $ NE.zipWith 
                        h
                        vars 
                        patttails
                        ) 
                    mexpr

                {- Either is
                 -      Left: a null pattern @_@
                 -      Right: a var pattern @u@
                 -}
                h :: Either () IdentT -> 
                    ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled) -> 
                    ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                h (Right v) pattsexpr = pattsexpr & _2 %~ substituteExpr (v,u)
                h _ pattsexpr = pattsexpr 

            -- rule for a built in list.. (essentially 'ctersrule' but hardcoded for lists)
            listrule  :: _ (Maybe (MplExpr MplPatternCompiled))
            listrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                -- Left is PListCons
                -- Right is Either PList or PString
                match :: MplPattern MplTypeChecked -> Maybe 
                    (Either 
                        ((Location, MplType MplTypeChecked), MplPattern MplTypeChecked, MplPattern MplTypeChecked)
                        (
                        Either
                            ((Location, MplType MplTypeChecked), [MplPattern MplTypeChecked])
                            ((Location, MplType MplTypeChecked), String)
                        )
                    )
                match = \case 
                    PListCons cxt a b -> Just $ Left (cxt,a,b)
                    PList cxt lst -> Just $ Right $ Left (cxt, lst)
                    PString cxt str -> Just $ Right $ Right (cxt, str)
                    _ -> Nothing

                f :: NonEmpty _ -> m (MplExpr MplPatternCompiled)
                f cters = fmap (ECase restp (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [lstcons, lstnil]
                    -- [ lstcons $ mapMaybe g0 $ NE.toList cters , lstnil  $ mapMaybe g1 $ NE.toList cters ]
                  where 
                    (consacc, nilacc) = foldl' g mempty $ reverse $ zip (NE.toList cters) (NE.toList patttails)
                      where
                        g acc (lstpatt, patttail) = case lstpatt of
                            Left (ann, l, r) -> first ((patttail & _1 %~ ([l, r]<>)):) acc
                            -- regular list
                            Right (Left (ann, l:r)) -> first ((patttail & _1 %~ ([l, PList ann r]<>)):) acc
                            Right (Left (ann, [])) -> second (patttail:) acc 
                            -- string
                            Right (Right (ann, [])) -> second (patttail:) acc 
                            Right (Right (ann, l:r)) -> 
                                first ((patttail & _1 %~ ([PChar ann l, PList ann $ map (PChar ann) r]<>)):) acc

                    -- get the type of the list.. this is a bit of a mess just by design of the 
                    -- language
                    ~tplst@(TypeBuiltIn (TypeListF _ tphead)) = getPattType $ fst $ NE.head pattheads 

                    lstcons = do
                        u0 <- freshUIdP
                        u1 <- freshUIdP

                        let patt' =  PSimpleListCons tplst u0 u1

                        if null consacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go (map VarSub [(u0, tphead), (u1, tplst)] ++ us) consacc mexpr

                    lstnil = do
                        let patt' =  PSimpleListEmpty tplst 

                        if null nilacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go us nilacc mexpr

                {-
                match :: MplPattern MplTypeChecked -> Maybe 
                    (Either 
                        ((Location, MplType MplTypeChecked), MplPattern MplTypeChecked, MplPattern MplTypeChecked)
                        ((Location, MplType MplTypeChecked), [MplPattern MplTypeChecked])
                    )
                match = \case 
                    PListCons cxt a b -> Just $ Left (cxt,a,b)
                    PList cxt lst -> Just $ Right (cxt, lst)
                    _ -> Nothing


                f :: NonEmpty _ -> m (MplExpr MplPatternCompiled)
                f cters = fmap (ECase restp (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [lstcons, lstnil]
                    -- [ lstcons $ mapMaybe g0 $ NE.toList cters , lstnil  $ mapMaybe g1 $ NE.toList cters ]
                  where 
                    (consacc, nilacc) = foldl' g mempty $ reverse $ zip (NE.toList cters) (NE.toList patttails)
                      where
                        g acc (lstpatt, patttail) = case lstpatt of
                            Left (ann, l, r) -> first ((patttail & _1 %~ ([l, r]<>)):) acc
                            Right (ann, l:r) -> first ((patttail & _1 %~ ([l, PList ann r]<>)):) acc
                            Right (ann, []) -> second (patttail:) acc 

                    -- get the type of the list.. this is a bit of a mess just by design of the 
                    -- language
                    ~tplst@(TypeBuiltIn (TypeListF _ tphead)) = getPattType $ fst $ NE.head pattheads 

                    lstcons = do
                        u0 <- freshUIdP
                        u1 <- freshUIdP

                        let patt' =  PSimpleListCons tplst u0 u1

                        if null consacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go (map VarSub [(u0, tphead), (u1, tplst)] ++ us) consacc mexpr

                    lstnil = do
                        let patt' =  PSimpleListEmpty tplst 

                        if null nilacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go us nilacc mexpr
                    -}


            -- hard coded case for the unit rule
            unitrule  :: _ (Maybe (MplExpr MplPatternCompiled))
            unitrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                match = preview _PSimpleUnit 

                f :: NonEmpty (XPSimpleUnit (MplPass 'TypeChecked)) -> m (MplExpr MplPatternCompiled)
                f cters = fmap (ECase restp (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [unitpat]
                  where
                    unitpat = do
                        let patt' = PSimpleUnit ty
                        fmap (patt',) $ go us (NE.toList patttails) mexpr


            ctersrule :: _ (Maybe (MplExpr MplPatternCompiled))
            ctersrule = sequenceA $ match pattheads <&> f
              where
                match :: 
                    NonEmpty (MplPattern MplTypeChecked, MplExpr MplPatternCompiled) -> 
                    Maybe 
                        ( NonEmpty 
                            ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), MplType MplTypeChecked)
                            , IdentT
                            , [MplPattern (MplPass 'TypeChecked)])
                        )
                match = traverse (preview (_1 % _PConstructor))

                f :: NonEmpty 
                    ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), XMplType MplTypeChecked)
                    , IdentT
                    , [MplPattern (MplPass 'TypeChecked)]) -> _ (MplExpr MplPatternCompiled)
                f cters = fmap (ECase restp (getExprFromSubstitutable u) . NE.fromList) 
                    $ for (tpclause ^. typeClausePhrases) $ \phrase -> do
                        usandtps <- traverse (sequenceOf _1 . (freshUIdP,)) 
                            $ phrase ^. typePhraseFrom
                        -- let patt' = PSimpleConstructor (phrase, snd nu) (phrase ^. typePhraseName) $ usandtps
                        let patt' = PSimpleConstructor 
                                ( phrase
                                , getDataPhraseTypeResult phrase
                                ) 
                                (phrase ^. typePhraseName) 
                                $ usandtps
                        case ctersmap ^. at (phrase ^. typePhraseName) of
                            Just cternpatts -> (patt',) <$> 
                                go (map VarSub usandtps ++ us) (NE.toList cternpatts) mexpr
                            Nothing -> pure $ (patt',) mexpr
                    
                  where
                    tpclause = NE.head cters ^. _1 % _1 % typePhraseExt :: MplTypeClause MplTypeChecked ('SeqObjTag 'DataDefnTag)
                    ctersmap 
                        = foldl' g Map.empty 
                        $ NE.reverse  -- need to reverse for the order of the map
                        $ NE.zip cters patttails
                      where
                        g :: 
                            Map _ _ -> 
                            ( 
                                ( ( MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag)
                                  , MplType MplTypeChecked
                                  )
                                , IdentT
                                , [MplPattern (MplPass 'TypeChecked)]
                                )
                            , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)) -> 
                            Map _ _
                        g acc ((phrase, identt, cterpatts), patttail) = 
                            let patttail' = patttail & _1 %~ (cterpatts<>)
                            in acc & at identt %~ Just . maybe ( patttail' :| [] ) (NE.cons patttail')

            {- There's some somewhat complicated interactions going on here (this simlarly occurs with the tuple).. 
             - Since we did not choose to group based on destructors, we know that when 
             - we actually do get to a destructor case, this should:
             -          - be of length exactly 1 (otherwise we would go to the catch all case 
             -              of mixed patterns and partition based on the groups)
             -}
            dstersrule :: _ (Maybe (MplExpr MplPatternCompiled))
            dstersrule = sequenceA $ match patts <&> f
              where
                match :: 
                    [([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)] -> 
                    Maybe (((Location, MplType MplTypeChecked), NonEmpty
                      (MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag), IdentT,
                       MplPattern (MplPass 'TypeChecked))), ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled))
                match [(fstpatt:remainingpatts, resexpr)] = (,(remainingpatts, resexpr)) <$> fstpatt ^? _PRecord
                match _ = Nothing

                f :: 
                    ( ( (Location, XMplType MplTypeChecked)
                      , NonEmpty
                            ( MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag)
                            , IdentT
                            , MplPattern (MplPass 'TypeChecked)
                            )
                      )
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    _ (MplExpr MplPatternCompiled)
                f (((_loc, _clausetp), recordphrases), (remainingpatts, resexpr)) = 
                    go 
                        ( NE.toList pattsubs ++ us)
                        [ ( NE.toList pattphrases ++ remainingpatts, resexpr) ]
                        mexpr
                  where
                    pattsubs = recordphrases & mapped %~ RecordSub u . (view _1 &&& view _2)
                    pattphrases = recordphrases & mapped %~ view _3

            tuplerule :: _ (Maybe (MplExpr MplPatternCompiled))
            tuplerule = sequenceA $ match patts <&> f
              where
                match [(fstpatt:remainingpatts, resexpr)] = (,(remainingpatts, resexpr)) <$> fstpatt ^? _PTuple
                match _ = Nothing

                f :: 
                    ( 
                        ( (Location, XMplType MplTypeChecked)
                        , 
                            ( MplPattern (MplPass 'TypeChecked)
                            , MplPattern (MplPass 'TypeChecked)
                            , [MplPattern (MplPass 'TypeChecked)]
                            )
                        )
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> _ (MplExpr MplPatternCompiled)
                f (((_loc, TypeBuiltIn (TypeTupleF _ (ty0, ty1, tys)))
                    , (patt0, patt1, patts))
                    , (remainingpatts, remexpr)) = 
                        go (tuplesubs ++ us) [(tuplespatts ++ remainingpatts, remexpr)] mexpr
                  where
                    tuplesubs = zipWith (curry (TupleSub u)) (ty0:ty1:tys) [0..]
                    tuplespatts = patt0 : patt1 : patts

                -- TOOD change this to an exception later
                f _ = error "Error in compilation of pattern matching -- tuple is not a tuple type"

            {- constant rules. TODO: in the future, since the strucutre is the same for all three of these,
             - modify the AST so that it is just ONE constructor for ALL built in types like this. 
             -}
            intrule :: _ (Maybe (MplExpr MplPatternCompiled))
            intrule = sequenceA $ traverse (preview (_1 % _PInt)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Int) -> _ (MplExpr MplPatternCompiled)
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Int)
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints 
                        ccond = 
                            _EPOps #
                                ( _TypeBoolF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EInt # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 

            {- duplicted code from intrule for bool and char-}
            charrule :: _ (Maybe (MplExpr MplPatternCompiled))
            charrule = sequenceA $ traverse (preview (_1 % _PChar)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Char) -> _ (MplExpr MplPatternCompiled)
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Char)
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( _TypeCharF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EChar # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 

            boolrule :: _ (Maybe (MplExpr MplPatternCompiled))
            boolrule = sequenceA $ traverse (preview (_1 % _PBool)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Bool) -> _ (MplExpr MplPatternCompiled)
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Bool)
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( _TypeBoolF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EBool # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 
                
        _ -> foldrM (go (u:us)) mexpr $ fmap NE.toList groupedpatts 
      where
        ty :: XMplType MplTypeChecked
        ty = getPattType $ fst $ NE.head pattheads

        -- Note: this is quite confusing that both 'pattheads' and 'pattails' both include the same expression.. honestly this was kind of a bad decision in the beginning and makes the code a bit more confusing than it should be.
        pattheads :: NonEmpty (MplPattern MplTypeChecked, MplExpr MplPatternCompiled)
        patttails :: NonEmpty ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
        (pattheads, patttails) = NE.unzip 
            $ fmap (\(~(p:ps), expr) -> ((p, expr), (ps, expr))) 
            $ NE.fromList patts

        {-
         - groups the patterns based on their "type" i.e., put them in groups of if they are
         -      - variables
         -      - constructors
         -      - records
         -      - tuples
         -}
        groupedpatts :: NonEmpty (NonEmpty ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled))
        groupedpatts =  NE.groupBy1 p $ NE.fromList patts
          where
            p :: 
                ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled) -> 
                ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled) -> 
                Bool
            p a b =  let p' predicate = ((&&) `on`  predicate . head . fst) a b in or 
                [ p' (\n -> has _PVar n || has _PNull n) 
                , p' (has _PConstructor)

                -- built in primitive types
                , p' (has _PInt)
                , p' (has _PChar)
                , p' (has _PBool)

                , p' (\n -> has _PList n || has _PListCons n)
                , p' (has _PUnit) 

                -- , p' (has _PRecord)
                -- , p' (has _PTuple)
                ]



{- Essentially the same duplciated code from above.. this is a bit unfortunate from the way the AST is structured-}
patternCompileConcPatPhrases ::
    forall s m .
    ( MonadState s m
    , HasUniqueSupply s ) =>
    (NonEmpty ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))) -> 
    m ([MplPattern MplPatternCompiled], NonEmpty (MplCmd MplPatternCompiled))
patternCompileConcPatPhrases pattphrases = 
    -- assert that all the patterns have the same length
    assert ((length $ NE.group $ fmap (length . fst) pattphrases) == 1) $ do
        -- get the number of patterns
        let numpats = length $ fst $ NE.head pattphrases 
        -- then, we want to replace all the patterns with these unique pattern variables
        us <- replicateM numpats freshUIdP 
        -- indeed, we maintain the type information
        let usandtp = zipWith (curry (second getPattType)) us (fst . NE.head $ pattphrases) 
            (patts :: [MplPattern MplPatternCompiled]) = fmap (review _PVar . swap) usandtp

        pattexpr <- go (map VarSub usandtp) (NE.toList pattphrases) (CIllegalInstr () :| []) 

        return $ (patts, pattexpr)  
  where
    -- the type of the codomain 
    go :: 
        [Substitutable]  -> 
        [([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))] -> 
        NonEmpty (MplCmd MplPatternCompiled) -> 
        _ (NonEmpty (MplCmd MplPatternCompiled))
    go [] patts mexpr 
        | null patts = pure $ mexpr
        | otherwise = pure $ snd $ head patts 
    go (u:us) patts mexpr = case groupedpatts of
        -- the case when all patts are of the same type
        _ :| [] -> fmap (fromJust . asum) 
            $ sequenceA 
                [ varrule
                , ctersrule
                , dstersrule
                , tuplerule
                , intrule
                , boolrule
                , charrule
                , listrule
                , unitrule
                ]
          where
            varrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            varrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                match :: MplPattern MplTypeChecked -> Maybe (Either () IdentT)
                match patt
                    | Just identt <- patt ^? _PVar % _2 = _Just % _Right # identt
                    | Just _ <- patt ^? _PNull = _Just % _Left # ()
                    | otherwise = Nothing

                f :: NonEmpty (Either () IdentT) -> m (NonEmpty (MplCmd MplPatternCompiled))
                f vars = go 
                    us 
                    (NE.toList $ NE.zipWith 
                        h
                        vars 
                        patttails
                        ) 
                    mexpr

                {- Either is
                 -      Left: a null pattern @_@
                 -      Right: a var pattern @u@
                 -}
                h :: Either () IdentT -> 
                    ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)) -> 
                    ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                h (Right v) pattsexpr = pattsexpr & _2 % mapped %~ substitute (v,u)
                h _ pattsexpr = pattsexpr 

            -- rule for a built in list.. (essentially 'ctersrule' but hardcoded for lists)
            listrule  :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            listrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                -- Left is PListCons
                -- Right is Either PList or PString
                match :: MplPattern MplTypeChecked -> Maybe 
                    (Either 
                        ((Location, MplType MplTypeChecked), MplPattern MplTypeChecked, MplPattern MplTypeChecked)
                        (
                        Either
                            ((Location, MplType MplTypeChecked), [MplPattern MplTypeChecked])
                            ((Location, MplType MplTypeChecked), String)
                        )
                    )
                match = \case 
                    PListCons cxt a b -> Just $ Left (cxt,a,b)
                    PList cxt lst -> Just $ Right $ Left (cxt, lst)
                    PString cxt str -> Just $ Right $ Right (cxt, str)
                    _ -> Nothing

                f :: NonEmpty _ -> m (NonEmpty (MplCmd MplPatternCompiled))
                f cters = fmap (pure . CCase () (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [lstcons, lstnil]
                    -- [ lstcons $ mapMaybe g0 $ NE.toList cters , lstnil  $ mapMaybe g1 $ NE.toList cters ]
                  where 
                    (consacc, nilacc) = foldl' g mempty $ reverse $ zip (NE.toList cters) (NE.toList patttails)
                      where
                        g acc (lstpatt, patttail) = case lstpatt of
                            Left (ann, l, r) -> first ((patttail & _1 %~ ([l, r]<>)):) acc
                            -- regular list
                            Right (Left (ann, l:r)) -> first ((patttail & _1 %~ ([l, PList ann r]<>)):) acc
                            Right (Left (ann, [])) -> second (patttail:) acc 
                            -- string
                            Right (Right (ann, [])) -> second (patttail:) acc 
                            Right (Right (ann, l:r)) -> 
                                first ((patttail & _1 %~ ([PChar ann l, PList ann $ map (PChar ann) r]<>)):) acc

                    -- get the type of the list.. this is a bit of a mess just by design of the 
                    -- language
                    ~tplst@(TypeBuiltIn (TypeListF _ tphead)) = getPattType $ fst $ NE.head pattheads 

                    lstcons = do
                        u0 <- freshUIdP
                        u1 <- freshUIdP

                        let patt' =  PSimpleListCons tplst u0 u1

                        if null consacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go (map VarSub [(u0, tphead), (u1, tplst)] ++ us) consacc mexpr

                    lstnil = do
                        let patt' =  PSimpleListEmpty tplst 

                        if null nilacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go us nilacc mexpr



            -- hard coded case for the unit rule
            unitrule  :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            unitrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                match = preview _PSimpleUnit 

                ty = getPattType $ fst $ NE.head pattheads 

                f :: NonEmpty (XPSimpleUnit (MplPass 'TypeChecked)) -> m (NonEmpty (MplCmd MplPatternCompiled))
                f cters = fmap (pure . CCase () (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [unitpat]
                  where
                    unitpat = do
                        let patt' = PSimpleUnit ty
                        fmap (patt',) $ go us (NE.toList patttails) mexpr
                        

            ctersrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            ctersrule = sequenceA $ match pattheads <&> f
              where
                match :: 
                    NonEmpty (MplPattern MplTypeChecked, NonEmpty (MplCmd MplPatternCompiled)) -> 
                    Maybe 
                        ( NonEmpty 
                            ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), MplType MplTypeChecked)
                            , IdentT
                            , [MplPattern (MplPass 'TypeChecked)])
                        )
                match = traverse (preview (_1 % _PConstructor))

                f :: NonEmpty 
                    ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), XMplType MplTypeChecked)
                    , IdentT
                    , [MplPattern (MplPass 'TypeChecked)]) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f cters = fmap (pure . CCase () (getExprFromSubstitutable u) . NE.fromList) 
                    $ for (tpclause ^. typeClausePhrases) $ \phrase -> do
                        usandtps <- traverse (sequenceOf _1 . (freshUIdP,)) 
                            $ phrase ^. typePhraseFrom
                        -- let patt' = PSimpleConstructor (phrase, snd nu) (phrase ^. typePhraseName) $ usandtps
                        let patt' = PSimpleConstructor 
                                ( phrase
                                , getDataPhraseTypeResult phrase
                                ) 
                                (phrase ^. typePhraseName) 
                                $ usandtps
                        case ctersmap ^. at (phrase ^. typePhraseName) of
                            Just cternpatts -> (patt',) <$> 
                                go (map VarSub usandtps ++ us) (NE.toList cternpatts) mexpr
                            Nothing -> pure $ (patt',) mexpr
                    
                  where
                    tpclause = NE.head cters ^. _1 % _1 % typePhraseExt :: MplTypeClause MplTypeChecked ('SeqObjTag 'DataDefnTag)
                    ctersmap 
                        = foldl' g Map.empty 
                        $ NE.reverse  -- need to reverse for the order of the map
                        $ NE.zip cters patttails
                      where
                        g :: 
                            Map _ _ -> 
                            ( 
                                ( ( MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag)
                                  , MplType MplTypeChecked
                                  )
                                , IdentT
                                , [MplPattern (MplPass 'TypeChecked)]
                                )
                            , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))) -> 
                            Map _ _
                        g acc ((phrase, identt, cterpatts), patttail) = 
                            let patttail' = patttail & _1 %~ (cterpatts<>)
                            in acc & at identt %~ Just . maybe ( patttail' :| [] ) (NE.cons patttail')



            {- There's some somewhat complicated interactions going on here (this simlarly occurs with the tuple).. 
             - Since we did not choose to group based on destructors, we know that when 
             - we actually do get to a destructor case, this should:
             -          - be of length exactly 1 (otherwise we would go to the catch all case 
             -              of mixed patterns and partition based on the groups)
             -}
            dstersrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            dstersrule = sequenceA $ match patts <&> f
              where
                match :: 
                    [([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))] -> 
                    Maybe (((Location, MplType MplTypeChecked), NonEmpty
                      (MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag), IdentT,
                       MplPattern (MplPass 'TypeChecked))), ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)))
                match [(fstpatt:remainingpatts, resexpr)] = (,(remainingpatts, resexpr)) <$> fstpatt ^? _PRecord
                match _ = Nothing

                f :: 
                    ( ( (Location, XMplType MplTypeChecked)
                      , NonEmpty
                            ( MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag)
                            , IdentT
                            , MplPattern (MplPass 'TypeChecked)
                            )
                      )
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> 
                    _ (NonEmpty (MplCmd MplPatternCompiled))
                f (((_loc, _clausetp), recordphrases), (remainingpatts, resexpr)) = 
                    go 
                        ( NE.toList pattsubs ++ us)
                        [ ( NE.toList pattphrases ++ remainingpatts, resexpr) ]
                        mexpr
                  where
                    pattsubs = recordphrases & mapped %~ RecordSub u . (view _1 &&& view _2)
                    pattphrases = recordphrases & mapped %~ view _3

            tuplerule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            tuplerule = sequenceA $ match patts <&> f
              where
                match [(fstpatt:remainingpatts, resexpr)] = (,(remainingpatts, resexpr)) <$> fstpatt ^? _PTuple
                match _ = Nothing

                f :: 
                    ( 
                        ( (Location, XMplType MplTypeChecked)
                        , 
                            ( MplPattern (MplPass 'TypeChecked)
                            , MplPattern (MplPass 'TypeChecked)
                            , [MplPattern (MplPass 'TypeChecked)]
                            )
                        )
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f (((_loc, TypeBuiltIn (TypeTupleF _ (ty0, ty1, tys)))
                    , (patt0, patt1, patts))
                    , (remainingpatts, remexpr)) = 
                        go (tuplesubs ++ us) [(tuplespatts ++ remainingpatts, remexpr)] mexpr
                  where
                    tuplesubs = zipWith (curry (TupleSub u)) (ty0:ty1:tys) [0..]
                    tuplespatts = patt0 : patt1 : patts

                -- TOOD change this to an exception later
                f _ = error "Error in compilation of pattern matching -- tuple is not a tuple type"

            {- constant rules. TODO: in the future, since the strucutre is the same for all three of these,
             - modify the AST so that it is just ONE constructor for ALL built in types like this. 
             -}
            intrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            intrule = sequenceA $ traverse (preview (_1 % _PInt)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Int) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Int)
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> 
                    NonEmpty (MplCmd MplPatternCompiled) -> 
                    _ (NonEmpty (MplCmd MplPatternCompiled))
                g ints accexpr = do 
                    let hints = NE.head ints 
                        ccond = 
                            _EPOps #
                                ( _TypeBoolF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EInt # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ pure $ CIf () ccond thenc accexpr 

            {- duplicted code from intrule for bool and char-}
            charrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            charrule = sequenceA $ traverse (preview (_1 % _PChar)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Char) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Char)
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> 
                    NonEmpty (MplCmd MplPatternCompiled) -> 
                    _ (NonEmpty (MplCmd MplPatternCompiled))
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( _TypeCharF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EChar # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ pure $ CIf () ccond thenc accexpr 

            boolrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            boolrule = sequenceA $ traverse (preview (_1 % _PBool)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Bool) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Bool)
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> 
                    NonEmpty (MplCmd MplPatternCompiled) -> 
                    _ (NonEmpty (MplCmd MplPatternCompiled))
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( _TypeBoolF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EBool # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ pure $ CIf () ccond thenc accexpr 
                
        _ -> foldrM (go (u:us)) mexpr $ fmap NE.toList groupedpatts 
      where
        -- Note: this is quite confusing that both 'pattheads' and 'pattails' both include the same expression.. honestly this was kind of a bad decision in the beginning and makes the code a bit more confusing than it should be.
        pattheads :: NonEmpty (MplPattern MplTypeChecked, NonEmpty (MplCmd MplPatternCompiled))
        patttails :: NonEmpty ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
        (pattheads, patttails) = NE.unzip 
            $ fmap (\(~(p:ps), expr) -> ((p, expr), (ps, expr))) 
            $ NE.fromList patts

        {-
         - groups the patterns based on their "type" i.e., put them in groups of if they are
         -      - variables
         -      - constructors
         -      - records
         -      - tuples
         -}
        groupedpatts :: NonEmpty (NonEmpty ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)))
        groupedpatts =  NE.groupBy1 p $ NE.fromList patts
          where
            p :: 
                ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)) -> 
                ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)) -> 
                Bool
            p a b =  let p' predicate = ((&&) `on`  predicate . head . fst) a b in or 
                [ p' (\n -> has _PVar n || has _PNull n) 
                , p' (has _PConstructor)

                -- built in primitive types
                , p' (has _PInt)
                , p' (has _PChar)
                , p' (has _PBool)

                , p' (\n -> has _PList n || has _PListCons n)
                , p' (has _PUnit) 

                -- , p' (has _PRecord)
                -- , p' (has _PTuple)
                ]

