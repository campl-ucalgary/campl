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

import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow
import Control.Applicative.Lift
import Data.Functor.Compose
import Data.Maybe
import Data.List
import Data.Monoid

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
            else return $ Left lg
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
        -- ECaseF ann expr cases -> ECase ann <$> expr <*> traverse sequenceA cases
        ECaseF ann expr cases -> do
            expr' <- expr
            cases' <- sequenceOf (traversed % _2) cases
            let caseslst' = fmap (over _1 pure) cases'
                exhstchk = patternCompileExhaustiveCheck $ fmap fst caseslst'
            tell $ bool [_NonExhaustiveCasePatt # ()] [] exhstchk

            ~([PVar _ u], nbdy) <- patternCompileSeqPatPhrases $ caseslst'
            return $ substituteVarIdentByExpr (u, expr') nbdy

        EObjCallF ann ident exprs -> EObjCall ann ident <$> sequenceA exprs
        ERecordF ann phrases -> do
            phrases' <- for phrases $ \(phrase, identt, (patts, mexpr)) -> do
                -- let exhstchk = null patts || patternCompileExhaustiveCheck (patts :| [])
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

        ESwitchF ann switches -> 
            sequenceOf (traversed % each) switches 
                >>= patternCompileExhaustiveESwitchCheck 
                >>= return . foldr f (EIllegalInstr ())
          where
            f :: 
                (MplExpr MplPatternCompiled, MplExpr MplPatternCompiled) -> 
                MplExpr MplPatternCompiled -> 
                MplExpr MplPatternCompiled
            f (bexpr, thenc) acceq = EIf (getExprType thenc) bexpr thenc acceq
            
        ELetF ann lets expr -> ELet ann <$> (traverse patternCompileStmt lets) <*> expr

        {-
        EFoldF ann foldon phrases -> EFold ann foldon phrases
        EUnfoldF ann expr phrases -> EUnfold ann expr phrases
        ESwitchF ann res -> ESwitch ann res
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
    ncmds <- traverseOf (traversed % _2) patternCompileCmds procDefn
    undefined


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
        CGet ann patt chp -> error "do @get@ later" 
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
            (CRace ann races':) <$> go cmds

        -- CPlug !(XCPlug x) (CPlugPhrase x, CPlugPhrase x)

        CPlugs ann (phrase0, phrase1, phrases) -> do
            ~(phrase0':phrase1':phrases') <- traverseOf (traversed % _3) patternCompileCmds $ phrase0:phrase1:phrases
            -- _ :: ((), ([ChIdentT], [ChIdentT]), NonEmpty (MplCmd MplTypeChecked))
            (CPlugs ann (phrase0', phrase1', phrases'):) <$>  go cmds
        CCase ann expr pattscmds -> do
            pattscmds' <- traverseOf (traversed % _2) patternCompileCmds pattscmds

            undefined

{-
  | CCase !(XCCase x)
          (XMplExpr x)
          (NonEmpty (XMplPattern x, NonEmpty (MplCmd x)))
  | CSwitch !(XCSwitch x)
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
            [ has (folded % _PRecord) patts
            ]

        -- we can actually exhaust all the bool types 
        -- (unlike Chars and Ints.. we just assume it is impossible to exhaust them completely)
        , and 
            [ getAll $ foldMapOf (folded % _PBool % _2) All patts
            , getAll $ foldMapOf (folded % _PBool % _2) (All . not) patts
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


{- | Pattern compiles a pattern phrase i.e., this will pattern compile something like
@
    SomePattern(a,b,MorePatterns(c)), a,b -> <final result expression>
    f,a,b -> <final result expression>
    ....
@
to replace the patterns with variables and translate them into cases in the final expression.

TODO: still need tuples
-}
patternCompileSeqPatPhrases  ::
    PatternCompile 
        (NonEmpty ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)) 
        ([MplPattern MplPatternCompiled], MplExpr MplPatternCompiled)
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

        pattexpr <- go (map VarSub usandtp) (NE.toList pattphrases) (EIllegalInstr ()) 

        return $ (patts, pattexpr)  
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
                {-
                [ varrule
                , ctersrule
                , dstersrule
                , tuplerule
                , intrule
                , boolrule
                , charrule
                ]
                -}
                [ varrule
                , ctersrule
                , dstersrule
                ]
          where
            varrule :: _ (Maybe (MplExpr MplPatternCompiled))
            -- varrule = sequenceA $ traverse ( undefined +++ preview (_1 %_PVar % _2)) pattheads <&> g
            varrule = sequenceA $ traverse (f . fst) pattheads <&> g
              where
                f :: MplPattern MplTypeChecked -> Maybe (Either () IdentT)
                f patt
                    | Just identt <- patt ^? _PVar % _2 = _Just % _Right # identt
                    | Just _ <- patt ^? _PNull = _Just % _Left # ()
                    | otherwise = Nothing

                g :: NonEmpty (Either () IdentT) -> _ (MplExpr MplPatternCompiled)
                g vars = go 
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
                match =  traverse (preview (_1 % _PConstructor))

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

            {- There's some somewhat complicated interactions going on here.. 
             - Since we did not choose to group based on destructors, we know that when 
             - we actually do get to a destructor case, this should:
             -          - be of length exactly 1 (otherwise we would go to the catch all case 
             -              of mixed patterns and partition based on the groups)
             -}
            dstersrule :: _ (Maybe (MplExpr MplPatternCompiled))
            dstersrule = sequenceA $ match patts <&> f
              where
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
            {-
            -- TODO: Put this in.. need to add a special projection operator in the AST as well.
            tuplerule :: _ (Maybe (MplExpr MplPatternCompiled))
            tuplerule = sequenceA $ traverse (preview (_1 % _PTuple)) pattheads <&> f
              where
                f :: NonEmpty 
                    ( (Location, XMplType MplTypeChecked)
                    , 
                        ( MplPattern (MplPass 'TypeChecked)
                        , MplPattern (MplPass 'TypeChecked)
                        , [MplPattern (MplPass 'TypeChecked)]
                        )
                    ) -> 
                    _ (MplExpr MplPatternCompiled)
                f = error "tuples not implemented yet"

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
                    (
                        ( ( Location , ([TypeT], [MplType MplTypeChecked], MplType MplTypeChecked))
                        , Int
                        )
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( ([], [], _TypeBoolF # Nothing)
                                , PrimitiveEq
                                , _EVar # swap u
                                , _EInt # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 

            -- duplicated code with the int case essentially
            charrule :: _ (Maybe (MplExpr MplPatternCompiled))
            charrule = sequenceA $ traverse (preview (_1 % _PChar)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Char) -> _ (MplExpr MplPatternCompiled)
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty
                    (
                        ( ( Location , ([TypeT], [MplType MplTypeChecked], MplType MplTypeChecked))
                        , Char
                        )
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( ([], [], _TypeBoolF # Nothing)
                                , PrimitiveEq
                                , _EVar # swap u
                                , _EChar # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 

            -- duplicated code with the int case essentially
            boolrule :: _ (Maybe (MplExpr MplPatternCompiled))
            boolrule = sequenceA $ traverse (preview (_1 % _PBool)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Bool) -> _ (MplExpr MplPatternCompiled)
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty
                    (
                        ( ( Location , ([TypeT], [MplType MplTypeChecked], MplType MplTypeChecked))
                        , Bool
                        )
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( ([], [], _TypeBoolF # Nothing)
                                , PrimitiveEq
                                , _EVar # swap u
                                , _EBool # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 
            -}
                
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
                [ p' (has _PVar) || p' (has _PNull)
                , p' (has _PConstructor)

                -- built in primitive types
                , p' (has _PInt)
                , p' (has _PChar)
                , p' (has _PBool)

                -- , p' (has _PRecord)
                -- , p' (has _PTuple)
                ]

            

