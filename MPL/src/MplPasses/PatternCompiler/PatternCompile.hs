{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Monad.State
import Control.Arrow
import Control.Applicative.Lift
import Data.Functor.Compose
import Data.Maybe
import Data.List

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
runPatternCompile (MplProg prog) = fmap runErrors $ getCompose go
  where
    go :: Compose m (Errors [err]) (MplProg MplPatternCompiled)
    go = fmap MplProg $ traverse 
        ( Compose
        . fmap eitherToErrors 
        . runExceptT 
        . patternCompileStmt )  
        prog

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
patternCompileDefn (ObjectDefn obj) = return $ ObjectDefn undefined
patternCompileDefn (FunctionDefn fun) = fmap FunctionDefn $ patternCompileFunDefn fun

-- | patternCompileExpr. As input,this expects the "case expression" as a result of compilation of pattern matching,
-- but this will then recurse so that the let bindings are correctly processed... Moreover, this drops additional
-- annotatoin information of position (it's not really needed anymroe -- anything after this i.e., lambda lifting is
-- just a program transformation and there are no error messages)
patternCompileExpr ::
    PatternCompile (MplExpr MplTypeChecked) (MplExpr MplPatternCompiled)
patternCompileExpr = cata go
  where
    go :: MplExprF (MplPass 'TypeChecked) (_ (MplExpr MplPatternCompiled)) -> _ (MplExpr MplPatternCompiled)
    go = \case
        EPOpsF ann primop l r -> EPOps ann primop <$> l <*> r
        EVarF ann ident -> pure $ EVar ann ident
        EIntF ann n -> pure $ EInt (snd ann) n
        ECharF ann v -> pure $ EChar (snd ann) v
        EDoubleF ann v -> pure $ EDouble (snd ann) v
        ECaseF ann expr cases -> ECase ann <$> expr <*> traverse sequenceA cases
        EObjCallF ann ident exprs -> EObjCall ann ident <$> sequenceA exprs
        ERecordF ann phrases -> error "pattern compiling for records needs a little more thought"
        -- ERecordF ann phrases -> ERecord (snd ann) <$> _ phrases
        ECallF ann ident exprs -> ECall ann ident <$> sequenceA exprs 
        EListF ann exprs -> EList (snd ann) <$> sequenceA exprs
        EStringF ann str -> pure $ EString (snd ann) str
        EUnitF ann -> pure $ EUnit $ snd ann
        ETupleF ann (mt0,mt1,mts) -> fmap (ETuple (snd ann)) $ (,,) <$> mt0 <*> mt1 <*> sequenceA mts
        EBuiltInOpF ann op l r -> EBuiltInOp (snd ann) op <$> l <*> r
        EIfF ann iff thenf elsef -> EIf ann <$> iff <*> thenf <*> elsef

        ELetF ann lets expr -> ELet ann <$> (traverse patternCompileStmt lets) <*> expr

        {-
        EFoldF ann foldon phrases -> EFold ann foldon phrases
        EUnfoldF ann expr phrases -> EUnfold ann expr phrases
        ESwitchF ann res -> ESwitch ann res
        XExprF ann -> XExpr ann
        -}

patternCompileFunDefn :: 
    PatternCompile (XFunctionDefn MplTypeChecked) (XFunctionDefn MplPatternCompiled)
patternCompileFunDefn (MplFunction funName funTp funDefn) = do
    (patts, pattexpr) <- patternCompilePattPhrases (MorphismIdent funName, funDefn)
    pattexpr' <- patternCompileExpr pattexpr
    return $ MplFunction funName funTp $ (patts, pattexpr')  :| []
{-
    -- assert that all the patterns have the same length
    assert ((length $ NE.group $ fmap length funDefn) == 1) $ do
        -- get the number of patterns
        let numpats = length $ fst $ NE.head funDefn 
        -- then, we want to replace all the patterns with these unique pattern variables
        us <- replicateM numpats freshIdP 
        -- indeed, we maintain the type information
        let usandtp = zipWith (curry (second getPattType)) us (fst . NE.head $ funDefn) 
            (patts :: [MplPattern MplPatternCompiled]) = fmap (review _PVar . swap) usandtp
        pattexpr <- _go usandtp funDefn Nothing >>= 
        return $ MplFunction funName funTp $ (patts, pattexpr)  :| []
-}

patternCompilePattPhrases  ::
    PatternCompile 
        (IdentPattern, NonEmpty ([MplPattern MplTypeChecked], MplExpr MplTypeChecked)) 
        ([MplPattern MplPatternCompiled], MplExpr MplTypeChecked)
patternCompilePattPhrases (identpatt, pattphrases) = 
    -- assert that all the patterns have the same length
    assert ((length $ NE.group $ fmap (length . fst) pattphrases) == 1) $ do
        -- get the number of patterns
        let numpats = length $ fst $ NE.head pattphrases 
        -- then, we want to replace all the patterns with these unique pattern variables
        us <- replicateM numpats freshIdP 
        -- indeed, we maintain the type information
        let usandtp = zipWith (curry (second getPattType)) us (fst . NE.head $ pattphrases) 
            (patts :: [MplPattern MplPatternCompiled]) = fmap (review _PVar . swap) usandtp

        pattexpr <- go usandtp (NE.toList pattphrases) Nothing 

        return $ (patts, pattexpr)  
  where
    -- the type of the codomain 
    restp :: MplSeqType MplTypeChecked
    restp = getExprType $ snd $ NE.head pattphrases

    go :: 
        [(IdentT, MplSeqType MplTypeChecked)]  -> 
        [([MplPattern MplTypeChecked], MplExpr MplTypeChecked)] -> 
        Maybe (MplExpr MplTypeChecked) -> 
        _ (MplExpr MplTypeChecked)
    go [] patts mexpr 
        | null patts = error "non exhaustive pattern"
        | otherwise = pure $ snd $ head patts 
    go (u:us) patts mexpr = case groupedpatts of
        -- | the case when all patts are of the same type
        _ :| [] -> fmap (fromJust . asum) (sequenceA [varrule, ctersrule]) 
          where
            varrule :: _ (Maybe (MplExpr MplTypeChecked))
            varrule = sequenceA $ traverse (preview (_1 %_PVar % _2)) pattheads <&> f
              where
                f :: NonEmpty IdentT -> _ (MplExpr MplTypeChecked)
                f vars = go 
                    us 
                    (NE.toList $ NE.zipWith (\v (vs, expr) -> (vs, substituteVarIdent (v,fst u) expr)) vars patttails) 
                    mexpr

            ctersrule :: _ (Maybe (MplExpr MplTypeChecked))
            ctersrule = sequenceA $ traverse (preview (_1 % _PConstructor)) pattheads <&> f
              where
                f :: NonEmpty ((MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), MplSeqType MplTypeChecked), IdentT, [MplPattern (MplPass 'TypeChecked)]) -> 
                     _ (MplExpr MplTypeChecked)
                f cters = fmap (ECase restp (uncurry EVar $ swap u) . NE.fromList) 
                    $ for (tpclause ^. typeClausePhrases) $ \phrase -> do
                        usandtps <- traverse (sequenceOf _1 . second ([],[],) . (freshIdP,)) $ phrase ^. typePhraseFrom
                        let patt' = PConstructor (phrase, snd u) (phrase ^. typePhraseName) $ map (uncurry PVar . swap) usandtps

                        {-
                        traceM 
                            $ (++"AA") 
                            $ intercalate "\n"
                            $ map (\(a,b) -> a ++ "->" ++ b)
                            $ (map (first (intercalate " ")))
                            $ fmap (map (pprint (Proxy :: Proxy MplRenamed)) *** (pprint (Proxy :: Proxy MplRenamed))) 
                            $ NE.toList cternpatts
                        -}
                        case ctersmap ^. at (phrase ^. typePhraseName) of
                            Just cternpatts -> (patt',) <$> go (usandtps ++ us) (NE.toList cternpatts) mexpr
                            Nothing -> case mexpr of
                                Just expr -> pure $ (patt',) expr
                                Nothing -> error "non exhaustive pattern"
                    
                  where
                    tpclause = NE.head cters ^. _1 % _1 % typePhraseExt :: MplTypeClause MplTypeChecked ('SeqObjTag 'DataDefnTag)
                    ctersmap = foldl' g Map.empty $ NE.reverse $ NE.zip cters patttails
                      where
                        g :: 
                            Map _ _ -> 
                            ( 
                                ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), ([TypeT], [MplType MplTypeChecked]
                                , MplType MplTypeChecked)), IdentT, [MplPattern (MplPass 'TypeChecked)]
                                )
                            , ([MplPattern MplTypeChecked], MplExpr MplTypeChecked)) -> 
                            Map _ _
                        g acc ((phrase, identt, cterpatts), patttail) = 
                            -- let patttail' = patttail & _1 %~ (trace ( (++"AA") $ intercalate "\n" $ map (pprint (Proxy :: Proxy MplRenamed)) cterpatts) cterpatts <>)
                            let patttail' = patttail & _1 %~ (cterpatts<>)
                            in acc & at identt %~ Just . maybe ( patttail' :| [] ) (NE.cons patttail')

            dstersrule :: _ (Maybe (MplExpr MplTypeChecked))
            dstersrule = undefined

        _ -> fmap fromJust $ foldrM (((.) . (.)) (fmap Just) (go us)) mexpr $ fmap NE.toList groupedpatts 

        -- _ -> _ $ foldrM (go us) (fmap ([],) mexpr) groupedpatts
      where
        ty :: MplSeqType MplTypeChecked
        ty = getPattType $ fst $ NE.head pattheads

        pattheads :: NonEmpty (MplPattern MplTypeChecked, MplExpr MplTypeChecked)
        patttails :: NonEmpty ([MplPattern MplTypeChecked], MplExpr MplTypeChecked)
        (pattheads, patttails) = NE.unzip $ fmap (\(~(p:ps), expr) -> ((p, expr), (ps, expr))) $ NE.fromList patts

        {-
         - groups the patterns based on their "type" i.e., put them in groups of if they are
         -      - variables
         -      - constructors
         -      - records
         -      - tuples
         -}
        groupedpatts :: NonEmpty (NonEmpty ([MplPattern MplTypeChecked], MplExpr MplTypeChecked))
        groupedpatts =  NE.groupBy1 p $ NE.fromList patts
          where
            p :: ([MplPattern MplTypeChecked], MplExpr MplTypeChecked) -> ([MplPattern MplTypeChecked], MplExpr MplTypeChecked) -> Bool
            p a b =  let p' predicate = ((&&) `on`  predicate . head . fst) a b in or 
                [ p' (has _PVar)
                , p' (has _PConstructor)
                , p' (has _PRecord)
                , p' (has _PTuple)
                ]

            

