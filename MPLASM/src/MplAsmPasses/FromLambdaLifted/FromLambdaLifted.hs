{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module MplAsmPasses.FromLambdaLifted.FromLambdaLifted where

import Optics

-- Front end
import qualified MplPasses.Passes as Passes
import qualified MplPasses.Parser.BnfcParse as B
import qualified MplPasses.PassesErrors as PassesErrors
import qualified MplPasses.PassesErrorsPprint as PassesErrors

import MplAST.MplCore 
import MplAST.MplTypeChecked 
import MplUtil.UniqueSupply

-- Assembler
import qualified MplAsmAST.MplAsmProg as Asm
import qualified MplAsmAST.MplAsmCore as Asm
import qualified MplAsmPasses.PassesErrorsPprint as Asm

import MplAsmPasses.FromLambdaLifted.FromLambdaLiftedUtil
import MplAsmPasses.FromLambdaLifted.FromLambdaLiftedAST
import MplAsmPasses.FromLambdaLifted.FromLambdaLiftedErrors 
import MplAsmPasses.FromLambdaLifted.FromLambdaLiftedStack

-- Prelude
import Control.Arrow
import Data.Ord
import Data.List
import Data.Maybe
import Data.Coerce
import Data.Foldable
import Data.Traversable
import Data.Bool
import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Control.Exception
import Debug.Trace

-- libraries
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Base ( ListF (..), NonEmptyF (..) )
import Data.Functor.Foldable ( cata, para, Base )

-- | hard coded built in data
lIST_INTERNAL__, lIST_INTERNAL_CONS__,  lIST_INTERNAL_EMPTY__, uNIT_INTERNAL__ :: Asm.IdP MplAsmFromLambdaLifted
lIST_INTERNAL__ = coerce "LIST_INTERNAL__"
lIST_INTERNAL_CONS__ = coerce "LIST_INTERNAL_CONS__"
lIST_INTERNAL_EMPTY__ = coerce "LIST_INTERNAL_EMPTY__"
uNIT_INTERNAL__ = coerce "UNIT_INTERNAL__"

-- | this will assemble an entire program
mplAssembleProg ::
    forall e.
    AsFromLambdaLiftedError e => 
    UniqueSupply -> 
    MplProg MplLambdaLifted ->
    Either [e] (Asm.MplAsmProg MplAsmFromLambdaLifted)
-- we assert that there are nothing in the where bindings (which shouldn't be the case 
-- from lambda lifting) actually not too sure why I don't do this anymore? I mean I guess
-- technically where bindings are not liek let bindings in Haskell, so they could indeed
-- may stil be there (since where bindings are just a symobl table trick in this language)
{-
mplAssembleProg uniq prog = assert (all (null . view stmtWhereBindings) prog') 
    $ (\case 
            (asmprog, []) -> Right asmprog
            (_, errs) -> Left errs 
        )
-}
mplAssembleProg uniq prog = 
    (\case 
            (asmprog, []) -> Right asmprog
            (_, errs) -> Left errs 
        )
    $ runWriter 
    $ flip evalStateT s0
    $ mplasmprog
    -- fmap Asm.MplAsmProg (evalState (fmap concat $ traverse mplAssembleStmt stmts) s0) (fmap assemblemain maybemain)
  where 
    -- first, we move the where bindings just above the function.. this is so we don't get 
    -- weird where bindings overriding the bottom most main function.
    prog' :: [MplStmt MplLambdaLifted]
    prog' = map f (coerce prog :: [MplStmt MplLambdaLifted])
      where
        f :: MplStmt MplLambdaLifted -> MplStmt MplLambdaLifted
        f stmt = stmt
            & stmtWhereBindings .~ mempty
            & stmtDefns %~ NE.fromList . ((foldMapOf (stmtWhereBindings % folded) g stmt)++) . NE.toList

        g :: MplStmt MplLambdaLifted -> [MplDefn MplLambdaLifted]
        g stmt = NE.toList (stmt ^. stmtDefns) ++ foldMapOf (stmtWhereBindings % folded) g stmt

        {-
        (NE.fromList (foldMapOf (stmtDefns % folded) runLambdaLiftDefn stmt))
        (stmt ^..  stmtWhereBindings % folded % to runlambdaLiftStmt)
        -}

    mplasmprog :: 
        ( HasUniqueSupply s
        , MonadState s m 
        , MonadWriter [e] m ) =>
        m (Asm.MplAsmProg MplAsmFromLambdaLifted)
    mplasmprog = do
        {- Here, we include declarations to replace built in lists and unit type.
         - Note that it is impossible for the user to overwrite this type, since 
         - each tag coming in from the front end will have a unique tag preprended 
         - Moreover, the order is important in the abstract machine (empty should be the 
         - 0th constructor, and cons should be the 1st constructor) -}
        let builtintypes = Asm.Constructors 
                [ Asm.TypeAndSeqSpecs lIST_INTERNAL__
                    [ (lIST_INTERNAL_EMPTY__, 0)
                    , (lIST_INTERNAL_CONS__, 2)
                    ]
                , Asm.TypeAndSeqSpecs uNIT_INTERNAL__
                    [ (uNIT_INTERNAL__, 0) ]
                ]
            

        asmstmts <- fmap concat $ traverse mplAssembleStmt stmts
        asmmain <- case maybemain of
            Just mainf -> assert (null rst) $ fmap Just $ do 
                res <- mplAssembleProcDefn mainf
                return res 
              where
                ((seqs, ins, outs), _) :| rst = mainf ^. procDefn 
                
            Nothing -> return Nothing

        return $ Asm.MplAsmProg (builtintypes : asmstmts) asmmain

    -- get some fresh unique supplies
    ~(s0:_:_) = uniqueSupplies uniq

    -- we find the bottom most definition named @run@ as the main function 
    -- (honestly quite confusing how this is done, but it should hopefully work)
    -- I don't think this is totally correct being totally honest
    stmts :: [MplStmt MplLambdaLifted]
    maybemain :: Maybe (MplProcess MplLambdaLifted)
    (stmts, maybemain) = first reverse . para f . reverse $ prog'

    f :: ListF 
            (MplStmt MplLambdaLifted) 
            ([MplStmt MplLambdaLifted], ([MplStmt MplLambdaLifted], Maybe _)) -> 
        ([MplStmt MplLambdaLifted], Maybe _)
    f = \case
        Cons stmt (lst, (stmts, _)) -> case para g (stmt ^. stmtDefns % to NE.toList) of
            (notmains, mainf) -> if null notmains
                then (lst, mainf)
                else (MplStmt (NE.fromList notmains) [] : lst, mainf)
          where
            g :: ListF (MplDefn MplLambdaLifted) ([MplDefn MplLambdaLifted], ([MplDefn MplLambdaLifted], Maybe _)) -> 
                ([MplDefn MplLambdaLifted], Maybe _)
            g = \case 
                Cons defn (lst', _) 
                    | Just proc <- defn ^? _ProcessDefn -> if proc ^. procName % nameStr == "run"
                        then ( lst' , Just proc)
                        else (defn : lst', Nothing)
                    | otherwise -> (defn : lst', Nothing)
                Nil -> (mempty, mzero)
        Nil -> (mempty, mzero)

    {-
    assemblemain :: 
        (IdentT, ([IdentT], [ChIdentT], [ChIdentT]), NonEmpty (MplCmd MplLambdaLifted)) -> 
        (Asm.Name, ([Asm.Name], [Asm.Name], [Asm.Name]), Asm.MplAsmComs MplAsmFromLambdaLifted)
    assemblemain (mainname, (seqs, ins, outs), cmds) = 
        ( toAsmIdP mainname
        , 
            ( map toAsmIdP seqs
            , map toAsmIdP ins
            , map toAsmIdP outs
            )
        , evalState (mplAssembleCmds cmds) s1
        )
    -}


mplAssembleStmt ::
    forall s m e.
    ( HasUniqueSupply s
    , AsFromLambdaLiftedError e 
    , MonadState s m 
    , MonadWriter [e] m 
    ) =>
    MplStmt MplLambdaLifted ->
    m [Asm.MplAsmStmt MplAsmFromLambdaLifted]
mplAssembleStmt stmt = fmap (filter filtercond) $ sequenceA
    [ pure $ Asm.Constructors (concat $ mapMaybe constructorStmts collectedDefns)
    , pure $ Asm.Destructors (concat $ mapMaybe destructorStmts collectedDefns)
    , pure $ Asm.Protocols (concat $ mapMaybe protocolStmts collectedDefns)
    , pure $ Asm.Coprotocols (concat $ mapMaybe coprotocolStmts collectedDefns)
    , fmap Asm.Functions $ sequenceA $ mapMaybe funStmts collectedDefns
    , fmap Asm.Processes $ sequenceA $ mapMaybe procStmts collectedDefns
    ]
  where
    filtercond :: Asm.MplAsmStmt MplAsmFromLambdaLifted -> Bool
    filtercond = \case
        Asm.Constructors defns -> not (null defns)
        Asm.Destructors defns  -> not (null defns)
        Asm.Protocols defns    -> not (null defns)
        Asm.Coprotocols defns  -> not (null defns)
        Asm.Functions defns    -> not (null defns)
        Asm.Processes defns    -> not (null defns)

    collectedDefns :: [MplDefn MplLambdaLifted]
    collectedDefns = collectDefns stmt

    collectDefns :: MplStmt MplLambdaLifted -> [MplDefn MplLambdaLifted]
    collectDefns stmt = 
        stmt ^. stmtDefns % to NE.toList
        ++ foldMapOf (stmtWhereBindings % folded) collectDefns stmt

    {- translates everything to asembler -}
    constructorStmts :: MplDefn MplLambdaLifted -> Maybe [Asm.TypeAndSeqSpecs MplAsmFromLambdaLifted]
    constructorStmts = preview
        ( _ObjectDefn 
        % _SeqObjDefn
        % _DataDefn 
        % to (toListOf (typeClauseSpineClauses % folded % to f))
        )
      where
        f clause = Asm.TypeAndSeqSpecs 
            ( clause ^. typeClauseName % to toAsmIdP)
            ( clause ^..
                typeClausePhrases 
                % folded
                % to
                    ( 
                        view (typePhraseName % to toAsmIdP) 
                        &&& view (typePhraseFrom % to genericLength) 
                    ) 
            )
    destructorStmts :: MplDefn MplLambdaLifted -> Maybe [Asm.TypeAndSeqSpecs MplAsmFromLambdaLifted]
    destructorStmts = preview
        ( _ObjectDefn 
        % _SeqObjDefn
        % _CodataDefn 
        % to (toListOf (typeClauseSpineClauses % folded % to f))
        )
          where
            f clause = Asm.TypeAndSeqSpecs 
                ( clause ^. typeClauseName % to toAsmIdP)
                ( clause ^..
                    typeClausePhrases 
                    % folded
                    % to
                        ( 
                            view (typePhraseName % to toAsmIdP) 
                            -- recall that the type phrase from has type ([Types], Types)
                            &&& view (typePhraseFrom % _1 % to genericLength )
                        ) 
                )

    protocolStmts :: MplDefn MplLambdaLifted -> Maybe [Asm.TypeAndConcSpecs MplAsmFromLambdaLifted]
    protocolStmts = preview
        ( _ObjectDefn 
        % _ConcObjDefn
        % _ProtocolDefn 
        % to (toListOf (typeClauseSpineClauses % folded % to f))
        )
      where
            f clause = Asm.TypeAndConcSpecs 
                ( clause ^. typeClauseName % to toAsmIdP)
                ( clause ^..
                    typeClausePhrases 
                    % folded
                    % to (view (typePhraseName % to toAsmIdP)) 

                )
    coprotocolStmts :: MplDefn MplLambdaLifted -> Maybe [Asm.TypeAndConcSpecs MplAsmFromLambdaLifted]
    coprotocolStmts = preview
        ( _ObjectDefn 
        % _ConcObjDefn
        % _CoprotocolDefn 
        % to (toListOf (typeClauseSpineClauses % folded % to f))
        )
      where
            -- duplicated from protocol case
            f clause = Asm.TypeAndConcSpecs 
                ( clause ^. typeClauseName % to toAsmIdP)
                ( clause ^..
                    typeClausePhrases 
                    % folded
                    % to (view (typePhraseName % to toAsmIdP)) 
                )


    funStmts :: MplDefn MplLambdaLifted -> Maybe (m (Asm.Name, [Asm.Name], Asm.MplAsmComs MplAsmFromLambdaLifted))
    funStmts defn = case defn of
        -- assert is here since from compilation of pattern matching, there should only be one pattern.
        FunctionDefn fundefn -> assert (lengthOf (funDefn % folded) fundefn == 1) 
            $ Just
            $ fmap 
                (
                    ( fundefn ^. funName % to toAsmIdP
                    , fundefn ^.. funDefn % to NE.head % _1 % folded % to (\(PVar _ v) -> toAsmIdP v )
                    ,)
                .
                    (++ [Asm.CRet ()])
                )
            $ fundefn ^. funDefn % to NE.head % _2 % to mplAssembleExpr

        _ -> Nothing

    procStmts :: MplDefn MplLambdaLifted -> Maybe (m (Asm.Name, ([Asm.Name], [Asm.Name], [Asm.Name]), Asm.MplAsmComs MplAsmFromLambdaLifted))
    procStmts defn = case defn of
        ProcessDefn procdefn -> assert (lengthOf (procDefn % folded) procdefn == 1) $ Just $ mplAssembleProcDefn procdefn
        _ -> Nothing

{- | used immiedately above to convert a prcoess to the assembled form. The reason why this isn't in the above
where statement is because we need to use this for compiliing the main function as well. 
Services are a complete goddy mess in this language..
-}
mplAssembleProcDefn :: 
    forall s m e.
    ( HasUniqueSupply s
    , AsFromLambdaLiftedError e 
    , MonadState s m 
    , MonadWriter [e] m ) =>
    MplProcess MplLambdaLifted ->
    m (Asm.Name, ([Asm.Name], [Asm.Name], [Asm.Name]), Asm.MplAsmComs MplAsmFromLambdaLifted)
mplAssembleProcDefn procdefn = assert (lengthOf (procDefn % folded) procdefn == 1) $ do
    defn' <- procdefn ^. procDefn % to NE.head % _2 % to mplAssembleCmds 

    ins' <- for (procdefn ^.. procDefn % to NE.head % _1 % _2 % folded) $ \ch -> do
        return $ toAsmIdP ch

    {- here, we do the services.
     So this is really stupid for many reasons.. If it is of type IntTerm
    -}
    outs' <- for (procdefn ^.. procDefn % to NE.head % _1 % _3 % folded) $ \ch -> do
        return $ toAsmIdP ch

    return 
        ( procdefn ^. procName % to toAsmIdP
        , 
            ( procdefn ^.. procDefn % to NE.head % _1 % _1 % folded % to (\(PVar _ v) -> toAsmIdP v )
            , ins'
            , outs'
            )
        , defn' 
        )
        
mplAssembleExpr :: 
    forall s m e.
    ( HasUniqueSupply s,
    AsFromLambdaLiftedError e 
    , MonadState s m, MonadWriter [e] m) =>
    MplExpr MplLambdaLifted -> 
    m (Asm.MplAsmComs MplAsmFromLambdaLifted)
mplAssembleExpr = para go
  where
    go = \case
        -- do the colon first (since this is a constructor and not really a primitive operation
        EPOpsF _ PrimitiveColon (_, l) (_, r) -> do   
            el <- freshTmpName
            er <- freshTmpName
            l' <- fmap (++ [Asm.CStore () el]) l
            r' <- fmap (++ [Asm.CStore () er]) r
            return $ concat [l', r'] ++
                [ Asm.CConstructor () 
                    ( Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_CONS__ )
                    [el, er]
                ]

        EPOpsF ann op (lexpr, l) (_, r) -> fmap concat $ sequenceA [r, l, op']
          where
            op' = case op of
                PrimitiveAdd -> pure [Asm.CAddInt ()]
                PrimitiveMul -> pure [Asm.CMulInt ()]
                PrimitiveSub -> pure [Asm.CSubInt ()]
                PrimitiveDiv -> pure [Asm.CDivInt ()]
                PrimitiveLt -> pure [Asm.CLtInt ()]
                PrimitiveGt -> pure [Asm.CGtInt ()]
                PrimitiveLeq -> pure [Asm.CLeqInt ()]
                PrimitiveGeq -> pure [Asm.CGeqInt ()]
                PrimitiveEq -> case getExprType lexpr of 
                    TypeBuiltIn (TypeIntF _) -> pure [Asm.CEqInt ()]
                    TypeBuiltIn (TypeBoolF _) -> pure [Asm.CEqBool ()]
                    TypeBuiltIn (TypeCharF _) -> pure [Asm.CEqChar ()]
                    _ -> error "illegal use of eq instruction on unsupported type (TODO: make this error message better). "
                _ -> error $ "assembling of operation is not implemented yet " ++ show op
        
            
        EVarF _ idp -> pure [Asm.CLoad () (toAsmIdP idp)]
        EIntF _ n -> pure [Asm.CInt () n]

        ECharF _ c -> pure [Asm.CChar () c]
        EBoolF _ b -> pure [Asm.CBool () b]
        ECaseF _ (_, caseon) (fmap (second snd) -> cases) -> do
            e <- freshTmpName
            caseon' <- caseon
            cases' <- traverse f cases
            return $ 
                caseon' <>
                [ Asm.CStore () e
                , Asm.CCase () e (NE.toList cases')
                ]
          where
            f :: (XECasePattern (MplPass 'LambdaLifted), m [Asm.MplAsmCom MplAsmFromLambdaLifted]) -> 
                m (Asm.TypeAndSpec MplAsmFromLambdaLifted, [Asm.IdP MplAsmFromLambdaLifted], Asm.MplAsmComs MplAsmFromLambdaLifted)
            f = \case
                (PSimpleConstructor ann phr args, coms) ->   
                    fmap 
                        ( Asm.TypeAndSpec (ann ^. _1 % typePhraseExt % typeClauseName % to toAsmIdP) (toAsmIdP phr)
                        , map (toAsmIdP . fst) args
                        ,) 
                    $ fmap (++[Asm.CRet ()]) coms
                (PSimpleListCons _ l r, coms) ->
                    fmap 
                        ( Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_CONS__
                        -- OKAY TODO: in the front end, we should probably shift some the
                        -- types around so that we actually do have the type information
                        -- , map (toAsmIdP . fst) [l, r]
                        , map toAsmIdP [l,r]
                        ,) 
                    $ fmap (++[Asm.CRet ()]) coms
                (PSimpleListEmpty _, coms) ->
                    fmap 
                        ( Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_EMPTY__
                        -- OKAY TODO: in the front end, we should probably shift some the
                        -- types around so that we actually do have the type information
                        -- , map (toAsmIdP . fst) [l, r]
                        , []
                        ,) 
                    $ fmap (++[Asm.CRet ()]) coms
                (PSimpleUnit _, coms) ->
                    fmap 
                        ( Asm.TypeAndSpec uNIT_INTERNAL__ uNIT_INTERNAL__
                        -- OKAY TODO: in the front end, we should probably shift some the
                        -- types around so that we actually do have the type information
                        -- , map (toAsmIdP . fst) [l, r]
                        , []
                        ,) 
                    $ fmap (++[Asm.CRet ()]) coms


        EObjCallF ann phr (fmap snd -> args) -> do
            (argscmds, argsids) <- fmap unzip $ for args $ \arg -> do
                e <- freshTmpName
                arg' <- arg 
                return $ (arg' ++ [Asm.CStore () e], e)
            return $ 
                concat argscmds <> 
                [ case ann ^. _1 of
                     DataDefn objdefn -> 
                        Asm.CConstructor 
                            () 
                            ( Asm.TypeAndSpec
                                (objdefn ^. typePhraseExt % typeClauseName % to toAsmIdP)
                                (toAsmIdP phr)
                            )
                            argsids
                     CodataDefn objdefn -> 
                        Asm.CDestructor 
                            () 
                            ( Asm.TypeAndSpec
                                (objdefn ^. typePhraseExt % typeClauseName % to toAsmIdP)
                                (toAsmIdP phr)
                            )
                            (init argsids)
                            (last argsids)
                ]


        EListF _ (fmap snd -> lst) -> foldrM f z lst
          where
            f :: m [Asm.MplAsmCom MplAsmFromLambdaLifted] -> 
                [Asm.MplAsmCom MplAsmFromLambdaLifted] -> 
                m [Asm.MplAsmCom MplAsmFromLambdaLifted]
            f h acc = do
                el <- freshTmpName
                er <- freshTmpName
                h' <- fmap (++ [Asm.CStore () el]) h 
                let acc' = acc ++ [Asm.CStore () er]
                return $ concat [ h' , acc' ] ++
                    [ Asm.CConstructor () 
                        (Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_CONS__)
                        [el, er]
                    ]

            z :: [Asm.MplAsmCom MplAsmFromLambdaLifted]
            z = [Asm.CConstructor () (Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_EMPTY__) []]
        EStoreF _ (Right procdefn) -> do
            cmds' <- procdefn ^. _2 % to mplAssembleCmds 

            ins' <- for (procdefn ^.. _1 % _2 % folded) $ \ch -> do
                return $ toAsmIdP ch

            outs' <- for (procdefn ^.. _1 % _3 % folded) $ \ch -> do
                return $ toAsmIdP ch
            
            return [Asm.CStoreProc () (Right ((procdefn ^.. _1 % _1 % folded % to (\(PVar _ v) -> toAsmIdP v), ins', outs'), cmds'))]
        EStoreF _ (Left id) -> return [Asm.CStoreProc () (Left $ toAsmIdP id)]
        EStringF _ str -> foldrM f z str
          where
            f :: Char -> [Asm.MplAsmCom MplAsmFromLambdaLifted] -> m [Asm.MplAsmCom MplAsmFromLambdaLifted]
            f c acc = do
                el <- freshTmpName
                er <- freshTmpName
                
                let c' = [Asm.CChar () c, Asm.CStore () el]
                    acc' = acc ++ [Asm.CStore () er]
                return $ concat [ c' , acc' ] ++
                    [ Asm.CConstructor () 
                        (Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_CONS__)
                        [el, er]
                    ]

            z :: [Asm.MplAsmCom MplAsmFromLambdaLifted]
            z = [Asm.CConstructor () (Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_EMPTY__) []]

        EUnitF _ -> return [Asm.CConstructor () (Asm.TypeAndSpec uNIT_INTERNAL__ uNIT_INTERNAL__) []]

        ECallF _ callid (fmap snd -> args) -> do
        {-
        ECallF _ callid (fargs) -> do
            _ callid
            _ fargs
            let args = fmap snd fargs
        -}
            (argscmds, argsids) <- fmap unzip $ for args $ \arg -> do
                e <- freshTmpName
                arg' <- arg 
                return $ (arg' ++ [Asm.CStore () e], e)
            return $ 
                concat argscmds <>
                [ Asm.CCall 
                    ()
                    (toAsmIdP callid)
                    argsids
                ]
        -- ERecordF !(XERecord x) (NonEmpty (XERecordPhrase x, MplAST.MplCore.IdP x, ([XMplPattern x], r)))
        ERecordF _ (fmap (over (_3 % _2) snd) -> records) -> do
            seqcoms <- for records $ \(recordphrase, phrname, (patts, coms)) -> 
                fmap 
                    ( Asm.TypeAndSpec 
                        (recordphrase ^. typePhraseExt % typeClauseName % to toAsmIdP)
                        (toAsmIdP phrname)
                    , map (\(PVar _ p) -> toAsmIdP p) patts
                    , 
                    )
                $ fmap (++ [Asm.CRet ()])
                    coms
                
            return [Asm.CRecord () $ NE.toList seqcoms]


        ETupleF _ (t0, t1, ts) -> do
            (vs, es) <- fmap unzip $ for (t0:t1:ts) $ snd >>> \t -> do
                e <- freshTmpName
                t' <- t
                return (t' ++ [Asm.CStore () e], e)
            return $ concat vs <> [ Asm.CTuple () es ]

        EProjF _ projnum (exp, v) -> assert (projnum >= 0) $
            case exp of
                EVar _ e -> 
                    return [Asm.CProj () (fromIntegral projnum) (toAsmIdP e)]
                _ -> do
                    e <- freshTmpName
                    v' <- v
                    return $ v' <> [ Asm.CStore () e, Asm.CProj () (fromIntegral projnum) e ]

        EIfF _ (_, condc) (_, thenc) (_, elsec) -> do
            e <- freshTmpName
            condc' <- condc
            thenc' <- thenc
            elsec' <- elsec
            return $ condc' <> 
                [ Asm.CStore () e
                , Asm.CIf () e 
                    (thenc' ++ [Asm.CRet ()]) 
                    (elsec' ++ [Asm.CRet ()])
                ]

        EIllegalInstrF _ -> return []

                
  {-
  | EBuiltInOpF !(XEBuiltInOp x) BuiltInOperators r r
  | ELetF !(XELet x) (NonEmpty (MplAST.MplCore.MplStmt x)) r
  | EFoldF !(XEFold x)
           r
           (NonEmpty
              (XEFoldPhrase x, MplAST.MplCore.IdP x, [XMplPattern x], r))
  | EUnfoldF !(XEUnfold x)
             r
             (NonEmpty
                (XEUnfoldPhrase x, XMplPattern x,
                 NonEmpty
                   (XEUnfoldSubPhrase x, MplAST.MplCore.IdP x, [XMplPattern x], r)))
  | ESwitchF !(XESwitch x) (NonEmpty (r, r))
  | EIllegalInstrF !(XEIllegalInstr x)
  | XExprF !(XXExpr x)
  -}

mplAssembleCmds ::
    forall s m e.
    ( HasUniqueSupply s
    , MonadState s m 
    , MonadWriter [e] m 
    , AsFromLambdaLiftedError e 
    ) =>
    NonEmpty (MplCmd MplLambdaLifted) -> 
    m (Asm.MplAsmComs MplAsmFromLambdaLifted)
mplAssembleCmds = fmap concat . traverse mplAssembleCmd

mplAssembleCmd :: 
    forall s m e.
    ( HasUniqueSupply s
    , MonadState s m 
    , MonadWriter [e] m 
    , AsFromLambdaLiftedError e 
    ) =>
    MplCmd MplLambdaLifted -> 
    m [Asm.MplAsmCom MplAsmFromLambdaLifted]
mplAssembleCmd = cata go
  where
    go = \case
        CRunF _ call seqs ins outs -> do
            (seqs', es) <- fmap unzip $ for seqs $ \seq -> do
                e <- freshTmpName
                seq' <- mplAssembleExpr seq
                return $ (seq' ++ [Asm.CStore () e], e)
                
            case call of
                Left callId -> return $ concat seqs' <> [ Asm.CRun () False (toAsmIdP callId) (es, map toAsmIdP ins, map toAsmIdP outs) ]
                Right expr -> do
                    (expr', e) <- do
                        e <- freshTmpName
                        expr' <- mplAssembleExpr expr
                        return (expr' ++ [Asm.CStore () e], e)
                    return $ concat seqs' <> expr' <> [ Asm.CRun () True e (es, map toAsmIdP ins, map toAsmIdP outs) ]

        CCloseF _ ch -> return [Asm.CClose () $ toAsmIdP ch]
        CHaltF _ ch -> return [Asm.CHalt () $ toAsmIdP ch]
        CGetF _ (PVar _ v) ch -> return [ Asm.CGet () (toAsmIdP v) (toAsmIdP ch) ]
        CPutF _ expr ch -> do
            e <- freshTmpName
            expr' <- mplAssembleExpr expr
            return $ expr' ++ [ Asm.CStore () e, Asm.CPut () e (toAsmIdP ch)]
        CHCaseF _ ch cases -> do
            cases' <- traverse f cases
            return $ [Asm.CHCase () (toAsmIdP ch) (NE.toList cases')]
          where
            f (procdefn, phr, cmds) = do
                let tp = case procdefn of
                        ProtocolDefn defn -> defn ^. typePhraseExt % typeClauseName
                        CoprotocolDefn defn -> defn ^. typePhraseExt % typeClauseName
                cmds' <- sequenceA cmds
                return (Asm.TypeAndSpec (toAsmIdP tp) (toAsmIdP phr),  concat cmds' )
            
        CHPutF ann phr ch -> do
            let chname = ch ^. nameStr
                chpol = ch ^. chIdentRPolarity
                -- this is more accurately the name of the clause
                tp = case ann ^. _2 of
                    ProtocolDefn defn -> defn ^. typePhraseExt % typeClauseName
                    CoprotocolDefn defn -> defn ^. typePhraseExt % typeClauseName

                phrname = phr ^. name % coerced 

                -- the type of the from expression
                fromtp = case ann ^. _2 of
                    ProtocolDefn defn -> defn ^. typePhraseFrom
                    CoprotocolDefn defn -> defn ^. typePhraseFrom
                -- the tpye of the to expression
                totp = case ann ^. _2 of
                    ProtocolDefn defn -> defn ^. typePhraseTo
                    CoprotocolDefn defn -> defn ^. typePhraseTo
                    
            {- Here's the idea... it uses the type of the phrase to determine whether it is a
             - service channel or not...  -}
            
            case chname of
                -- DEPRECATED this underscore syntax -- it just doesn't play
                -- nicely with certain services which may or may not be a
                -- service depending on what is hcased.
                _ -> case chpol of
                -- '_':_ -> case chpol of
                    Input -> case totp of
                        -- String 
                        TypeBuiltIn (TypeGetF _ (TypeBuiltIn (TypeListF _ (TypeBuiltIn (TypeCharF _)))) _) 
                            | phrname == "ConsolePut" -> return [ Asm.CSHPut () Asm.SHPutString (toAsmIdP ch) ]
                        TypeBuiltIn (TypePutF _ (TypeBuiltIn (TypeListF _ (TypeBuiltIn (TypeCharF _)))) _) 
                            | phrname == "ConsoleGet" -> return [ Asm.CSHPut () Asm.SHGetString (toAsmIdP ch) ]
                        -- S (*) Neg(StringTerminal)
                        {-
                        TypeBuiltIn 
                                (TypeParF _ 
                                    (TypeVar _ _) 
                                    -- Okay technically, this should be a type with no args.... But we're just gonna ignore this actually..
                                    (TypeBuiltIn (TypeNegF _ (TypeConcWithArgs _ ( traceShowId . view (name % coerced) -> "StringTerminal") ([],[]))))
                                )
                        -}
                        TypeBuiltIn 
                                (TypeTensorF _ 
                                    (TypeVar _ _) 
                                    (TypeBuiltIn (TypeNegF _ _))
                                )
                            | phrname == "ConsoleStringTerminal" -> return [ Asm.CSHPut () Asm.SHSplitNegStringTerm (toAsmIdP ch) ]

                        TypeBuiltIn (TypeTopBotF _) 
                            | phrname == "ConsoleClose" -> return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]


                        -- Int
                        TypeBuiltIn (TypeGetF _ (TypeBuiltIn (TypeIntF _)) _ ) 
                            | phrname == "IntConsolePut" -> return [ Asm.CSHPut () Asm.SHPutInt (toAsmIdP ch) ]
                        TypeBuiltIn (TypePutF _ (TypeBuiltIn (TypeIntF _)) _ ) 
                            | phrname == "IntConsoleGet" -> return [ Asm.CSHPut () Asm.SHGetInt (toAsmIdP ch) ]
                        TypeBuiltIn (TypeTopBotF _) 
                            | phrname == "IntConsoleClose" -> return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]

                        -- Char
                        TypeBuiltIn (TypeGetF _ (TypeBuiltIn (TypeCharF _)) _ ) 
                            | phrname == "CharConsolePut" -> return [ Asm.CSHPut () Asm.SHPutChar (toAsmIdP ch) ]
                        TypeBuiltIn (TypePutF _ (TypeBuiltIn (TypeCharF _)) _ ) 
                            | phrname == "CharConsoleGet" -> return [ Asm.CSHPut () Asm.SHGetChar (toAsmIdP ch) ]
                        TypeBuiltIn (TypeTopBotF _) 
                            | phrname == "CharConsoleClose" -> return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]

                        -- Timeout  (recall this is of input polarity
                        -- Timer:: C => Get(Int | C (*) Put(() | TopBot))
                        TypeBuiltIn (TypeGetF _ (TypeBuiltIn (TypeIntF _)) (TypeBuiltIn (TypeTensorF _ (TypeVar _ _) (TypeBuiltIn (TypePutF _ (TypeBuiltIn (TypeUnitF _)) (TypeBuiltIn (TypeTopBotF _)))))))
                            | phrname == "Timer" -> return [ Asm.CSHPut () Asm.SHTimeOut (toAsmIdP ch) ]
                        TypeBuiltIn (TypeTopBotF _) 
                            | phrname == "TimerClose" -> return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]
                        

                        -- _ -> tell [_NoService # (ch, phr, ann ^. _2)] >> return []
                        _ -> return [ Asm.CHPut () (Asm.TypeAndSpec (toAsmIdP tp) (toAsmIdP phr)) (toAsmIdP ch) ]

                    Output -> case fromtp of
                        -- String 
                        TypeBuiltIn (TypeGetF _ (TypeBuiltIn (TypeListF _ (TypeBuiltIn (TypeCharF _)))) _) 
                            | phrname == "StringTerminalGet" -> return [ Asm.CSHPut () Asm.SHGetString (toAsmIdP ch) ]
                        TypeBuiltIn (TypePutF _ (TypeBuiltIn (TypeListF _ (TypeBuiltIn (TypeCharF _)))) _) 
                            | phrname == "StringTerminalPut" -> return [ Asm.CSHPut () Asm.SHPutString (toAsmIdP ch) ]
                        TypeBuiltIn (TypeTopBotF _) 
                            | phrname == "StringTerminalClose" -> return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]

                        -- Int
                        TypeBuiltIn (TypeGetF _ (TypeBuiltIn (TypeIntF _)) _ ) 
                            | phrname == "IntTerminalGet" -> return [ Asm.CSHPut () Asm.SHGetInt (toAsmIdP ch) ]
                        TypeBuiltIn (TypePutF _ (TypeBuiltIn (TypeIntF _)) _ ) 
                            | phrname == "IntTerminalPut" -> return [ Asm.CSHPut () Asm.SHPutInt (toAsmIdP ch) ]
                        TypeBuiltIn (TypeTopBotF _) 
                            | phrname == "IntTerminalClose" -> return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]

                        -- Char
                        TypeBuiltIn (TypeGetF _ (TypeBuiltIn (TypeCharF _)) _ ) 
                            | phrname == "CharTerminalGet" -> return [ Asm.CSHPut () Asm.SHGetChar (toAsmIdP ch) ]
                        TypeBuiltIn (TypePutF _ (TypeBuiltIn (TypeCharF _)) _ ) 
                            | phrname == "CharTerminalPut" -> return [ Asm.CSHPut () Asm.SHPutChar (toAsmIdP ch) ]
                        TypeBuiltIn (TypeTopBotF _)  
                            | phrname == "CharTerminalClose" -> return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]
                    {-
                    Output -> case fromtp of
                        TypeBuiltIn (TypeGetF _ (TypeBuiltIn (TypeIntF _)) _ ) -> 
                            return [ Asm.CSHPut () Asm.SHGetInt (toAsmIdP ch) ]
                        TypeBuiltIn (TypePutF _ (TypeBuiltIn (TypeIntF _)) _ ) -> 
                            return [ Asm.CSHPut () Asm.SHPutInt (toAsmIdP ch) ]
                        TypeBuiltIn (TypeTopBotF _) -> 
                            return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]
                    -}
                    {-
                    Output -> case phr ^. nameStr of
                        "IntTerminalGet" -> return [ Asm.CSHPut () Asm.SHGetInt (toAsmIdP ch) ]
                        "IntTerminalPut" -> return [ Asm.CSHPut () Asm.SHPutInt (toAsmIdP ch) ]
                        "IntTerminalClose" -> return [ Asm.CSHPut () Asm.SHClose (toAsmIdP ch) ]
                    -}
                        -- _ -> tell [_NoService # (ch, phr, ann ^. _2)] >> return []

                        _ -> return [ Asm.CHPut () (Asm.TypeAndSpec (toAsmIdP tp) (toAsmIdP phr)) (toAsmIdP ch) ]

                -- This case is redundant with the previous case.
                -- _ -> return [ Asm.CHPut () (Asm.TypeAndSpec (toAsmIdP tp) (toAsmIdP phr)) (toAsmIdP ch) ]

        CSplitF _ ch (lch, rch) -> return [Asm.CSplit () (toAsmIdP ch) (toAsmIdP lch, toAsmIdP rch) ]
        CForkF _ ch ((ch0, withs0, cmds0), (ch1, withs1, cmds1)) -> do
            cmds0' <- fmap concat $ sequenceA cmds0
            cmds1' <- fmap concat $ sequenceA cmds1
            return 
                [ Asm.CFork 
                    () 
                    (toAsmIdP ch) 
                    ( (toAsmIdP ch0, map toAsmIdP withs0, cmds0')
                    , (toAsmIdP ch1, map toAsmIdP withs1, cmds1')
                    )
                ]
        CIdF _ (lch, rch) -> return [Asm.CId () (toAsmIdP lch, toAsmIdP rch)]
        -- CIdNegF _ (lch, rch) -> return [Asm.CId () (toAsmIdP lch, toAsmIdP rch)]
        {-
        We need to do some work to get the negation to work correctly. Essentially, 
        we need the id comand to be not on the the dummy channel (channel of type negation),
        for this to not deadlock.

        First, we need to assert that they both have the same polarity (obviously).
        -}
        {-
        CIdNegF _ (lch, rch) -> assert (and 
            [ ((==) `on` view polarity) lch rch
            , has (chIdentTType % _TypeBuiltIn % _TypeNegF) lch || has (chIdentTType % _TypeBuiltIn % _TypeNegF) lch
            ]) $ return $ case lch ^. chIdentTType of
                TypeBuiltIn (TypeNegF _ _) -> rchislch
                _ -> lchisrch
          where
            rchislch = [Asm.CId () (toAsmIdP rch, toAsmIdP lch)]
            lchisrch = [Asm.CId () (toAsmIdP rch, toAsmIdP lch)]
        -}
        CIdNegF _ (lch, rch) -> assert (and 
            [ ((==) `on` view polarity) lch rch
            , has (chIdentTType % _TypeBuiltIn % _TypeNegF) lch || has (chIdentTType % _TypeBuiltIn % _TypeNegF) lch
            ]) $ return rchislch
          where
            rchislch = [Asm.CId () (toAsmIdP rch, toAsmIdP lch)]
            -- lchisrch = [Asm.CId () (toAsmIdP rch, toAsmIdP lch)]

        CRaceF _ races -> do
            races' <- traverse f races 
            return [Asm.CRace () (NE.toList races')]
          where
            f (ch, cmds) = do
                cmds' <- fmap concat $ sequenceA cmds
                return (toAsmIdP ch, cmds')
    {-
  | MplAST.MplCmd.CPlugF !(MplAST.MplCmd.XCPlug x)
                         ((XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r),
                          (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r))
    -}
        {- This will:
         -      - get the plugged channels (the lens operator soup)
         -      - then put the plug phrases together. 
         - See @mplAssemblePlugs@ for the algorithm
         -}
        CPlugsF ann (p0, p1, ps) -> mplAssemblePlugs (ann ^.. _2 % folded % _1) (p0:p1:ps)
  {-
  | CPlugsF !(XCPlugs x)
            ((XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r),
             (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r),
             [(XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r)])
  -}
        CCaseF _ caseon cases -> do
            e <- freshTmpName
            caseon' <- mplAssembleExpr caseon
            cases' <- traverse f cases
            return $ 
                caseon' <>
                [ Asm.CStore () e
                , Asm.CCase () e (NE.toList cases')
                ]

          where
            -- duplciated code from the above case for expressions
            f :: (XCCasePattern (MplPass 'LambdaLifted), NonEmpty (m [Asm.MplAsmCom MplAsmFromLambdaLifted])) -> 
                m (Asm.TypeAndSpec MplAsmFromLambdaLifted, [Asm.Name], Asm.MplAsmComs MplAsmFromLambdaLifted)
            f = \case
                (PSimpleConstructor ann phr args, coms) ->   
                    fmap 
                        ( Asm.TypeAndSpec (ann ^. _1 % typePhraseExt % typeClauseName % to toAsmIdP) (toAsmIdP phr)
                        , map (toAsmIdP . fst) args
                        ,) 
                    $ fmap concat
                    $ sequenceA coms
                (PSimpleListCons _ l r, coms) ->
                    fmap 
                        ( Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_CONS__
                        -- OKAY TODO: in the front end, we should probably shift some the
                        -- types around so that we actually do have the type information
                        -- , map (toAsmIdP . fst) [l, r]
                        , map toAsmIdP [l,r]
                        ,) 
                    $ fmap concat
                    $ sequenceA coms
                (PSimpleListEmpty _, coms) ->
                    fmap 
                        ( Asm.TypeAndSpec lIST_INTERNAL__ lIST_INTERNAL_EMPTY__
                        -- OKAY TODO: in the front end, we should probably shift some the
                        -- types around so that we actually do have the type information
                        -- , map (toAsmIdP . fst) [l, r]
                        , []
                        ,) 
                    $ fmap concat
                    $ sequenceA coms
                (PSimpleUnit _, coms) ->
                    fmap 
                        ( Asm.TypeAndSpec uNIT_INTERNAL__ uNIT_INTERNAL__
                        -- OKAY TODO: in the front end, we should probably shift some the
                        -- types around so that we actually do have the type information
                        -- , map (toAsmIdP . fst) [l, r]
                        , []
                        ,) 
                    $ fmap concat
                    $ sequenceA coms
        CIfF _ condc thenc elsec -> do
            e <- freshTmpName
            condc' <- mplAssembleExpr condc
            thenc' <- fmap concat $ sequenceA thenc
            elsec' <- fmap concat $ sequenceA elsec
            return $ condc' <> [Asm.CStore () e, Asm.CIf () e thenc' elsec']

        CIllegalInstrF _ -> return []
  {-
  | MplAST.MplCmd.CIfF !(MplAST.MplCmd.XCIf x)
                       (XMplExpr x)
                       (NonEmpty r)
                       (NonEmpty r)

  | CIllegalInstrF !(XCIllegalInstr x)
  -}

{- | assemble a list of plug expressions. We want the following:
 -      - given a plugs expression, we want to compile this down to plug expressions (which only plug two phrases together)
 -          which must have the output plugged channels first, then the input channels.
 - Here's how we do this.
 -      - Let plugged denote the plugged channels
 -      -     lst denote the plug phrases
 -      - Find a ``starting'' plug first i.e., a plug which has no plugged channels on its input but a plugged channel on its output
 -              and move this to the front of lst.
 -      - Then, do the following until there are either no more plugged channels OR there are no plugged phrases left
 -          - at the front of lst, compute
 -              - pluggedofphrase <- plugged \\ the output channels of the head of lst
 -}
mplAssemblePlugs ::
    forall s m.
    ( HasUniqueSupply s
    , MonadState s m ) =>
    -- | plugged channels
    [IdentT] -> 
    -- | all the plug phrases
    [(XCPlugPhrase MplLambdaLifted, ([ChP MplLambdaLifted], [ChP MplLambdaLifted]), NonEmpty (m [Asm.MplAsmCom MplAsmFromLambdaLifted]))] -> 
    -- | resulting plug assembly
    m (Asm.MplAsmComs MplAsmFromLambdaLifted)
mplAssemblePlugs plugged = 
    {- sort so that we have the input to 'goOutput' has the first element with a plugged OUTPUT channel -}
    goOutput [] plugged . moveStartingPlugPhraseToHead
  where
    {- moves the starting plug first -}
    moveStartingPlugPhraseToHead = 
        uncurry (flip (++)) 
        . break 
            ( uncurry (&&) 
            <<< (view (_2 % _1 % to ((==0) . numberOfPlugged plugged))) 
                &&& (view (_2 % _2 % to ((>0) . numberOfPlugged plugged)))
            )

    moveIntersectWithInputs pluggeds =
        uncurry (flip (++)) . break ((view (_2 % _1 % to ((>0) . numberOfPlugged pluggeds))))
    moveIntersectWithOutputs pluggeds =
        uncurry (flip (++)) . break ((view (_2 % _2 % to ((>0) . numberOfPlugged pluggeds))))

    numberOfPlugged pluggeds = length . filter ((`elem`pluggeds) . view identR)
    
    -- assumes that the head of the input list has an plugged OUTPUT channel
    goOutput pluggedinscope pluggeds lst = case lst of
        [outplg] -> fmap concat $ sequenceOf traversed (outplg ^. _3)
        outplg:rst -> do
            let topwiths = collectUsedChannels (pluggedinscope ++ pluggedof) [outplg]
                botwiths = collectUsedChannels (pluggedinscope ++ pluggedof) rst
                pluggedof = outplg ^. _2 % to (usedPlugsOf pluggeds)

            topcmds <- fmap concat $ sequenceOf traversed (outplg ^. _3)
            botcmds <- goInp (pluggedinscope ++ pluggedof) (pluggeds \\ pluggedof) $ moveIntersectWithInputs pluggedof rst

            return [Asm.CPlug () (map toAsmIdP pluggedof) ((topwiths, topcmds), (botwiths, botcmds)) ]

        -- this case is impossible 
        [] -> return []

    goInp pluggedinscope pluggeds lst  = case lst of
        [inpplg] -> fmap concat $ sequenceOf traversed (inpplg ^. _3)
        inpplg:rst -> do
            -- _ inpplg
            let topwiths = collectUsedChannels (pluggedinscope ++ pluggedof) [inpplg]
                botwiths = collectUsedChannels (pluggedinscope ++ pluggedof) rst
                pluggedof = inpplg ^. _2 % to (usedPlugsOf pluggeds)

            topcmds <- fmap concat $ sequenceOf traversed (inpplg ^. _3)
            botcmds <- goInp (pluggedinscope ++ pluggedof) (pluggeds \\ pluggedof) $ moveIntersectWithOutputs pluggedof rst


            return [Asm.CPlug () (map toAsmIdP pluggedof) ((topwiths, topcmds), (botwiths, botcmds))]
            
        -- this case is  imposible
        [] -> return []

            -- CPlug (XCPlug x) [IdP x] (PlugPhrase x, PlugPhrase x)

    -- assumes that the head of the input list has an plugged INPUT channel
    
    -- collects all the used channels in a list of plugged phrases
    collectUsedChannels :: 
        -- | the plugged context in the current phrase
        [IdentT] ->
        [(XCPlugPhrase MplLambdaLifted, ([ChP MplLambdaLifted], [ChP MplLambdaLifted]), NonEmpty (m [Asm.MplAsmCom MplAsmFromLambdaLifted]))] -> 
        ([Asm.IdP MplAsmFromLambdaLifted], [Asm.IdP MplAsmFromLambdaLifted])
    {-
    collectUsedChannels usedplugged = 
        map toAsmIdP 
        . filter (`notElem` usedplugged) 
        . map (view identR)
        . toListOf (folded % _2 % each % folded)
    -}
    collectUsedChannels usedplugged = foldMapOf (folded % _2) (over each f)
      where
        f = map toAsmIdP 
            -- . filter (`elem` usedplugged) 
            . filter (\v -> if v `elem` plugged then v `elem` usedplugged else True) 
            . map (view identR)

    -- computes the channels which are actually plugged in some channels
    usedPlugsOf ::
        -- | the plugged channels
        [IdentT] -> 
        -- | the channels in a phrase
        ([ChP MplLambdaLifted], [ChP MplLambdaLifted]) ->
        -- | returns the pluggeds used in the phrase
        [IdentT] 
    usedPlugsOf pluggeds = intersect pluggeds . map (view identR) . uncurry (++)

-- | generates a fresh name. This is mainly used for generating expressions.
freshTmpName ::
    ( HasUniqueSupply s
    , MonadState s m ) =>
    m (Asm.IdP MplAsmFromLambdaLifted)
freshTmpName = freshUniqueSupply <&> 
    coerce 
    . ('e':) 
    . show 
    . (coerce :: Unique -> Word) 
    . uniqueFromSupply

getExprType ::
    MplExpr MplLambdaLifted -> 
    MplType MplTypeChecked
getExprType = \case
    EVar ann _ -> ann
    EInt ann _ -> ann
    EChar ann _ -> ann
    EDouble ann _ -> ann
    EPOps ann _ _ _ -> ann
    EBool ann _ -> ann
    ECase ann _ _ -> ann
    EObjCall ann _ _ -> snd ann
    ECall ann _ _ -> ann
    ERecord ann _ -> ann
    EList ann _ -> ann
    EString ann _ -> ann
    EUnit ann -> ann
    ETuple ann _ -> ann
    EProj ann _ _ -> ann
    EBuiltInOp ann _ _ _ -> ann
    EIf ann _ _ _ -> ann
    EStore ann _ -> ann
    {-
    EFold _ _ _ -> undefined
    EUnfold _ _ _ -> undefined
    -} 
    EIllegalInstr ann -> ann
