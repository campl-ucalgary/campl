{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplCliRunner.LambdaLiftedToAsm where

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

import Data.Text.Prettyprint.Doc

import Optics
import Control.Arrow
import Control.Exception
import Control.Applicative

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty (..) )

import Data.Ord
import Data.List
import Data.Maybe
import Data.Coerce
import Data.Foldable
import Data.Traversable
import Data.Bool

import Control.Monad.State
import Data.Functor.Base ( ListF (..), NonEmptyF (..) )
import Data.Functor.Foldable ( cata, para, Base )

data MplFrontEnd 

{- | the usual configuring the AST -}
type instance Asm.IdP MplFrontEnd = Asm.Name

type instance Asm.XCAssign MplFrontEnd = ()
type instance Asm.XCLoad MplFrontEnd = ()
type instance Asm.XCStore MplFrontEnd = ()
type instance Asm.XCRet MplFrontEnd = ()
type instance Asm.XCCall MplFrontEnd = ()
type instance Asm.XCInt MplFrontEnd = ()
type instance Asm.XCChar MplFrontEnd = ()
type instance Asm.XCBool MplFrontEnd = ()
type instance Asm.XCEqInt MplFrontEnd = ()

type instance Asm.XCLeqInt MplFrontEnd = ()
type instance Asm.XCEqChar MplFrontEnd = ()
type instance Asm.XCLeqChar MplFrontEnd = ()

type instance Asm.XCAdd MplFrontEnd = ()
type instance Asm.XCSub MplFrontEnd = ()
type instance Asm.XCMul MplFrontEnd = ()
type instance Asm.XCConstructor MplFrontEnd = ()
type instance Asm.XCDestructor MplFrontEnd = ()

type instance Asm.XCCase MplFrontEnd = ()
type instance Asm.XCRecord MplFrontEnd = ()
type instance Asm.XCIf MplFrontEnd = ()

type instance Asm.XCTuple MplFrontEnd = ()
type instance Asm.XCProj MplFrontEnd = ()

type instance Asm.XCGet MplFrontEnd = ()
type instance Asm.XCPut MplFrontEnd = ()
type instance Asm.XCHPut MplFrontEnd = ()
type instance Asm.XCHCase MplFrontEnd = ()
type instance Asm.XCSplit MplFrontEnd = ()
type instance Asm.XCFork MplFrontEnd = ()
type instance Asm.XCPlug MplFrontEnd = ()
type instance Asm.XCRun MplFrontEnd = ()
type instance Asm.XCId MplFrontEnd = ()
type instance Asm.XCRace MplFrontEnd = ()
type instance Asm.XCClose MplFrontEnd = ()
type instance Asm.XCHalt MplFrontEnd = ()


-- | overloaded operator for converting an identifier to the assembler identifier
class ToAsmIdP t where
    toAsmIdP :: t -> Asm.IdP MplFrontEnd

instance ToAsmIdP IdentR where
    toAsmIdP identt = coerce $ concat
        [ identt ^. name % coerced 
        , show (coerce (identt ^. identRUniqueTag) :: Word)
        ]

instance ToAsmIdP ChIdentT where
    toAsmIdP chidentt = chidentt ^. chIdentTChIdentR % chIdentRIdentR % to toAsmIdP

-- | this will assemble an entire program
mplAssembleProg ::
    UniqueSupply -> 
    MplProg MplLambdaLifted ->
    Asm.MplAsmProg MplFrontEnd
-- we assert that there are nothing in the where bindings (which shouldn't be the case 
-- from lambda lifting)
mplAssembleProg uniq prog = assert (all (null . view stmtWhereBindings) prog') $ 
    Asm.MplAsmProg 
        (evalState (fmap concat $ traverse mplAssembleStmt stmts) s0) 
        (fmap assemblemain maybemain)
  where 
    prog' :: [MplStmt MplLambdaLifted]
    prog' = coerce prog
    --
    ~(s0:s1:_) = uniqueSupplies uniq
    -- we find the bottom most definition named @run@ as the main function.
    stmts :: [MplStmt MplLambdaLifted]
    maybemain :: Maybe (IdP MplLambdaLifted, ([IdP MplLambdaLifted],[ChP MplLambdaLifted],[ChP MplLambdaLifted]),_)
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
                        then 
                            ( lst'
                            , Just 
                                ( proc ^. procName
                                , 
                                    ( proc ^. procDefn % to NE.head % _1 % _1 % to (map (\(PVar _ v) -> v))
                                    , proc ^. procDefn % to NE.head % _1 % _2
                                    , proc ^. procDefn % to NE.head % _1 % _3
                                    )
                                , proc ^. procDefn % to NE.head % _2 
                                )
                            )
                        else (defn : lst', Nothing)
                    | otherwise -> (defn : lst', Nothing)
                Nil -> (mempty, mzero)
        Nil -> (mempty, mzero)

    assemblemain :: 
        (IdentT, ([IdentT], [ChIdentT], [ChIdentT]), NonEmpty (MplCmd MplLambdaLifted)) -> 
            (Asm.Name, ([Asm.Name], [Asm.Name], [Asm.Name]), Asm.MplAsmComs MplFrontEnd)
    assemblemain (mainname, (seqs, ins, outs), cmds) = 
        ( toAsmIdP mainname
        , 
            ( map toAsmIdP seqs
            , map toAsmIdP ins
            , map toAsmIdP outs
            )
        , evalState (mplAssembleCmds cmds) s1
        )


mplAssembleStmt ::
    forall s m.
    ( HasUniqueSupply s
    , MonadState s m ) =>
    MplStmt MplLambdaLifted ->
    m [Asm.MplAsmStmt MplFrontEnd]
mplAssembleStmt stmt = fmap (filter filtercond) $ sequenceA
    [ pure $ Asm.Constructors (concat $ mapMaybe constructorStmts collectedDefns)
    , pure $ Asm.Destructors (concat $ mapMaybe destructorStmts collectedDefns)
    , pure $ Asm.Protocols (concat $ mapMaybe protocolStmts collectedDefns)
    , pure $ Asm.Coprotocols (concat $ mapMaybe coprotocolStmts collectedDefns)
    , fmap Asm.Functions $ sequenceA $ mapMaybe funStmts collectedDefns
    , fmap Asm.Processes $ sequenceA $ mapMaybe procStmts collectedDefns
    ]
  where
    filtercond :: Asm.MplAsmStmt MplFrontEnd -> Bool
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
    constructorStmts :: MplDefn MplLambdaLifted -> Maybe [Asm.TypeAndSeqSpecs MplFrontEnd]
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
    destructorStmts :: MplDefn MplLambdaLifted -> Maybe [Asm.TypeAndSeqSpecs MplFrontEnd]
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
                            &&& view (typePhraseFrom % _1 % to (succ . genericLength) )
                        ) 
                )

    protocolStmts :: MplDefn MplLambdaLifted -> Maybe [Asm.TypeAndConcSpecs MplFrontEnd]
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
    coprotocolStmts :: MplDefn MplLambdaLifted -> Maybe [Asm.TypeAndConcSpecs MplFrontEnd]
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


    funStmts :: MplDefn MplLambdaLifted -> Maybe (m (Asm.Name, [Asm.Name], Asm.MplAsmComs MplFrontEnd))
    funStmts defn = case defn of
        -- assert is here since from compilation of pattern matching, there should only be one pattern.
        FunctionDefn fundefn -> assert (lengthOf (funDefn % folded) fundefn == 1) 
            $ Just
            $ fmap 
                ( fundefn ^. funName % to toAsmIdP
                , fundefn ^.. funDefn % to NE.head % _1 % folded % to (\(PVar _ v) -> toAsmIdP v )
                ,)
            $ fundefn ^. funDefn % to NE.head % _2 % to mplAssembleExpr

        _ -> Nothing

    procStmts :: MplDefn MplLambdaLifted -> Maybe (m (Asm.Name, ([Asm.Name], [Asm.Name], [Asm.Name]), Asm.MplAsmComs MplFrontEnd))
    procStmts defn = case defn of
        ProcessDefn procdefn -> assert (lengthOf (procDefn % folded) procdefn == 1) 
            $ Just 
            $ fmap 
                ( procdefn ^. procName % to toAsmIdP
                , 
                    ( procdefn ^.. procDefn % to NE.head % _1 % _1 % folded % to (\(PVar _ v) -> toAsmIdP v )
                    , procdefn ^.. procDefn % to NE.head % _1 % _2 % folded % to toAsmIdP
                    , procdefn ^.. procDefn % to NE.head % _1 % _3 % folded % to toAsmIdP
                    )
                ,)
            $ procdefn ^. procDefn % to NE.head % _2 % to mplAssembleCmds 
        _ -> Nothing
        


    {-
    procStmts :: MplDefn MplLambdaLifted -> Maybe (Asm.MplAsmStmt MplFrontEnd)
    procStmts defn = case defn of
        -- assert is here since from compilation of pattern matching, there should only be one pattern (similarly to fun stmts).
        ProcessDefn procdefn -> Just $ assert (lengthOf (procDefn % folded) procdefn == 1) $
            undefined
        _ -> Nothing
        
    -}

mplAssembleExpr :: 
    forall s m.
    ( HasUniqueSupply s
    , MonadState s m ) =>
    MplExpr MplLambdaLifted -> 
    m (Asm.MplAsmComs MplFrontEnd)
mplAssembleExpr = cata go
  where
    go = \case
        EPOpsF _ op l r -> fmap concat $ sequenceA [r, l, pure op']
          where
            op' = pure $ case op of
                PrimitiveAdd -> Asm.CAdd ()
                _ -> error $ "assembling of operation is not implemented yet" ++ show op
        
        EVarF _ idp -> pure [Asm.CLoad () (toAsmIdP idp)]
        EIntF _ n -> pure [Asm.CInt () n]
        ECharF _ c -> pure [Asm.CChar () c]
        EBoolF _ b -> pure [Asm.CBool () b]
        ECaseF _ caseon cases -> do
            e <- freshTmpName
            caseon' <- caseon
            cases' <- traverse f cases
            return $ 
                caseon' <>
                [ Asm.CStore () e
                , Asm.CCase () e (NE.toList cases')
                ]
          where
            f :: (XECasePattern (MplPass 'LambdaLifted), m [Asm.MplAsmCom MplFrontEnd]) -> 
                m (Asm.TypeAndSpec MplFrontEnd, [Asm.IdP MplFrontEnd], Asm.MplAsmComs MplFrontEnd)
            f (PSimpleConstructor ann phr args, coms) =   
                fmap 
                    ( Asm.TypeAndSpec (ann ^. _1 % typePhraseExt % typeClauseName % to toAsmIdP) (toAsmIdP phr)
                    , map (toAsmIdP . fst) args
                    ,) 
                    coms
        EObjCallF ann phr args -> do
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

        ECallF _ callid args -> do
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
        ERecordF _ records -> do
            seqcoms <- for records $ \(recordphrase, phrname, (patts, coms)) -> 
                fmap 
                    ( Asm.TypeAndSpec 
                        (recordphrase ^. typePhraseExt % typeClauseName % to toAsmIdP)
                        (toAsmIdP phrname)
                    , map (\(PVar _ p) -> toAsmIdP p) patts
                    , 
                    )
                    coms
                
            return [Asm.CRecord () $ NE.toList seqcoms]

  {-
  | EDoubleF !(XEDouble x) Double
  -}

  {-
  | EListF !(XEList x) [r]
  | MplAST.MplCore.EStringF !(XEString x) [Char]
  | EUnitF !(XEUnit x)
  -}

        ETupleF _ (t0, t1, ts) -> do
            (vs, es) <- fmap unzip $ for (t0:t1:ts) $ \t -> do
                e <- freshTmpName
                t' <- t
                return (t' ++ [Asm.CStore () e], e)
            return $ concat vs <> [ Asm.CTuple () es ]

        EProjF _ projnum v -> assert (projnum >= 0) $ do
            e <- freshTmpName
            v' <- v 
            return $ v' <> [ Asm.CStore () e, Asm.CProj () (fromIntegral projnum) e ]

        EIfF _ condc thenc elsec -> do
            e <- freshTmpName
            condc' <- condc
            thenc' <- thenc
            elsec' <- elsec
            return $ condc' <> [Asm.CStore () e, Asm.CIf () e thenc' elsec']

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
    forall s m.
    ( HasUniqueSupply s
    , MonadState s m ) =>
    NonEmpty (MplCmd MplLambdaLifted) -> 
    m (Asm.MplAsmComs MplFrontEnd)
mplAssembleCmds = fmap concat . traverse mplAssembleCmd

mplAssembleCmd :: 
    forall s m.
    ( HasUniqueSupply s
    , MonadState s m ) =>
    MplCmd MplLambdaLifted -> 
    m [Asm.MplAsmCom MplFrontEnd]
mplAssembleCmd = cata go
  where
    go = \case
        CRunF _ callid seqs ins outs -> do
            (seqs', es) <- fmap unzip $ for seqs $ \seq -> do
                e <- freshTmpName
                seq' <- mplAssembleExpr seq
                return $ (seq' ++ [Asm.CStore () e], e)
                
            return $ concat seqs' <> [ Asm.CRun () (toAsmIdP callid) (es, map toAsmIdP ins, map toAsmIdP outs) ]
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
            let tp = case ann ^. _2 of
                    ProtocolDefn defn -> defn ^. typePhraseExt % typeClauseName
                    CoprotocolDefn defn -> defn ^. typePhraseExt % typeClauseName
            return [ Asm.CHPut () (Asm.TypeAndSpec (toAsmIdP tp) (toAsmIdP phr) ) (toAsmIdP ch) ]
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
        CIdNegF _ (lch, rch) -> return [Asm.CId () (toAsmIdP lch, toAsmIdP rch)]

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
            f :: (XCCasePattern (MplPass 'LambdaLifted), NonEmpty (m [Asm.MplAsmCom MplFrontEnd])) -> 
                m (Asm.TypeAndSpec MplFrontEnd, [Asm.Name], Asm.MplAsmComs MplFrontEnd)
            f (PSimpleConstructor ann phr args, coms) =   
                fmap 
                    ( Asm.TypeAndSpec (ann ^. _1 % typePhraseExt % typeClauseName % to toAsmIdP) (toAsmIdP phr)
                    , map (toAsmIdP . fst) args
                    ,) 
                    (fmap concat $ sequenceA coms)
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
    [(XCPlugPhrase MplLambdaLifted, ([ChP MplLambdaLifted], [ChP MplLambdaLifted]), NonEmpty (m [Asm.MplAsmCom MplFrontEnd]))] -> 
    -- | resulting plug assembly
    m (Asm.MplAsmComs MplFrontEnd)
mplAssemblePlugs plugged = 
    {- sort so that we have the input to 'goOutput' has the first element with a plugged OUTPUT channel -}
    goOutput plugged . moveStartingPlugPhraseToHead
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
    goOutput pluggeds lst = case lst of
        [outplg] -> fmap concat $ sequenceOf traversed (outplg ^. _3)
        outplg:rst -> do
            let topwiths = collectUsedChannels [outplg]
                botwiths = collectUsedChannels rst
                pluggedof = outplg ^. _2 % to (usedPlugsOf pluggeds)

            topcmds <- fmap concat $ sequenceOf traversed (outplg ^. _3)
            botcmds <- goInp (pluggeds \\ pluggedof) $ moveIntersectWithInputs pluggedof rst

            return [Asm.CPlug () (map toAsmIdP pluggedof) ((topwiths, topcmds), (botwiths, botcmds)) ]

        -- this case is impossible 
        [] -> return []

    goInp pluggeds lst  = case lst of
        [inpplg] -> fmap concat $ sequenceOf traversed (inpplg ^. _3)
        inpplg:rst -> do
            let topwiths = collectUsedChannels [inpplg]
                botwiths = collectUsedChannels rst
                pluggedof = inpplg ^. _2 % to (usedPlugsOf pluggeds)

            topcmds <- fmap concat $ sequenceOf traversed (inpplg ^. _3)
            botcmds <- goInp (pluggeds \\ pluggedof) $ moveIntersectWithOutputs pluggedof rst

            return [Asm.CPlug () (map toAsmIdP pluggedof) ((topwiths, topcmds), (botwiths, botcmds))]
            
        -- this case is  imposible
        [] -> return []

            -- CPlug (XCPlug x) [IdP x] (PlugPhrase x, PlugPhrase x)

    -- assumes that the head of the input list has an plugged INPUT channel
    
    -- collects all the used channels in a list of plugged phrases
    collectUsedChannels :: 
        [(XCPlugPhrase MplLambdaLifted, ([ChP MplLambdaLifted], [ChP MplLambdaLifted]), NonEmpty (m [Asm.MplAsmCom MplFrontEnd]))] -> 
        [Asm.IdP MplFrontEnd]
    collectUsedChannels = 
        map toAsmIdP 
        . filter (`notElem` plugged) 
        . map (view identR)
        . toListOf (folded % _2 % each % folded)

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
    m (Asm.IdP MplFrontEnd)
freshTmpName = freshUniqueSupply <&> 
    coerce 
    . ('e':) 
    . show 
    . (coerce :: Unique -> Word) 
    . uniqueFromSupply

instance Pretty (Asm.TypeAndSpec MplFrontEnd) where
    pretty (Asm.TypeAndSpec tp spec) = pretty tp <> dot <> pretty spec


