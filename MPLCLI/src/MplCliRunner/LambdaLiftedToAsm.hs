{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MplCliRunner.LambdaLiftedToAsm where

-- Front end
import qualified MplPasses.Passes as Passes
import qualified MplPasses.Parser.BnfcParse as B
import qualified MplPasses.PassesErrors as PassesErrors
import qualified MplPasses.PassesErrorsPprint as PassesErrors

import MplAST.MplCore 
import MplAST.MplTypeChecked 
import MplUtil.UniqueSupply

import qualified MplAsmAST.MplAsmProg as Asm
import qualified MplAsmAST.MplAsmCore as Asm

import Control.Monad.State

import Optics
import Data.Coerce
import Data.List
import Control.Arrow
import Control.Exception
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty (..) )

import Data.Traversable
import Data.Functor.Foldable

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


class ToAsmIdP t where
    toAsmIdP :: t -> Asm.IdP MplFrontEnd

instance ToAsmIdP IdentT where
    toAsmIdP identt = coerce $ concat
        [ identt ^. name % coerced 
        , show (coerce (identt ^. identRUniqueTag) :: Word)
        ]

mplAssembleStmt ::
    forall s m.
    ( HasUniqueSupply s
    , MonadState s m ) =>
    MplStmt MplLambdaLifted ->
    m [Asm.MplAsmStmt MplFrontEnd]
mplAssembleStmt stmt = 
    sequenceA
    [ pure $ Asm.Constructors (concat $ mapMaybe constructorStmts collectedDefns)
    , pure $ Asm.Destructors (concat $ mapMaybe destructorStmts collectedDefns)
    , pure $ Asm.Protocols (concat $ mapMaybe protocolStmts collectedDefns)
    , pure $ Asm.Coprotocols (concat $ mapMaybe coprotocolStmts collectedDefns)
    , fmap Asm.Functions $ sequenceA $ mapMaybe funStmts collectedDefns
    , fmap Asm.Processes $ sequenceA $ mapMaybe procStmts collectedDefns
    ]
  where
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
                ( undefined
                , (undefined, undefined, undefined)
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
        CRunF _ callid seqs ins outs -> undefined
    {-
    = MplAST.MplCmd.CRunF !(MplAST.MplCmd.XCRun x)
                        (MplAST.MplCore.IdP x)
                        [XMplExpr x]
                        [ChP x]
                        [ChP x]
  | MplAST.MplCmd.CCloseF !(MplAST.MplCmd.XCClose x) (ChP x)
  | MplAST.MplCmd.CHaltF !(MplAST.MplCmd.XCHalt x) (ChP x)
  | MplAST.MplCmd.CGetF !(MplAST.MplCmd.XCGet x)
                        (XMplPattern x)
                        (ChP x)
  | MplAST.MplCmd.CPutF !(MplAST.MplCmd.XCPut x) (XMplExpr x) (ChP x)
  | MplAST.MplCmd.CHCaseF !(MplAST.MplCmd.XCHCase x)
                          (ChP x)
                          (NonEmpty (XCHCasePhrase x, MplAST.MplCore.IdP x, NonEmpty r))
  | MplAST.MplCmd.CHPutF !(MplAST.MplCmd.XCHPut x)
                         (MplAST.MplCore.IdP x)
                         (ChP x)
  | MplAST.MplCmd.CSplitF !(MplAST.MplCmd.XCSplit x)
                          (ChP x)
                          (ChP x, ChP x)
  | MplAST.MplCmd.CForkF !(MplAST.MplCmd.XCFork x)
                         (ChP x)
                         ((ChP x, XCForkPhrase x, NonEmpty r),
                          (ChP x, XCForkPhrase x, NonEmpty r))
  | MplAST.MplCmd.CIdF !(MplAST.MplCmd.XCId x) (ChP x, ChP x)
  | CIdNegF !(XCIdNeg x) (ChP x, ChP x)
  | MplAST.MplCmd.CRaceF !(MplAST.MplCmd.XCRace x)
                         (NonEmpty (ChP x, NonEmpty r))
  | MplAST.MplCmd.CPlugF !(MplAST.MplCmd.XCPlug x)
                         ((XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r),
                          (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r))
  | CPlugsF !(XCPlugs x)
            ((XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r),
             (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r),
             [(XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r)])
  | MplAST.MplCmd.CCaseF !(MplAST.MplCmd.XCCase x)
                         (XMplExpr x)
                         (NonEmpty (XCCasePattern x, NonEmpty r))
  | CSwitchF !(XCSwitch x) (NonEmpty (XMplExpr x, NonEmpty r))
  | MplAST.MplCmd.CIfF !(MplAST.MplCmd.XCIf x)
                       (XMplExpr x)
                       (NonEmpty r)
                       (NonEmpty r)
  | CIllegalInstrF !(XCIllegalInstr x)
  -}

-- | generates a fresh name. This is mainly used for generating expressions.
freshTmpName ::
    ( HasUniqueSupply s
    , MonadState s m ) =>
    m (Asm.IdP MplFrontEnd)
freshTmpName = fmap (coerce . ("e"++) . show . (coerce :: Unique -> Word) . uniqueFromSupply) freshUniqueSupply

