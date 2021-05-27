{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module MplAST.MplCmd where

import Optics.TH
import Optics.Prism
import Optics.Operators
import Data.Functor.Foldable.TH

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE 

import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplIdent

import GHC.Generics
import Data.Data
import Data.Typeable
import Data.Kind

{- Type for the mpl concurrent commands...
 -}


type family ChP x
type family XMplCmd x

-- Extensions
type family XCRun x
type family XCClose x
type family XCHalt x
type family XCGet x
type family XCPut x
type family XCHCase x
type family XCHPut x
type family XCSplit x
type family XCFork x
type family XCId x
type family XCIdNeg x
type family XCRace x
type family XCPlug x
type family XCPlugs x
type family XCCase x
type family XCCasePattern x
type family XCSwitch x
type family XCIf x
type family XCIllegalInstr x

type family XXCmd x


-- extensions for phrases. We need the type family
-- dependency to write a type class to get the context 
-- out of the extension for printing the context
type family XCHCasePhrase x 
type family XCForkPhrase x 
type family XCPlugPhrase x 

-- aliases for some of the phrases
type CPlugPhrase x = (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty (MplCmd x))

-- aliases for some of the phrases
type ForallProcessCommand (c :: Type -> Constraint) x =
    ( c (IdP x)
    , c (ChP x)

    , c (XMplExpr x)
    , c (XMplPattern x)

    , c (XMplCmd x)
    , c (XCRun x)
    , c (XCClose x)
    , c (XCHalt x)
    , c (XCGet x)
    , c (XCPut x)
    , c (XCHCase x)
    , c (XCHPut x)
    , c (XCSplit x)
    , c (XCFork x)
    , c (XCId x)
    , c (XCIdNeg x)
    , c (XCRace x)
    , c (XCPlug x)
    , c (XCPlugs x)
    , c (XCCase x)
    , c (XCCasePattern x)
    , c (XCSwitch x)
    , c (XCIf x)

    , c (XXCmd x)

    , c (XCHCasePhrase x)
    , c (XCForkPhrase x)
    , c (XCPlugPhrase x)
    , c (XCIllegalInstr x)
    )
type CForkPhrase x = 
    (ChP x, XCForkPhrase x, NonEmpty (MplCmd x))

-- data ProcessCommand pattern letdef typedef seqcalleddef conccalleddef ident chident =
data MplCmd x =
    CRun !(XCRun x) (IdP x) [XMplExpr x] [ChP x] [ChP x] 
        -- called, seq args, input chs, outchs 
    | CClose !(XCClose x) (ChP x )
    | CHalt !(XCHalt x) (ChP x)

    | CGet !(XCGet x) (XMplPattern x) (ChP x)
    | CPut !(XCPut x) (XMplExpr x) (ChP x)

    | CHCase 
        !(XCHCase x)
        (ChP x)
        (NonEmpty (XCHCasePhrase x, IdP x, NonEmpty (MplCmd x)))
        {-
        (NonEmpty 
            ( ident
            , conccalleddef
            , ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident))
            -}
    | CHPut  
        !(XCHPut x) (IdP x) (ChP x)
            -- hput (Co)Protocol on channel
        
        -- { _cHPut :: ident, _cHPutDef :: conccalleddef , _cHPutCh :: chident }

    | CSplit !(XCSplit x) (ChP x) (ChP x, ChP x)
    -- { _cSplit :: chident, _cSplitInto :: (chident, chident) }
    | CFork !(XCFork x) 
        (ChP x) 
            ( CForkPhrase x , CForkPhrase x )
        {-
        { 
        _cFork :: chident
        , _cForkInto :: 
        ( (chident, [chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)
        , (chident, [chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) ) }
        -}

    | CId !(XCId x) (ChP x, ChP x) -- { _cIdLarg :: chident, _cIdRarg :: chident}
    | CIdNeg !(XCIdNeg x) (ChP x, ChP x) -- { _cIdLarg :: chident, _cIdNegArg :: chident}
    
    | CRace !(XCRace x) 
        (NonEmpty (ChP x, NonEmpty (MplCmd x)))
        -- { _cRaces :: NonEmpty (chident, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) }

    | CPlug !(XCPlug x) (CPlugPhrase x, CPlugPhrase x)
    | CPlugs !(XCPlugs x) (CPlugPhrase x, CPlugPhrase x, [CPlugPhrase x])
        {-
        -- | plugged together
        [chident] 
        -- | plug phrases
        ([chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) 
        ([chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)
        [([chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)]
        -}

    | CCase 
        !(XCCase x) 
        (XMplExpr x) 
        (NonEmpty (XCCasePattern x, NonEmpty (MplCmd x)))
        {-
        { _cCase :: Expr pattern letdef typedef seqcalleddef ident
        , _cCases :: [(pattern, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)] }
        -}
    | CSwitch !(XCSwitch x) (NonEmpty (XMplExpr x, NonEmpty (MplCmd x)))
    | CIf !(XCIf x) (XMplExpr x) (NonEmpty (MplCmd x)) (NonEmpty (MplCmd x))
    | CIllegalInstr (XCIllegalInstr x)
        -- { _cSwitches :: NonEmpty (Expr pattern letdef typedef seqcalleddef ident, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) }

deriving instance ( ForallProcessCommand Show x ) => Show (MplCmd x)
deriving instance ( ForallProcessCommand Eq x ) => Eq (MplCmd x)
    

$(concat <$> traverse makePrisms 
    [ ''MplCmd ]
 )

pattern UCRun call seq inchs outchs = CRun () call seq inchs outchs
pattern UCClose ch = CClose () ch
pattern UCHalt ch = CHalt () ch
pattern UCGet a b = CGet () a b
pattern UCPut a b = CPut () a b
pattern UCHCase ch phrases <- CHCase () ch (fmap (\((),b,c) -> (b,c)) -> phrases)
  where
    UCHCase ch phrases = CHCase () ch $ fmap (\(b,c) -> ((),b,c)) phrases
pattern UCHPut a b = CHPut () a b

pattern UCFork ch phrases <- CFork () ch ((\((a, (), ast), (b, (), bst)) -> ((a,ast),(b,bst))) -> phrases) 
  where
    UCFork ch ((a,ast), (b,bst)) = CFork () ch ((a,(),ast), (b,(),bst))

pattern UCId ab = CId () ab
pattern UCIdNeg ab = CIdNeg () ab

pattern UCRace phrases = CRace () phrases

{-
pattern UCPlugs abcs <- CPlugs () ( (\(((), a), ((), b), cs) -> (a,b, map snd cs)) -> abcs)
  where
    UCPlugs (a,b,cs) = CPlugs () (((),a), ((),b), fmap ((),) cs)
    -}

pattern UCCase expr phrases = CCase () expr phrases
pattern UCSwitch phrases = CSwitch () phrases

$(makeBaseFunctor ''MplCmd)
    
