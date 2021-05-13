{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MplAST.MplProg where

import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplCmd
import MplAST.MplType
import MplAST.MplIdent

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import GHC.Generics 
import Data.Void

import Data.List.NonEmpty
import Data.Data
import Data.Kind

{- Module for data types defining an MPL program
 -
 -}

newtype MplProg x = MplProg { _prog :: [MplStmt x] }
  deriving (Semigroup, Monoid )

deriving instance (ForallDefn Show x ) => Show (MplProg x)


data MplStmt x = MplStmt {
    _stmtDefns :: NonEmpty (MplDefn x)
    , _stmtWhereBindings :: [MplStmt x] 
}

type MplSeqType x = ([TypeP x], [XMplType x], XMplType x) 
type MplConcType x = ([TypeP x], [XMplType x], [XMplType x], [XMplType x]) 
type MplChType x = ([TypeP x], XMplType x) 


deriving instance ( ForallDefn Show x ) => Show (MplStmt x)

type family XDataDefn x 
type family XCodataDefn x 
type family XCoprotocolDefn x 
type family XProtocolDefn x 
type family XProcessDefn x 
type family XFunctionDefn x 

type ForallDefn (c :: Type -> Constraint) x =
    ( ForallObjDefn c x
    , c (XFunctionDefn x)
    , c (XProcessDefn x)

    )

type ForallObjDefn (c :: Type -> Constraint) x =
    ( c (XDataDefn x)
    , c (XCodataDefn x)
    , c (XCoprotocolDefn x)
    , c (XProtocolDefn x)

    , c (MplSeqObjDefn x)
    , c (MplConcObjDefn x)
    )

data MplDefn x =
    ObjectDefn !(MplObjectDefn x)
    | FunctionDefn !(XFunctionDefn x)
    | ProcessDefn !(XProcessDefn x)

data MplObjectDefn x =
    SeqObjDefn !(MplSeqObjDefn x)
    | ConcObjDefn !(MplConcObjDefn x)
    
data MplSeqObjDefn x =
    DataDefn !(XDataDefn x) 
    | CodataDefn !(XCodataDefn x) 

data MplConcObjDefn x = 
    ProtocolDefn !(XProtocolDefn x)
    | CoprotocolDefn !(XCoprotocolDefn x)

data ObjectDefnTag =
    SeqObjTag SeqObjDefnTag
    | ConcObjTag ConcObjDefnTag 
  deriving (Show, Eq)

data SeqObjDefnTag =
    DataDefnTag 
    | CodataDefnTag
  deriving (Show, Eq)

data ConcObjDefnTag =
    ProtocolDefnTag
    | CoprotocolDefnTag
  deriving (Show, Eq)

$(concat <$> traverse makeClassyPrisms 
    [ ''ObjectDefnTag
    , ''SeqObjDefnTag
    , ''ConcObjDefnTag 
    , ''MplObjectDefn
    , ''MplSeqObjDefn 
    , ''MplConcObjDefn 
    ]
 )

instance AsSeqObjDefnTag ObjectDefnTag where
    _SeqObjDefnTag = _SeqObjTag

instance AsConcObjDefnTag ObjectDefnTag where
    _ConcObjDefnTag = _ConcObjTag

instance AsMplSeqObjDefn (MplObjectDefn x) x where
    _MplSeqObjDefn = _SeqObjDefn 

instance AsMplConcObjDefn (MplObjectDefn x) x where
    _MplConcObjDefn = _ConcObjDefn 

deriving instance (ForallDefn Show x) => Show (MplDefn x)
deriving instance (ForallObjDefn Show x) => Show (MplObjectDefn x) 
deriving instance (ForallObjDefn Show x) => Show (MplSeqObjDefn x)
deriving instance (ForallObjDefn Show x) => Show (MplConcObjDefn x)

type family XTypeClauseSpineExt x (t :: ObjectDefnTag)

data MplTypeClauseSpine x (t :: ObjectDefnTag) = MplTypeClauseSpine {
        _typeClauseSpineClauses :: NonEmpty (MplTypeClause x t)
        , _typeClauseSpineExt :: !(XTypeClauseSpineExt x t)
    }

deriving instance 
    ( Show (XTypeClauseSpineExt x t)
    , Show (MplTypeClause x t)) => Show (MplTypeClauseSpine x t)

{-# COMPLETE UMplTypeClauseSpine #-}
pattern UMplTypeClauseSpine n =  MplTypeClauseSpine n ()

type family TypeClauseArgs x (t :: ObjectDefnTag) where
    TypeClauseArgs x (SeqObjTag _) = [IdP x]
    TypeClauseArgs x (ConcObjTag _) = ([IdP x], [IdP x])

type family XTypeClauseExt x (t :: ObjectDefnTag)

type ForallTypeClause (c :: Type -> Constraint) x t =
    ( c (XTypeClauseExt x t)
    , c (TypeClauseArgs x t)
    , ForallTypePhrase c x t)

data MplTypeClause x (t :: ObjectDefnTag) = MplTypeClause {
    _typeClauseName :: IdP x
    , _typeClauseArgs :: TypeClauseArgs x t
    , _typeClauseStateVar :: IdP x
    , _typeClausePhrases :: [MplTypePhrase x t]
    , _typeClauseExt :: (XTypeClauseExt x t)
}  

deriving instance 
    ( Show (IdP x)
    , ForallTypeClause Show x t ) =>
    Show (MplTypeClause x t)

pattern UMplTypeClause a b c d =  MplTypeClause a b c d ()

type family XTypePhraseExt x (t :: ObjectDefnTag)
type family XTypePhraseFrom x (t :: ObjectDefnTag)
type family XTypePhraseTo x (t :: ObjectDefnTag)

type ForallTypePhrase (c :: Type -> Constraint) x t = 
    ( c (XTypePhraseExt x t)
    , c (XTypePhraseFrom x t)
    , c (XTypePhraseTo x t)
    )

data MplTypePhrase x (t :: ObjectDefnTag) = MplTypePhrase {
    _typePhraseName :: IdP x
    , _typePhraseFrom :: XTypePhraseFrom x t
    , _typePhraseTo :: XTypePhraseTo x t
    , _typePhraseExt :: (XTypePhraseExt x t)
}  

deriving instance (Show (IdP x), ForallTypePhrase Show x t) => Show (MplTypePhrase x t)

type family XFunType x 

data MplFunction x = MplFunction { 
    _funName :: IdP x
    , _funType :: XFunType x
    , _funDefn :: NonEmpty ([XMplPattern x], XMplExpr x) 
} 

$(makeLenses ''MplFunction)

type ForallFunction (c :: Type -> Constraint) x =
    ( c (XFunType x)
    , c (XMplPattern x)
    , c (XMplExpr x)
    )

deriving instance 
    ( Show (IdP x)
    , ForallFunction Show x
    ) => Show (MplFunction x)

type family XProcType x 

data MplProcess x = MplProcess { 
    _procName :: IdP x 
    , _procType :: XProcType x
    , _procDefn :: NonEmpty ( ([XMplPattern x], [ChP x], [ChP x]), NonEmpty (XMplCmd x)) 
}  

$(makeLenses ''MplProcess)

type ForallProcess (c :: Type -> Constraint) x = 
    ( c (XProcType x)
    , c (XMplCmd x)
    , c (XMplPattern x)) 

deriving instance 
    ( Show (IdP x)
    , Show (ChP x)
    , ForallProcess Show x) => Show (MplProcess x)



$(concat <$> traverse makeLenses 
    [ ''MplProg
    , ''MplTypeClauseSpine
    , ''MplStmt
    , ''MplDefn
    , ''MplObjectDefn
    , ''MplTypeClause
    , ''MplTypePhrase
    ]
 )

$(concat <$> traverse makePrisms  
    [ ''MplProg
    , ''MplTypeClauseSpine
    , ''MplTypeClause
    , ''MplTypePhrase
    , ''MplFunction
    , ''MplProcess
    ]
 )

instance AsMplObjectDefn (MplDefn x) x where
    _MplObjectDefn = prism' cts prj
      where
        cts = ObjectDefn
        prj (ObjectDefn n) = Just n
        prj _ = Nothing

$(makeBaseFunctor ''MplStmt)

--------------
-- Expression data type...
--------------

type family XEPOps x
type family XEVar x
type family XEInt x
type family XEChar x
type family XEDouble x
type family XECase x
type family XEObjCall x
type family XECall x
type family XERecord x
type family XERecordPhrase x

type family XEList x
type family XEString x
type family XEUnit x
type family XETuple x
type family XEBuiltInOp x

type family XEIf x
type family XELet x
type family XEFold x
type family XEFoldPhrase x
type family XEUnfoldSubPhrase x
type family XEUnfold x
type family XEUnfoldPhrase x
type family XESwitch x

type family XXExpr x

type ForallMplExpr (c :: Type -> Constraint) x =
    ( c (XMplExpr x)
    , c (XEPOps x)
    , c (XEVar x)
    , c (XEInt x)
    , c (XEChar x)
    , c (XEDouble x)
    , c (XECase x)
    , c (XEObjCall x)
    , c (XECall x)
    , c (XERecord x)
    , c (XERecordPhrase x)

    -- built in types (with their own special syntactic sugar)
    , c (XEList x)
    , c (XEString x)
    , c (XEUnit x)
    , c (XETuple x)
    , c (XEBuiltInOp x)

    -- extra structures
    , c (XEIf x)
    , c (XELet x)
    , c (XEFold x)
    , c (XEFoldPhrase x)
    , c (XEUnfoldSubPhrase x)
    , c (XEUnfold x)
    , c (XEUnfoldPhrase x)
    , c (XESwitch x)

    , c (XXExpr x)
    )

data MplExpr x =
    EPOps !(XEPOps x) PrimitiveOperators (MplExpr x) (MplExpr x)
    | EVar !(XEVar x) (IdP x)
    | EInt !(XEInt x) Int
    | EChar !(XEChar x) Char
    | EDouble !(XEDouble x) Double
    | ECase !(XECase x) (MplExpr x) (NonEmpty (XMplPattern x, MplExpr x))

    | EObjCall !(XEObjCall x) (IdP x) [MplExpr x]
    | ECall !(XECall x) (IdP x) [MplExpr x]
    | ERecord !(XERecord x) (NonEmpty (XERecordPhrase x, IdP x, ([XMplPattern x], MplExpr x)))

    -- built in...
    | EList !(XEList x) [MplExpr x]
    | EString !(XEString x) String
    | EUnit !(XEUnit x) 
    | ETuple !(XETuple x) (MplExpr x, MplExpr x, [MplExpr x])
    | EBuiltInOp !(XEBuiltInOp x) BuiltInOperators (MplExpr x) (MplExpr x)


    | EIf !(XEIf x) (MplExpr x) (MplExpr x) (MplExpr x)
    | ELet !(XELet x) (NonEmpty (MplStmt x)) (MplExpr x)
    | EFold !(XEFold x) (MplExpr x)
        (NonEmpty 
            ( XEFoldPhrase x
            , IdP x
            , [XMplPattern x]
            , (MplExpr x)
            ) 
        )
    | EUnfold 
        !(XEUnfold x) 
        (MplExpr x)
        ( NonEmpty 
            ( XEUnfoldPhrase x
            , XMplPattern x
            , NonEmpty (XEUnfoldSubPhrase x, IdP x, [XMplPattern x], (MplExpr x))
            )
        )
    | ESwitch !(XESwitch x) (NonEmpty ((MplExpr x), (MplExpr x)))

    | XExpr !(XXExpr x)

    
$(makeClassyPrisms ''MplExpr)
$(makeBaseFunctor ''MplExpr)

deriving instance 
    ( Show (IdP x)
    , Show (MplStmt x)
    , Show (XMplPattern x)
    , ForallMplExpr Show x
    ) => Show (MplExpr x)

pattern UEPOps op a b <- EPOps () op a b
  where
    UEPOps op a b = EPOps () op a b

pattern UEVar a <- EVar () a
  where
    UEVar a = EVar () a

pattern UEInt n <- EInt () n
  where
    UEInt n = EInt () n

pattern UEChar n <- EChar () n
  where
    UEChar n = EChar () n

pattern UEDouble n <- EDouble () n
  where
    UEDouble n = EDouble () n

pattern UECase caseon cases <- ECase () caseon cases
  where
    UECase caseon cases = ECase () caseon cases

pattern UECall id args <- ECall () id args
  where
    UECall id args = ECall () id args

pattern UERecord phrases <- 
    ERecord () (fmap (\((),b,c) -> (b,c)) -> phrases)
  where
    UERecord phrases = 
        ERecord () $ fmap (\(b,c) -> ((), b, c)) 
            phrases

--------------
-- Expression data type...
--------------
