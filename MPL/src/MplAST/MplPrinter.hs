{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MplAST.MplPrinter where

import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplCmd
import MplAST.MplType
import MplAST.MplProg
import MplAST.MplIdent
import MplAST.MplRenamed
import MplAST.MplParsed
import MplAST.MplTypeChecked
import MplAST.MplExt

import MplUtil.UniqueSupply

import qualified MplLanguage.AbsMPL as B
import qualified MplLanguage.ErrM as B
import qualified MplLanguage.LayoutMPL as B
import qualified MplLanguage.LexMPL as B
import qualified MplLanguage.ParMPL as B
import qualified MplLanguage.PrintMPL as B

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Data
import Data.Kind
import Data.Bool
import Data.Maybe
import Control.Arrow
import Data.Kind
import Data.Void
import Data.Proxy

import Debug.Trace

{- Module for a pretty printer of the AST (useful for debugging)
 - Note this just translates back to the BNFC representation and uses
 - BNFC to print the AST.
 -}

tracePprint :: PPrint a MplRenamed => a -> a
tracePprint n = trace (pprint (Proxy :: Proxy MplRenamed) n) n

class PPrint a x where
    pprint :: Proxy x -> a -> String

instance PPrint Int x where
    pprint _ n = show n

instance PPrint Double x where
    pprint _ n = show n

instance PPrint Char x where
    pprint _ n = show n

instance PPrint String x where
    pprint _ = id

instance PPrint UniqueTag x where
    pprint _ (UniqueTag (Unique n)) = show n

instance PPrint Polarity x where
    pprint _ Input = "INP"
    pprint _ Output = "OUT"

instance {-# OVERLAPPABLE #-} PPrint IdentP x where
    pprint _ n = case n ^. name of Name str -> str

instance {-# OVERLAPPABLE #-} PPrint IdentR x where
    pprint proxy n = n ^. identRIdentP % to (pprint proxy) ++ "__" ++ n ^. uniqueTag % to (pprint proxy)

instance {-# OVERLAPPING #-} PPrint IdentR MplParsed where
    pprint proxy n = n ^. identRIdentP % to (pprint proxy)

instance {-# OVERLAPPING #-} PPrint IdentR MplRenamed where
    pprint proxy n = n ^. identRIdentP % to (pprint proxy) ++ "__" ++ n ^. uniqueTag % to (pprint proxy)

instance PPrint TypeT x where
    pprint proxy (NamedType identr) = pprint proxy identr 
    pprint proxy (GenNamedType p) = "T" ++ pprint proxy p

instance {-# OVERLAPPABLE #-} PPrint ChIdentR x where
    pprint proxy n = n ^. chIdentRIdentR % to (pprint proxy) ++ "__" ++ n ^. polarity % to (pprint proxy)

instance {-# OVERLAPPING #-} PPrint ChIdentR MplParsed where
    pprint proxy n = n ^. chIdentRIdentR % to (pprint proxy)

instance {-# OVERLAPPING #-} PPrint ChIdentR MplRenamed where
    pprint proxy n = n ^. chIdentRIdentR % to (pprint proxy) ++ "__" ++ n ^. polarity % to (pprint proxy)


instance PPrint PrimitiveOperators x where
    pprint _ = go
      where
        go = \case
            PrimitiveAdd -> "+"
            PrimitiveSub -> "-"
            PrimitiveMul -> "*"
            PrimitiveDiv -> "/"
            PrimitiveEq -> "=="
            PrimitiveNeq -> "/="
            PrimitiveLt -> "<"  
            PrimitiveGt -> ">"
            PrimitiveLeq -> "<="
            PrimitiveGeq -> ">="
            PrimitiveColon -> ":"
            n -> error $ "error in print -- operator not implemented yet: " ++ show n

{- | For converting to a bnfc ident (this heavily relies on the pprint instances above)-}
class ToBnfcIdent t x where
    toBnfcIdent :: PPrint a x => Proxy x -> a -> t

instance ToBnfcIdent B.PIdent x where
    toBnfcIdent proxy n =  B.PIdent ((-1,-1), pprint proxy n)

instance ToBnfcIdent B.PDouble x where
    toBnfcIdent proxy n =  B.PDouble ((-1,-1), pprint proxy n)

instance ToBnfcIdent B.PChar x where
    toBnfcIdent proxy n =  B.PChar ((-1,-1), pprint proxy n)

instance ToBnfcIdent B.UIdent x where
    toBnfcIdent proxy n =  B.UIdent ((-1,-1), pprint proxy n)

instance ToBnfcIdent B.UPIdent x where
    toBnfcIdent proxy n =  B.UPIdent ((-1,-1), pprint proxy n)

instance ToBnfcIdent B.SplitChannel x where
    toBnfcIdent proxy n =  B.SPLIT_CHANNEL $ toBnfcIdent proxy n

instance ToBnfcIdent B.PInteger x where
    toBnfcIdent proxy n =  B.PInteger ((-1,-1), pprint proxy n)

instance ToBnfcIdent B.PString x where
    toBnfcIdent proxy n =  B.PString ((-1,-1), pprint proxy n)

instance ToBnfcIdent B.ForkChannel x where
    toBnfcIdent proxy n =  B.FORK_CHANNEL $ toBnfcIdent proxy n

instance ToBnfcIdent B.ForallVarList x where
    toBnfcIdent proxy n =  B.MPL_SEQ_FUN_TYPE_FORALL_LIST $ toBnfcIdent proxy n

instance ToBnfcIdent B.TypeHandleName x where
    toBnfcIdent proxy n =  B.TYPE_HANDLE_NAME $ toBnfcIdent proxy n

instance ToBnfcIdent B.Colon x where
    toBnfcIdent proxy n =  B.Colon ((-1,-1), pprint proxy n) 

instance ToBnfcIdent B.Infixl3op x where
    toBnfcIdent proxy n =  B.Infixl3op ((-1,-1), pprint proxy n) 

instance ToBnfcIdent B.Infixl5op x where
    toBnfcIdent proxy n =  B.Infixl5op ((-1,-1), pprint proxy n) 

instance ToBnfcIdent B.Infixl6op x where
    toBnfcIdent proxy n =  B.Infixl6op ((-1,-1), pprint proxy n) 

{- For helping keywords-}
class BnfcKeyword t where
    bnfcKeyword :: t

instance BnfcKeyword B.Case where
    bnfcKeyword = B.Case ((-1,-1), "case")

instance BnfcKeyword B.LBracket where
    bnfcKeyword = B.LBracket ((-1,-1), "(")

instance BnfcKeyword B.RBracket where
    bnfcKeyword = B.RBracket ((-1,-1), ")")

instance BnfcKeyword B.LSquareBracket where
    bnfcKeyword = B.LSquareBracket ((-1,-1), "[")

instance BnfcKeyword B.RSquareBracket where
    bnfcKeyword = B.RSquareBracket ((-1,-1), "]")

instance BnfcKeyword B.NullPattern where
    bnfcKeyword = B.NullPattern ((-1,-1), "_")

instance BnfcKeyword B.Close where
    bnfcKeyword = B.Close ((-1,-1), "close")

instance BnfcKeyword B.Halt where
    bnfcKeyword = B.Halt ((-1,-1), "halt")

instance BnfcKeyword B.Get where
    bnfcKeyword = B.Get ((-1,-1), "get")

instance BnfcKeyword B.Put where
    bnfcKeyword = B.Put ((-1,-1), "put")

instance BnfcKeyword B.HCase where
    bnfcKeyword = B.HCase ((-1,-1), "hcase")

instance BnfcKeyword B.HPut where
    bnfcKeyword = B.HPut ((-1,-1), "hput")

instance BnfcKeyword B.Split where
    bnfcKeyword = B.Split ((-1,-1), "split")

instance BnfcKeyword B.Fork where
    bnfcKeyword = B.Fork ((-1,-1), "fork")

instance BnfcKeyword B.ChId where
    bnfcKeyword = B.ChId ((-1,-1), "|=|")

instance BnfcKeyword B.Par where
    bnfcKeyword = B.Par ((-1,-1), "(+)")

instance BnfcKeyword B.Tensor where
    bnfcKeyword = B.Tensor ((-1,-1), "(*)")

instance BnfcKeyword B.Colon where
    bnfcKeyword = B.Colon ((-1,-1), ":")

class MplTypeToBnfc t x where
    mplTypeToBnfc :: Proxy x -> t -> B.MplType

instance MplTypeToBnfc IdentR x where
    mplTypeToBnfc proxy i = mplTypeToBnfc proxy (_TypeVar # ((), i) :: MplType MplRenamed)

instance MplTypeToBnfc IdentP x where
    mplTypeToBnfc proxy i = mplTypeToBnfc proxy (_TypeVar # ((), i) :: MplType MplParsed)


instance ( PPrint (IdP x) t, PPrint (TypeP x) t) => MplTypeToBnfc (MplType x) t where
    mplTypeToBnfc proxy = f
      where
        f (TypeVar cxt tp) = B.MPL_UIDENT_NO_ARGS_TYPE $ toBnfcIdent proxy tp
        f (TypeWithNoArgs cxt tp) = B.MPL_UIDENT_NO_ARGS_TYPE (toBnfcIdent proxy tp)

        f (TypeSeqWithArgs cxt tp args) = 
            B.MPL_UIDENT_ARGS_TYPE (toBnfcIdent proxy tp) bnfcKeyword (map f args) bnfcKeyword
        f (TypeSeqVarWithArgs cxt tp args) = 
            B.MPL_UIDENT_ARGS_TYPE (
                -- (\case (B.UIdent (pos, str)) -> B.UIdent (pos, str ++ "TVAR") )
                (\case (B.UIdent (pos, str)) -> B.UIdent (pos, str) )
                (toBnfcIdent proxy tp :: B.UIdent)) 
                bnfcKeyword (map f args) bnfcKeyword
        f (TypeConcWithArgs cxt tp (seqs, chs)) = 
            B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE (toBnfcIdent proxy tp) bnfcKeyword (map f seqs) (map f chs) bnfcKeyword
        f (TypeConcVarWithArgs cxt tp (seqs, chs)) = 
            B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE (toBnfcIdent proxy tp) bnfcKeyword (map f seqs) (map f chs) bnfcKeyword
        f (TypeBuiltIn n) = case n of
            TypeSeqArrF cxt froms to ->
                B.MPL_SEQ_ARROW_TYPE [] (NE.toList $ fmap f froms) (f to)
            TypeConcArrF cxt seqs froms tos -> 
                B.MPL_CONC_ARROW_TYPE [] (map f seqs) (map f froms) (map f tos)

            TypeParF cxt l@(TypeBuiltIn (TypeTensorF _ _ _)) r@(TypeBuiltIn (TypeTensorF _ _ _)) -> 
                B.PAR_TYPE 
                    (B.MPL_BRACKETED_TYPE bnfcKeyword (f l) bnfcKeyword) 
                        bnfcKeyword 
                    (B.MPL_BRACKETED_TYPE bnfcKeyword (f r) bnfcKeyword) 
            TypeParF cxt l r@(TypeBuiltIn (TypeTensorF _ _ _)) -> 
                B.PAR_TYPE 
                    (f l) 
                        bnfcKeyword 
                    (B.MPL_BRACKETED_TYPE bnfcKeyword (f r) bnfcKeyword) 
            TypeParF cxt l@(TypeBuiltIn (TypeTensorF _ _ _)) r -> 
                B.PAR_TYPE 
                    (B.MPL_BRACKETED_TYPE bnfcKeyword (f l) bnfcKeyword) 
                        bnfcKeyword 
                    (f r) 

            TypeParF cxt l r -> 
                B.PAR_TYPE (f l) bnfcKeyword (f r)
            TypeTensorF cxt l r -> 
                B.TENSOR_TYPE (f l) bnfcKeyword (f r)

            TypeGetF cxt seq conc ->
                B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE 
                    (toBnfcIdent proxy "Get") 
                    bnfcKeyword 
                    [f seq]
                    [f conc]
                    bnfcKeyword
            TypePutF cxt seq conc ->
                B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE 
                    (toBnfcIdent proxy "Put") 
                    bnfcKeyword 
                    [f seq]
                    [f conc]
                    bnfcKeyword
            TypeTopBotF cxt -> B.MPL_UIDENT_NO_ARGS_TYPE (toBnfcIdent proxy "TopBot")
            TypeNegF cxt tp -> B.MPL_UIDENT_ARGS_TYPE (toBnfcIdent proxy "Neg") bnfcKeyword [f tp] bnfcKeyword

            TypeIntF _cxt -> B.MPL_UIDENT_NO_ARGS_TYPE $ toBnfcIdent proxy "Int"
            TypeDoubleF _cxt -> B.MPL_UIDENT_NO_ARGS_TYPE $ toBnfcIdent proxy "Double"
            TypeCharF _cxt -> B.MPL_UIDENT_NO_ARGS_TYPE $ toBnfcIdent proxy "Char"
            TypeBoolF _cxt -> B.MPL_UIDENT_NO_ARGS_TYPE $ toBnfcIdent proxy "Bool"
            TypeUnitF _cxt -> B.MPL_UNIT_TYPE bnfcKeyword bnfcKeyword
            TypeListF _cxt rst -> B.MPL_LIST_TYPE bnfcKeyword (f rst) bnfcKeyword 

            TypeTupleF cxt (t0, t1, ts) -> 
                B.MPL_TUPLE_TYPE 
                    bnfcKeyword
                    (f t0)
                    (map (B.TUPLE_LIST_TYPE . f) (t1 : ts))
                    bnfcKeyword


class UserProvidedTypeToBnfc t x where
    userProvidedTypeToBnfc :: Proxy x -> t -> Maybe B.MplType

instance UserProvidedTypeToBnfc (Maybe ([MplType MplParsed], [MplType MplParsed], [MplType MplParsed])) MplParsed where
    userProvidedTypeToBnfc proxy Nothing = Nothing
    userProvidedTypeToBnfc proxy (Just (seqs, ins, outs)) = Just $ 
        B.MPL_CONC_ARROW_TYPE [] (map (mplTypeToBnfc proxy) seqs) (map (mplTypeToBnfc proxy) ins) (map (mplTypeToBnfc proxy) outs)

instance UserProvidedTypeToBnfc (Maybe ([MplType MplParsed], MplType MplParsed)) x where
    userProvidedTypeToBnfc proxy Nothing = Nothing
    userProvidedTypeToBnfc proxy (Just (froms, to)) = Just $
        B.MPL_SEQ_ARROW_TYPE [] (map (mplTypeToBnfc proxy) froms) (mplTypeToBnfc proxy to) 

instance PPrint ident x => UserProvidedTypeToBnfc 
    (Maybe ([ident], [MplType MplRenamed], [MplType MplRenamed], [MplType MplRenamed])) x where

    userProvidedTypeToBnfc proxy Nothing = Nothing
    userProvidedTypeToBnfc proxy (Just (foralls, seqs, ins, outs)) = Just $ 
        B.MPL_CONC_ARROW_TYPE (
            map (toBnfcIdent proxy) foralls) 
            (map (mplTypeToBnfc proxy) seqs) 
            (map (mplTypeToBnfc proxy) ins) 
            (map (mplTypeToBnfc proxy) outs)
        
instance PPrint ident x => 
    UserProvidedTypeToBnfc (Maybe ([ident], [MplType MplRenamed], MplType MplRenamed)) x where
    userProvidedTypeToBnfc proxy Nothing = Nothing
    userProvidedTypeToBnfc proxy (Just (foralls, froms, to)) = Just $ 
        B.MPL_SEQ_ARROW_TYPE 
        (map (toBnfcIdent proxy) foralls) 
        (map (mplTypeToBnfc proxy) froms) 
        (mplTypeToBnfc proxy to) 


instance PPrint ident x => UserProvidedTypeToBnfc ([ident], [MplType MplTypeChecked], MplType MplTypeChecked) x where
    userProvidedTypeToBnfc proxy (foralls, froms, to) = Just $
        B.MPL_SEQ_ARROW_TYPE 
            (map (toBnfcIdent proxy) foralls) 
            (map (mplTypeToBnfc proxy) froms) 
            (mplTypeToBnfc proxy to) 

instance PPrint ident x => UserProvidedTypeToBnfc ([ident], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked]) x where
    userProvidedTypeToBnfc proxy (foralls, seqs, ins, outs) = Just $ 
        B.MPL_CONC_ARROW_TYPE 
            (map (toBnfcIdent proxy) foralls) 
            (map (mplTypeToBnfc proxy)seqs) 
            (map (mplTypeToBnfc proxy)ins) 
            (map (mplTypeToBnfc proxy)outs)

class MplTypesToBnfc t x where
    mplTypesToBnfc :: Proxy x -> t -> [B.MplType]

-- not totally sure why we need this instance?
instance MplTypesToBnfc (MplType MplRenamed) x where
    mplTypesToBnfc proxy = pure . mplTypeToBnfc proxy

instance MplTypesToBnfc (MplType MplTypeChecked) x where
    mplTypesToBnfc proxy = pure . mplTypeToBnfc proxy

instance MplTypesToBnfc [MplType MplParsed] x where
    mplTypesToBnfc proxy = map (mplTypeToBnfc proxy)

instance MplTypesToBnfc [MplType MplRenamed] x where
    mplTypesToBnfc proxy = map (mplTypeToBnfc proxy)

instance MplTypesToBnfc [MplType MplTypeChecked] x where
    mplTypesToBnfc proxy = map (mplTypeToBnfc proxy)

instance MplTypesToBnfc ([MplType MplRenamed], MplType MplRenamed) x where
    mplTypesToBnfc proxy (as, a) = map (mplTypeToBnfc proxy) (as++[a])

instance MplTypesToBnfc ([MplType MplTypeChecked], MplType MplTypeChecked) x where
    mplTypesToBnfc proxy (as, a) = map (mplTypeToBnfc proxy) (as++[a])

instance MplTypesToBnfc ([MplType MplRenamed], IdentR) x where
    mplTypesToBnfc proxy (as, a) = map (mplTypeToBnfc proxy) (as++ [_TypeVar # ((),a)])

instance MplTypesToBnfc ([MplType MplParsed], IdentP) x where
    mplTypesToBnfc proxy (as, a) = map (mplTypeToBnfc proxy) (as++ [_TypeVar # ((),a)])


{-
instance MplTypesToBnfc ([MplType MplRenamed], MplType MplRenamed) x where
    mplTypesToBnfc proxy (as, a) = map (mplTypeToBnfc proxy) (as++[a])
-}

    
instance MplTypesToBnfc IdentR x where
    mplTypesToBnfc proxy a = [mplTypeToBnfc proxy (_TypeVar # ((), a) :: MplType MplRenamed)]

{- | Convert a pattern to a bnfc -}
class MplPattToBnfc t x where
    mplPattToBnfc ::  Proxy x -> t -> B.Pattern

instance 
    ( PPrint (IdP x) y 
    , MplSimpleConstructorArgsToBnfc (XPSimpleConstructorArgs x) y
    , MplPattToBnfc (XCCasePattern x) y
    ) => MplPattToBnfc (MplPattern x) y where
    mplPattToBnfc proxy = f
      where
        f (PVar _ id) = B.VAR_PATTERN $ toBnfcIdent proxy id
        f (PChar _ id) = B.CHAR_PATTERN $ toBnfcIdent proxy id
        f (PInt _ id) = B.INT_PATTERN $ toBnfcIdent proxy id
        f (PUnit _) = B.UNIT_PATTERN bnfcKeyword bnfcKeyword
        f (PList _ lst) = B.LIST_PATTERN bnfcKeyword (map f lst) bnfcKeyword
        f (PListCons _ a b) = B.LIST_COLON_PATTERN (f a) bnfcKeyword (f b)

        f (PSimpleListCons _ a b) = B.LIST_COLON_PATTERN (B.VAR_PATTERN $ toBnfcIdent proxy a) bnfcKeyword (B.VAR_PATTERN $ toBnfcIdent proxy b)
        f (PSimpleListEmpty _) = B.LIST_PATTERN bnfcKeyword [] bnfcKeyword
        f (PSimpleUnit _) = B.UNIT_PATTERN bnfcKeyword bnfcKeyword

        f (PString _ str) = B.STR_PATTERN $ toBnfcIdent proxy $ show str

        f (PConstructor _ id args) = 
            B.CONSTRUCTOR_PATTERN_ARGS (toBnfcIdent proxy id) bnfcKeyword (map f args) bnfcKeyword 
        f (PSimpleConstructor _ id args) =  
            B.CONSTRUCTOR_PATTERN_ARGS (toBnfcIdent proxy id) 
                bnfcKeyword 
                    (mplSimpleConstructorArgsToBnfc proxy args )
                bnfcKeyword 
            -- B.CONSTRUCTOR_PATTERN_ARGS (toBnfcIdent proxy id) bnfcKeyword (map f args) bnfcKeyword

        f (PRecord _ args) = 
            B.RECORD_PATTERN bnfcKeyword (NE.toList $ fmap g args) bnfcKeyword
          where
            g (_, id, patt) = B.DESTRUCTOR_PATTERN_PHRASE (toBnfcIdent proxy id) (f patt)
        f (PNull _) = B.NULL_PATTERN bnfcKeyword

        f (PTuple _ (t0,t1,ts)) = 
            B.TUPLE_PATTERN bnfcKeyword (f t0) (map (B.TUPLE_LIST_PATTERN . f) $ t1:ts) bnfcKeyword

        f (PBool _ id) = 
            B.CONSTRUCTOR_PATTERN_ARGS (toBnfcIdent proxy $ show id) bnfcKeyword [] bnfcKeyword 

{- | Convert a pattern from a simple consturctor args to a bnfc -}
class MplSimpleConstructorArgsToBnfc t x where
    mplSimpleConstructorArgsToBnfc ::  Proxy x -> t -> [B.Pattern]

instance MplSimpleConstructorArgsToBnfc Void x where
    mplSimpleConstructorArgsToBnfc proxy _ = [] 

-- little bit of awkardness that this is essentially hard coded for the pattern compilation case to print it
instance 
    ( PPrint x y ) => MplSimpleConstructorArgsToBnfc [(x, z)] y where
    mplSimpleConstructorArgsToBnfc proxy args = 
        map (B.VAR_PATTERN . toBnfcIdent proxy . fst) args


{- | Convert a phrase to a bnfc -}
class MplPhraseToBnfc x t res y | t -> res where
    mplPhraseToBnfc :: Proxy y -> MplTypePhrase x t -> res

instance 
    ( PPrint (IdP x) y
    , MplTypesToBnfc (XTypePhraseFrom x ('SeqObjTag 'DataDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('SeqObjTag 'DataDefnTag)) y ) => 
    MplPhraseToBnfc x (SeqObjTag DataDefnTag) B.SeqTypePhraseDefn y where
    mplPhraseToBnfc proxy phrase = 
        B.SEQ_TYPE_PHRASE [phrase ^. typePhraseName % to (toBnfcIdent proxy)] 
            (phrase ^. typePhraseFrom % to (mplTypesToBnfc proxy)) 
            (phrase ^. typePhraseTo % to (mplTypeToBnfc proxy))

instance 
    ( PPrint (IdP x) y
    , MplTypesToBnfc (XTypePhraseFrom x ('SeqObjTag 'CodataDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('SeqObjTag 'CodataDefnTag)) y) =>
    MplPhraseToBnfc x (SeqObjTag CodataDefnTag) B.SeqTypePhraseDefn y where
    mplPhraseToBnfc proxy phrase = 
        B.SEQ_TYPE_PHRASE [phrase ^. typePhraseName % to (toBnfcIdent proxy)] 
            (phrase ^. typePhraseFrom % to (mplTypesToBnfc proxy)) 
            (phrase ^. typePhraseTo % to (mplTypeToBnfc proxy))

instance 
    ( PPrint (IdP x) y
    , MplTypeToBnfc (XTypePhraseFrom x ('ConcObjTag 'ProtocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('ConcObjTag 'ProtocolDefnTag)) y ) =>
    MplPhraseToBnfc x (ConcObjTag ProtocolDefnTag) B.ConcurrentTypePhraseDefn y where
    mplPhraseToBnfc proxy phrase = 
        B.CONCURRENT_TYPE_PHRASE [phrase ^. typePhraseName % to (toBnfcIdent proxy)]
            (phrase ^. typePhraseFrom % to (mplTypeToBnfc proxy)) 
            (phrase ^. typePhraseTo % to (mplTypeToBnfc proxy))
            
instance 
    ( PPrint (IdP x) y
    , MplTypeToBnfc (XTypePhraseFrom x ('ConcObjTag 'CoprotocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('ConcObjTag 'CoprotocolDefnTag)) y ) =>
    MplPhraseToBnfc x (ConcObjTag CoprotocolDefnTag) B.ConcurrentTypePhraseDefn y where
    mplPhraseToBnfc proxy phrase = 
        B.CONCURRENT_TYPE_PHRASE [phrase ^. typePhraseName % to (toBnfcIdent proxy)]
            (phrase ^. typePhraseFrom % to (mplTypeToBnfc proxy)) 
            (phrase ^. typePhraseTo % to (mplTypeToBnfc proxy))

{- clause to bnfc -}
class MplClauseToBnfc x t res y | t -> res where
    mplClauseToBnfc :: Proxy y -> MplTypeClause x t -> res

instance 
    ( PPrint (IdP x) y
    , MplTypesToBnfc (XTypePhraseFrom x ('SeqObjTag 'DataDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('SeqObjTag 'DataDefnTag)) y ) =>
    MplClauseToBnfc x (SeqObjTag DataDefnTag) B.SeqTypeClauseDefn y where
    mplClauseToBnfc proxy clause = 
        B.SEQ_TYPE_CLAUSE name st $ clause ^. typeClausePhrases % to (map (mplPhraseToBnfc proxy))
      where
        name = B.MPL_UIDENT_ARGS_TYPE (clause ^. typeClauseName % to (toBnfcIdent proxy))
            bnfcKeyword (clause ^. typeClauseArgs % to 
                (map (B.MPL_UIDENT_NO_ARGS_TYPE . (toBnfcIdent proxy)))) bnfcKeyword
        st = clause ^. typeClauseStateVar % to (B.MPL_UIDENT_NO_ARGS_TYPE . (toBnfcIdent proxy) )

instance  
    ( PPrint (IdP x) y
    , MplTypesToBnfc (XTypePhraseFrom x ('SeqObjTag 'CodataDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('SeqObjTag 'CodataDefnTag)) y ) =>
    MplClauseToBnfc x (SeqObjTag CodataDefnTag) B.SeqTypeClauseDefn y where
    mplClauseToBnfc proxy clause = 
        B.SEQ_TYPE_CLAUSE st name $ clause ^. typeClausePhrases % to (map (mplPhraseToBnfc proxy))
      where
        name = B.MPL_UIDENT_ARGS_TYPE (clause ^. typeClauseName % to (toBnfcIdent proxy) )
            bnfcKeyword (clause ^. typeClauseArgs % to 
                (map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent proxy))) bnfcKeyword
        st = clause ^. typeClauseStateVar % to (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent proxy)

instance  
    ( PPrint (IdP x) y
    , MplTypeToBnfc (XTypePhraseTo x ('ConcObjTag 'ProtocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseFrom x ('ConcObjTag 'ProtocolDefnTag)) y) =>
    MplClauseToBnfc x (ConcObjTag ProtocolDefnTag) B.ConcurrentTypeClauseDefn y where
    mplClauseToBnfc proxy clause = 
        B.CONCURRENT_TYPE_CLAUSE name st $ clause ^. typeClausePhrases % to (map (mplPhraseToBnfc proxy))
      where
        name = B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE 
            (clause ^. typeClauseName % to (toBnfcIdent proxy))
            bnfcKeyword 
            seqs
            concs
            bnfcKeyword
        (seqs, concs) = clause ^. typeClauseArgs % to 
                ( map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent proxy)
                        *** map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent proxy))
        st = clause ^. typeClauseStateVar % to (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent proxy)

instance 
    ( PPrint (IdP x) y
    , MplTypeToBnfc (XTypePhraseFrom x ('ConcObjTag 'CoprotocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('ConcObjTag 'CoprotocolDefnTag)) y) => 
    MplClauseToBnfc x (ConcObjTag CoprotocolDefnTag) B.ConcurrentTypeClauseDefn y where
    mplClauseToBnfc proxy clause = 
        B.CONCURRENT_TYPE_CLAUSE st name $ clause ^. typeClausePhrases % to (map (mplPhraseToBnfc proxy))
        -- error "to do in pretty printer"
      where
        name = B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE 
            (clause ^. typeClauseName % to (toBnfcIdent proxy))
            bnfcKeyword 
            seqs
            concs
            bnfcKeyword
        (seqs, concs) = clause ^. typeClauseArgs % to 
                ( map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent proxy)
                        *** map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent proxy))
        st = clause ^. typeClauseStateVar % to (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent proxy )

mplObjDefnToBnfc :: 
    ( XDataDefn x ~ MplTypeClauseSpine x (SeqObjTag DataDefnTag)
    , XCodataDefn x ~ MplTypeClauseSpine x (SeqObjTag CodataDefnTag)
    , XProtocolDefn x ~ MplTypeClauseSpine x (ConcObjTag ProtocolDefnTag)
    , XCoprotocolDefn x ~ MplTypeClauseSpine x (ConcObjTag CoprotocolDefnTag)

    , MplTypeToBnfc (XTypePhraseFrom x ('ConcObjTag 'CoprotocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('ConcObjTag 'ProtocolDefnTag)) y
    , MplTypesToBnfc (XTypePhraseFrom x ('SeqObjTag 'CodataDefnTag)) y

    , MplTypeToBnfc (XTypePhraseTo x ('ConcObjTag 'CoprotocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseFrom x ('ConcObjTag 'ProtocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('SeqObjTag 'CodataDefnTag)) y
    , MplTypesToBnfc (XTypePhraseFrom x ('SeqObjTag 'DataDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('SeqObjTag 'DataDefnTag)) y

    , PPrint (IdP x) y
    ) =>
    Proxy y ->
    MplObjectDefn x -> 
    B.MplDefn
mplObjDefnToBnfc proxy (SeqObjDefn (DataDefn x)) = 
    B.MPL_SEQUENTIAL_TYPE_DEFN 
        $ B.DATA_DEFN 
        $ (x ^. typeClauseSpineClauses % to (NE.toList . fmap (mplClauseToBnfc proxy) ))
mplObjDefnToBnfc proxy (SeqObjDefn (CodataDefn x)) = 
    B.MPL_SEQUENTIAL_TYPE_DEFN 
        $ B.CODATA_DEFN 
        $ (x ^. typeClauseSpineClauses % to (NE.toList . fmap (mplClauseToBnfc proxy) ))
mplObjDefnToBnfc proxy (ConcObjDefn (ProtocolDefn x)) = 
    B.MPL_CONCURRENT_TYPE_DEFN 
        $ B.PROTOCOL_DEFN 
        $ (x ^. typeClauseSpineClauses % to (NE.toList . fmap (mplClauseToBnfc proxy) ))
mplObjDefnToBnfc proxy (ConcObjDefn (CoprotocolDefn x)) = 
    B.MPL_CONCURRENT_TYPE_DEFN 
        $ B.COPROTOCOL_DEFN 
        $ (x ^. typeClauseSpineClauses % to (NE.toList . fmap (mplClauseToBnfc proxy) ))

{- | expressions to bnfc -}
class MplExprToBnfc t y where
    mplExprToBnfc :: Proxy y -> t -> B.Expr

instance 
    MplPrintConstraints x y =>
    MplExprToBnfc (MplExpr x) y where
    mplExprToBnfc proxy = f
      where
        f (EPOps _ op exp0 exp1) = case op of
            PrimitiveAdd ->  B.INFIXL5_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveSub ->  B.INFIXL5_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveMul ->  B.INFIXL6_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveDiv ->  B.INFIXL6_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)

            PrimitiveEq ->  B.INFIXL3_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveNeq ->  B.INFIXL3_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveLeq ->  B.INFIXL3_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveLt ->  B.INFIXL3_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveGeq ->  B.INFIXL3_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveGt ->  B.INFIXL3_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            PrimitiveColon ->  B.INFIXR0_EXPR (f exp0) (toBnfcIdent proxy op) (f exp1)
            -- _ ->  error "primitive op not implemented yet"

        f (EProj _ n expr) = B.FUN_EXPR  (toBnfcIdent proxy ("__INTERNAL_PROJ_" ++ show n)) bnfcKeyword [f expr] bnfcKeyword 
        f (EVar _ id) = B.VAR_EXPR $ toBnfcIdent proxy id 
        f (EInt _ id) = B.INT_EXPR $ toBnfcIdent proxy id 
        f (EChar _ id) = B.CHAR_EXPR $ toBnfcIdent proxy id
        f (EDouble _ id) = B.DOUBLE_EXPR $ toBnfcIdent proxy id 
        f (EUnit _) = B.UNIT_EXPR bnfcKeyword bnfcKeyword
        f (EList _ ls) = B.LIST_EXPR bnfcKeyword (map f ls) bnfcKeyword
        f (EString _ ls) = B.STRING_EXPR (toBnfcIdent proxy $ show ls) 

        f (EBool _ id) = B.DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR  
            (toBnfcIdent proxy $ show id) 
            bnfcKeyword 
                [] 
            bnfcKeyword 

        f (EIf _ cond thenc elsec) = 
            B.IF_EXPR
                (f cond)
                (f thenc)
                (f elsec)

        f (ESwitch _ switches) = 
            B.SWITCH_EXP 
                $ map g 
                $ NE.toList switches
          where
            g (l,r) = B.SWITCH_EXPR_PHRASE (f l) (f r)

        f (ETuple _ (t0,t1,ts)) = 
            B.TUPLE_EXPR 
            bnfcKeyword 
                (f t0) 
                (map (B.TUPLE_EXPR_LIST . f) (t1:ts))
            bnfcKeyword
        -- | TUPLE_EXPR LBracket Expr [TupleExprList] RBracket

        f (ECase _ expr pattexprs) = B.CASE_EXPR bnfcKeyword (f expr) $ NE.toList $ fmap g pattexprs
          where
            g (patt, expr) = B.PATTERN_TO_EXPR [mplPattToBnfc proxy patt] (mplExprToBnfc proxy expr)
        f (EObjCall _ id exprs) = B.DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR  
            (toBnfcIdent proxy id) bnfcKeyword (map f exprs) bnfcKeyword 
        f (ECall _ id exprs) = B.FUN_EXPR  (toBnfcIdent proxy id) bnfcKeyword (map f exprs) bnfcKeyword 
        f (ELet _ stmts expr) = B.LET_EXPR (NE.toList (fmap g stmts)) (f expr)
          where
            g = B.LET_EXPR_PHRASE . mplStmtToBnfc proxy
        f (ERecord _ exprs) = B.RECORD_EXPR bnfcKeyword (NE.toList $ fmap g exprs) bnfcKeyword
          where
            g (cxt, ident, (patts, expr)) = 
                B.RECORD_EXPR_HIGHER_ORDER_PHRASE (toBnfcIdent proxy ident) 
                    $ B.PATTERN_TO_EXPR (map (mplPattToBnfc proxy) patts) (f expr)
        f (EFold cxt expr phrases) = B.FOLD_EXPR (f expr) $ NE.toList $ fmap g phrases
          where
            g (cxt, ident, patts, expr) = 
                B.FOLD_EXPR_PHRASE 
                    (toBnfcIdent proxy ident) 
                    bnfcKeyword 
                    (map (mplPattToBnfc proxy) patts) 
                    (f expr)
        f (EUnfold cxt expr phrases) = B.UNFOLD_EXPR (f expr) $ NE.toList $ fmap g phrases
          where
            g (cxt, patt, foldphrases) = 
                B.UNFOLD_EXPR_PHRASE (mplPattToBnfc proxy patt) $ NE.toList $ fmap h foldphrases
            h (cxt, ident, patts, expr) = 
                B.FOLD_EXPR_PHRASE 
                    (toBnfcIdent proxy ident) bnfcKeyword 
                    (map (mplPattToBnfc proxy) patts) (f expr)

        f (EIllegalInstr _) = B.VAR_EXPR $ toBnfcIdent proxy "ILLEGAL_INSTR"

        {-
        EVar !(XEVar x) (IdP x)
        EInt !(XEInt x) Int
        EChar !(XEChar x) Char
        EDouble !(XEDouble x) Double
        ECase !(XECase x) (MplExpr x) (NonEmpty (XMplPattern x, MplExpr x))
    
        EObjCall !(XEObjCall x) (IdP x) [MplExpr x]
        ECall !(XECall x) (IdP x) [MplExpr x]
        ERecord !(XERecord x) (NonEmpty (XERecordPhrase x, IdP x, ([XMplPattern x], MplExpr x)))
    
         built in...
        EList !(XEList x) [MplExpr x]
        EString !(XEString x) [MplExpr x]
        EUnit !(XEUnit x) 
        ETuple !(XETuple x) (MplExpr x, MplExpr x, [MplExpr x])
        EBuiltInOp !(XEBuiltInOp x) BuiltInOperators (MplExpr x) (MplExpr x)
    
    
        EIf !(XEIf x) (MplExpr x) (MplExpr x) (MplExpr x)
        ELet !(XELet x) (NonEmpty (MplStmt x)) (MplExpr x)
        EFold !(XEFold x) (MplExpr x)
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
                , NonEmpty (XEFoldPhrase x, IdP x, [XMplPattern x], (MplExpr x))
                )
            )
        | ESwitch !(XESwitch x) (NonEmpty ((MplExpr x), (MplExpr x)))
    
        | XExpr !(XXExpr x) -}

class MplCmdToBnfc t y where
    mplCmdToBnfc :: Proxy y -> t -> B.ProcessCommand

instance 
    ( MplToForkPhrase (CForkPhrase x) y
    , MplToPlugPhrase (CPlugPhrase x) y
    , PPrint (IdP x) y
    , PPrint (ChP x) y
    , MplExprToBnfc (XMplExpr x) y
    , MplPattToBnfc (XCCasePattern x) y
    , MplPattToBnfc (XMplPattern x) y) => MplCmdToBnfc (MplCmd x) y where
    mplCmdToBnfc proxy = f
      where
        f (CRun _ id seqs ins outs) = 
            B.PROCESS_RUN (toBnfcIdent proxy id) bnfcKeyword (map (mplExprToBnfc proxy) seqs) 
                (map (toBnfcIdent proxy) ins) (map (toBnfcIdent proxy) outs) bnfcKeyword
        f (CClose _ ch) = B.PROCESS_CLOSE bnfcKeyword $ toBnfcIdent proxy ch
        f (CHalt _ ch) = B.PROCESS_HALT bnfcKeyword $ toBnfcIdent proxy ch
        f (CGet _ patt ch) = B.PROCESS_GET bnfcKeyword (mplPattToBnfc proxy patt) (toBnfcIdent proxy ch)
        f (CPut _ expr ch) = B.PROCESS_PUT bnfcKeyword (mplExprToBnfc proxy expr) (toBnfcIdent proxy ch)
        f (CHCase _ ch cases) = B.PROCESS_HCASE bnfcKeyword (toBnfcIdent proxy ch) $ NE.toList $ fmap g cases
          where
            g (_, id, cmds) = B.HCASE_PHRASE (toBnfcIdent proxy id) (mplCmdsToBnfc proxy cmds)
        f (CHPut _ id ch) = B.PROCESS_HPUT bnfcKeyword (toBnfcIdent proxy id) (toBnfcIdent proxy ch)
        f (CSplit _ ch (s,t)) = B.PROCESS_SPLIT bnfcKeyword (toBnfcIdent proxy ch) (map (toBnfcIdent proxy) [s,t])
        -- TODO -- we should actually show the generated context here!
        f (CFork _ ch (a,b)) = 
            B.PROCESS_FORK bnfcKeyword (toBnfcIdent proxy ch) $ map (mplToForkPhrase proxy) [a, b]
        f (CId _ (a,b)) = B.PROCESS_ID (toBnfcIdent proxy a) bnfcKeyword (toBnfcIdent proxy b)
        f (CIdNeg _ (a,b)) = B.PROCESS_NEG 
            (toBnfcIdent proxy a) bnfcKeyword 
            (toBnfcIdent proxy b)
        f (CRace _ races) = B.PROCESS_RACE $ NE.toList $ fmap g races
          where
            g (ch, cmds) = B.RACE_PHRASE (toBnfcIdent proxy ch) $ mplCmdsToBnfc proxy cmds
        -- TODO -- we should actually show the generated context here!??
        f (CPlugs _ (a,b,c)) = B.PROCESS_PLUG $ map (mplToPlugPhrase proxy) (a:b:c)

        f (CCase cxt caseon cases) = B.PROCESS_CASE bnfcKeyword 
            (mplExprToBnfc proxy caseon) $ NE.toList $ fmap g cases 
          where
            g (patt, cmds) = B.PROCESS_CASE_PHRASE 
                (mplPattToBnfc proxy patt) 
                (mplCmdsToBnfc proxy cmds)
        f (CSwitch cxt switches) = B.PROCESS_SWITCH $ NE.toList $ fmap g switches
          where
            g (expr, cmds) = B.PROCESS_SWITCH_PHRASE 
                (mplExprToBnfc proxy expr) 
                (mplCmdsToBnfc proxy cmds)

        f (CIf cxt expr cthen celse) = 
            B.PROCESS_IF 
                (mplExprToBnfc proxy expr)
                (mplCmdsToBnfc proxy cthen)
                (mplCmdsToBnfc proxy celse)
        -- TODO: add something in
        f (CIllegalInstr _) = 
            B.PROCESS_RUN (toBnfcIdent proxy "ILLEGALINSTR") bnfcKeyword [] [] [] bnfcKeyword 

-- we can configure a fork phrase to have a context..
class MplToForkPhrase t y where
    mplToForkPhrase :: Proxy y -> t -> B.ForkPhrase
          {-
          where
            g (a,cxt, cmds) = undefined
                -- Just cxt -> B.FORK_WITH_PHRASE (toBnfcIdent a) (map toBnfcIdent cxt) $ mplCmdsToBnfc cmds
                -- Nothing -> B.FORK_PHRASE (toBnfcIdent a) $ mplCmdsToBnfc cmds
           -}
instance ( PPrint ident y, MplCmdToBnfc t y) => MplToForkPhrase (ident, [ident], NonEmpty t) y where
    mplToForkPhrase proxy (ch, cxt, cmds) = B.FORK_WITH_PHRASE 
        (toBnfcIdent proxy ch) (map (toBnfcIdent proxy) cxt) $ mplCmdsToBnfc proxy cmds

instance ( PPrint ident y, MplCmdToBnfc t y) => MplToForkPhrase (ident, Maybe [ident], NonEmpty t) y where
    mplToForkPhrase proxy (ch, Just cxt, cmds) = mplToForkPhrase proxy (ch, cxt, cmds)
    mplToForkPhrase proxy (ch, Nothing, cmds) = B.FORK_PHRASE (toBnfcIdent proxy ch)  $ mplCmdsToBnfc proxy cmds

class MplToPlugPhrase t y where
    mplToPlugPhrase :: Proxy y -> t -> B.PlugPhrase
          {-
          where
            g (_, (a,b), cmds) = B.PLUG_PHRASE_AS 
                (map toBnfcIdent a) (map toBnfcIdent b) (mplCmdsToBnfc cmds)
            -}
instance ( PPrint ident y, MplCmdToBnfc cmd y) => 
    MplToPlugPhrase ((), ([ident], [ident]), NonEmpty cmd) y where
    mplToPlugPhrase proxy (cxt, (ins, outs), cmds) = B.PLUG_PHRASE_AS 
        (map (toBnfcIdent proxy) ins) 
        (map (toBnfcIdent proxy) outs) 
        $ mplCmdsToBnfc proxy cmds


mplCmdsToBnfc :: MplCmdToBnfc t y => Proxy y -> NonEmpty t -> B.ProcessCommandsBlock
mplCmdsToBnfc proxy (cmd :| []) = B.PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK $ mplCmdToBnfc proxy cmd
mplCmdsToBnfc proxy cmds = B.PROCESS_COMMANDS_DO_BLOCK $ NE.toList $ fmap (mplCmdToBnfc proxy) cmds

instance PPrint ChIdentT x where
    pprint proxy n = n ^. chIdentTChIdentR % to (pprint proxy)

instance PPrint B.MplProg x where
    pprint _ = B.printTree 

instance PPrint B.MplType x where
    pprint _ = B.printTree 

instance (PPrint (IdP x) t, PPrint (TypeP x) t) => PPrint (MplType x) t where
    pprint proxy = B.printTree . mplTypeToBnfc proxy

-- print instances of annotations from type checking (never printed out)
instance PPrint ((), MplSeqObjDefn MplTypeCheckedClause) y where
    pprint _ = const ""
instance PPrint ((), MplConcObjDefn MplTypeCheckedClause) y where
    pprint _ = const ""


{- final printing functions -}
type MplPrintConstraints x y =
    ( XDataDefn x ~ MplTypeClauseSpine x (SeqObjTag DataDefnTag)
    , XCodataDefn x ~ MplTypeClauseSpine x (SeqObjTag CodataDefnTag)
    , XProtocolDefn x ~ MplTypeClauseSpine x (ConcObjTag ProtocolDefnTag)
    , XCoprotocolDefn x ~ MplTypeClauseSpine x (ConcObjTag CoprotocolDefnTag)

    , MplTypeToBnfc (XTypePhraseFrom x ('ConcObjTag 'CoprotocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('ConcObjTag 'ProtocolDefnTag)) y
    , MplTypesToBnfc (XTypePhraseFrom x ('SeqObjTag 'CodataDefnTag)) y

    , MplTypeToBnfc (XTypePhraseTo x ('ConcObjTag 'CoprotocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseFrom x ('ConcObjTag 'ProtocolDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('SeqObjTag 'CodataDefnTag)) y
    , MplTypesToBnfc (XTypePhraseFrom x ('SeqObjTag 'DataDefnTag)) y
    , MplTypeToBnfc (XTypePhraseTo x ('SeqObjTag 'DataDefnTag)) y

    , MplPattToBnfc (XECasePattern x) y

    , PPrint (IdP x) y

    , XProcessDefn x ~ MplProcess x
    , XFunctionDefn x ~ MplFunction x


    , MplCmdToBnfc (XMplCmd x) y
    , PPrint (ChP x) y
    , UserProvidedTypeToBnfc (XProcType x) y
    , MplExprToBnfc (XMplExpr x) y
    , MplPattToBnfc (XMplPattern x) y
    , UserProvidedTypeToBnfc (XFunType x) y
    ) 

mplPprint :: 
    MplPrintConstraints x y =>
    Proxy y ->
    MplProg x -> 
    String
mplPprint proxy = B.printTree . mplProgToBnfc proxy

mplProgToBnfc :: 
    MplPrintConstraints x y =>
    Proxy y ->
    MplProg x -> 
    B.MplProg
mplProgToBnfc proxy (MplProg stmts) = B.MPL_PROG $ map (mplStmtToBnfc proxy) stmts

mplStmtToBnfc :: 
    MplPrintConstraints x y =>
    Proxy y -> 
    MplStmt x -> 
    B.MplStmt
mplStmtToBnfc proxy (MplStmt (defn:| []) []) = 
    B.MPL_STMT $ mplDefnToBnfc proxy defn
mplStmtToBnfc proxy (MplStmt defns []) = 
    B.MPL_DEFN_STMS $ NE.toList $ fmap (mplDefnToBnfc proxy) defns
mplStmtToBnfc proxy (MplStmt defns wheres) = 
    B.MPL_DEFN_STMS_WHERE (NE.toList $ fmap (mplDefnToBnfc proxy) defns) 
        (map (B.MPL_WHERE . mplStmtToBnfc proxy) wheres)

mplDefnToBnfc :: 
    MplPrintConstraints x y =>
    Proxy y ->
    MplDefn x -> 
    B.MplDefn
mplDefnToBnfc proxy (ObjectDefn obj) = mplObjDefnToBnfc proxy obj
mplDefnToBnfc proxy (FunctionDefn (MplFunction id tp body)) = B.MPL_FUNCTION_DEFN $ 
    case userProvidedTypeToBnfc proxy tp of
        Just tp -> B.INTERNAL_TYPED_FUNCTION_DEFN id' tp body'
        Nothing -> B.FUNCTION_DEFN id' body'
  where
    id' = toBnfcIdent proxy id
    body' = NE.toList $ fmap 
        (uncurry B.PATTERN_TO_EXPR <<< map (mplPattToBnfc proxy) *** mplExprToBnfc proxy) 
        body
mplDefnToBnfc proxy (ProcessDefn (MplProcess id tp body)) = B.MPL_PROCESS_DEFN $ 
    case userProvidedTypeToBnfc proxy tp of
        Just tp -> B.INTERNAL_TYPED_PROCESS_DEFN id' tp body'
        Nothing -> B.PROCESS_DEFN id' body'
        
  where
    id' = toBnfcIdent proxy id
    body' = NE.toList $ fmap f body

    f ((patts, ins, outs), cmds) = 
        B.PROCESS_PHRASE 
            (map (mplPattToBnfc proxy) patts) 
            (map (toBnfcIdent proxy) ins) 
            (map (toBnfcIdent proxy) outs) $ mplCmdsToBnfc proxy cmds


instance MplPrintConstraints x y => PPrint (MplProg x) y where
    pprint = mplPprint

instance MplPrintConstraints x y => PPrint [MplDefn x] y where
    pprint proxy = pprint proxy . B.MPL_PROG . map (B.MPL_STMT . mplDefnToBnfc proxy)

instance 
    ( PPrint (IdP x) y
    , MplPattToBnfc (XMplPattern x) y
    , MplPattToBnfc (XCCasePattern x) y
    , MplSimpleConstructorArgsToBnfc (XPSimpleConstructorArgs x) y
    ) => PPrint (MplPattern x) y where
    pprint proxy = B.printTree . mplPattToBnfc proxy

instance 
    ( PPrint (IdP x) y
    , MplToForkPhrase (ChP x, XCForkPhrase x, NonEmpty (MplCmd x)) y
    , MplToPlugPhrase (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty (MplCmd x)) y
    , PPrint (ChP x) y
    , MplExprToBnfc (XMplExpr x) y
    , MplPattToBnfc (XMplPattern x) y
    , MplPattToBnfc (XCCasePattern x) y
    ) => PPrint (MplCmd x) y where
    pprint proxy = B.printTree . mplCmdToBnfc proxy

instance MplPrintConstraints x y => PPrint (MplExpr x) y where
    pprint proxy = B.printTree . mplExprToBnfc proxy


{- | Wrapper function for 'pprint' specialized to 'MplParsed'. This is used most frequently when reprinting the AST to users -}
pprintParsed :: ( PPrint a MplParsed ) => a -> String
pprintParsed a = pprint (Proxy :: Proxy MplParsed) a

