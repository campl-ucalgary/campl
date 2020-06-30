{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module CMPLCompileAST where

import CMPLConstructsBag
import CMPLAST

import AMPLAST (AComIdents (..), ALabelComsIdents (..) )
import AMPLUntaggedConstructBag 
import qualified AMPLAST as AMPL

import Data.Functor.Foldable

import Control.Monad.State

import Optics.TH
import Optics.Optic
import Optics.State.Operators
import Optics.Operators
import Optics.Iso
import Optics.Re
import Optics.View
import Optics.Setter
import Data.Tuple.Optics
import Data.Tuple

import Control.Arrow

import Data.Function

import MPLIdent

-- this is guaranteed to be unique in AMPLASM
-- because # is not permited in the front end
cmplUniqueTmp :: String
cmplUniqueTmp = amplAsmUniquePrefix ++ "tmp"

defaultCmplUniqueVarState :: CmplUniqueVarState 
defaultCmplUniqueVarState = CmplUniqueVarState {
    _compileExprUniqueSuffix = 0
    , _compileExprTmpVar = cmplUniqueTmp
}

data CmplUniqueVarState = CmplUniqueVarState {
    -- | Counter to give a unique suffix
    _compileExprUniqueSuffix :: Word
    -- | The tmp var which will receive the unqiue suffix
    , _compileExprTmpVar :: String
}
$(makeClassy ''CmplUniqueVarState)

freshTmpVar :: 
    ( HasCmplUniqueVarState s
    , MonadState s m ) =>
    m String
freshTmpVar = 
    gets (\n -> show (n ^. compileExprTmpVar) ++ n ^. compileExprTmpVar) 
    <* (compileExprUniqueSuffix %= succ)

-- Note -- this is lossy with the positions of identifiers... 
-- BNFC does not allow declaring a position token
-- and a layout token, so we cannot preserve the 
-- location of certain tokens (e.g. plug and race)
-- so we replace those with the null values instead
cmplCompileExpr :: 
    ( HasCmplUniqueVarState s
    , MonadState s m ) => 
    Expr -> m [UntaggedACom]
cmplCompileExpr = cata cmplCompileExprF

cmplCompileExprF ::
    ( HasCmplUniqueVarState s
    , MonadState s m ) => 
    ExprF (m [UntaggedACom]) -> m [UntaggedACom]
cmplCompileExprF n = 
    case n of
        ECallF fun exprs -> do
            callargs <- sequence exprs >>= generateCallArgsIdentifiers
            let funident = fun
                callinstr = ACall () funident (map fst callargs)
            return (concatMap snd callargs <> [callinstr])

        EOpF op a b -> concat <$> sequence [b, a, generateOp op]

        EConsF ((base, sub), exprs) -> do
            callargs <- sequence exprs >>= generateCallArgsIdentifiers
            let consbase = base
                conssub = sub
                consinstr = AConstructorArgs ((consbase, conssub), map fst callargs)
            return (concatMap snd callargs <> [consinstr])
        EDestF ((base, sub), expr) -> do
            callarg <- expr >>= generateCallArgIdentifier
            let consbase = base
                conssub = sub
                consinstr = ADest (consbase, conssub) (fst callarg)
            return (snd callarg <> [consinstr])

        ECaseF caseon cases -> do
            caseon' <- caseon >>= generateCallArgIdentifier
            labelcoms <- map (overALabelComsComs (++[ARet ()]))
                            <$> generateUntaggedALabelComs cases
            return (snd caseon' <> [ACase () (fst caseon', labelcoms)] )

        EIfF b (bthen, belse) -> do
            b' <- b >>= generateCallArgIdentifier
            clause <- (,) <$> bthen <*> belse
            return (snd b' <> [AIf () (fst b', clause)])

        EVarF ident -> return [ALoad () (ident)]

        EConstCharF c -> return [AChar () c]
        EConstIntF i -> return [AInt () i]
        EStringF s -> error "havne't really thought about how to do strings yet..." 

        ERecordF recs -> do
            labelcoms <- map (overALabelComsComs (++[ARet ()]))
                        <$> generateUntaggedALabelComs recs
            return [ARecord () labelcoms]

        EProductF tps -> error "haven't totally thought of tuples yet.."
        EProductElemF (nth, tuple) -> error "Need the type information if we are just generating more codata"

        EErrorF str -> return [AErrMsg str]

generateCallArgIdentifier :: 
    ( HasCmplUniqueVarState s
    , MonadState s m ) => 
    [UntaggedACom] -> 
    m (String, [UntaggedACom])
generateCallArgIdentifier acoms = do
    tmp <- freshTmpVar
    return ( tmp, acoms <> [AStore () tmp] )

generateCallArgsIdentifiers :: 
    ( HasCmplUniqueVarState s
    , MonadState s m ) => 
    [[UntaggedACom]] -> 
    m [(String, [UntaggedACom])]
generateCallArgsIdentifiers = traverse generateCallArgIdentifier

generateUntaggedALabelComs :: 
    forall m s r.
    ( HasCmplUniqueVarState s
    , MonadState s m ) => 
    [((String,String),[String], m [UntaggedACom])] -> 
    m [UntaggedALabelComs]
generateUntaggedALabelComs = traverse f
  where
    f :: ((String, String), [String], m [UntaggedACom]) -> m UntaggedALabelComs
    f ((base, sub), args, coms) = do
        coms' <- coms
        return $ 
            ALabelComsArgs 
                ((base, sub), args)
                coms' 

overALabelComsComs :: ([UntaggedACom] -> [UntaggedACom]) -> UntaggedALabelComs -> UntaggedALabelComs
overALabelComsComs f (ALabelComs n coms) = ALabelComs n (f coms)
overALabelComsComs f (ALabelComsArgs n coms) = ALabelComsArgs n (f coms)

generateOp ::
    ( HasCmplUniqueVarState s
    , MonadState s m ) => 
    BuiltInOps -> 
    m [UntaggedACom]
generateOp n = do
    return $ case n of
        AddInt -> [AAddInt ()]
        SubInt -> [ASubInt ()]
        MulInt -> [AMulInt ()]
        ModInt -> [AModInt ()]

        EqInt -> [AEqInt ()]

        EqChar -> [AEqChar ()] 

cmplCompileProcessCommand :: 
    forall m s r . 
    ( HasCmplUniqueVarState s
    , MonadState s m ) => 
    [ProcessCommand] -> 
    m [UntaggedACom]
cmplCompileProcessCommand = fix f
  where
    f :: ([ProcessCommand] -> m [UntaggedACom]) -> [ProcessCommand] -> m [UntaggedACom]
    f rec [] = return []
    f rec (cmd:cmds) = do 
        case cmd of
            PRun (pname, (seqs, (inchs, outchs))) -> 
                let mseqs = traverse cmplCompileExpr seqs >>= generateCallArgsIdentifiers
                    inchs' = inchs
                    outchs' = outchs
                in concat <$> sequence 
                    [ concatMap snd <$> mseqs
                    , sequence 
                        [ ARun () pname <$> ( (,(inchs', outchs')) 
                            . map fst <$> mseqs) ]
                    , rec cmds]

            PClose ch -> (:) (AClose () (ch)) <$> rec cmds
            PHalt chs -> (++) (map (AHalt ()) chs)
                <$> rec cmds

            PHCase ch handles -> (:) 
                <$> ( AHCase () . (ch,) 
                    <$> generateUntaggedALabelComs (map (\(ident, cmds) -> (ident, [], rec cmds)) handles) )
                <*> rec cmds
            PHPut (base, sub) ch -> (:) 
                (AHPut () (base, sub) (ch))
                <$> rec cmds

            PGet v ch -> (:)
                (AGet () v ch)
                <$> rec cmds
            PPut e ch -> do
                tmp <- freshTmpVar
                concat <$> sequence
                    [ cmplCompileExpr e
                    , pure [ALoad () tmp]
                    , pure [APut () ch tmp ]
                    , rec cmds
                    ]

            PSplit ch (ch1,ch2) -> (:)
                (ASplit () ch
                    ch1 ch2)
                <$> rec cmds
            PFork ch ((ch1, ch1s), cmds1) ((ch2, ch2s), cmds2) -> (:)
                <$> ( AFork () ch
                        <$> ( ((ch1, ch1s), )
                            <$> rec cmds1 )
                        <*> ( ((ch2, ch2s), )
                            <$> rec cmds2 ) )
                <*> rec cmds

            PPlug chs (chs1, cmds1) (chs2, cmds2) -> (:)
                <$> ( APlug () chs
                        <$> ( (chs1,) <$> rec cmds1 )
                        <*> ( (chs2,) <$> rec cmds2 )
                        )
                <*> rec cmds

            PId (ch1, ch2) -> (:)
                (AId () (ch1, ch2))
                <$> rec cmds

            PCase caseon cases -> do
                caseon' <- cmplCompileExpr caseon >>= generateCallArgIdentifier
                labelcoms <- generateUntaggedALabelComs (map (over _3 rec) cases)
                cmds' <- rec cmds
                return (snd caseon' <> [ACase () (fst caseon', labelcoms)] <> cmds')

            PIf b (bthen, belse) -> do
                b' <- cmplCompileExpr b >>= generateCallArgIdentifier
                clause <- (,) <$> rec bthen <*> rec belse
                cmds' <- rec cmds
                return (snd b' <> [AIf () (fst b', clause)] <> cmds')

            n -> error ("unsupported: " ++ show n)
