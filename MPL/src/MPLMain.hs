module MPLMain where

import MPLPar.LexMPL
import MPLPar.ParMPL
import MPLPar.LayoutMPL
import MPLPar.AbsMPL
import MPLPar.ErrM
import MPLPar.PtreeToAST

import TypeInfer.MPL_AST
import TypeInfer.Gen_Eqns_Stmt
import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.PreProcess_Let

import ToCMPL.MPLToCMPL
import ToCMPL.GenAndSolveSetEqns
import ToCMPL.LamLift_Help
import ToCMPL.PatternCompiler
import ToCMPL.LambdaLift


{-
import qualified CMPL.CMPLtoPTree as CP 
import qualified CMPL.CompileAll as CALL
import qualified AMPL.STConverter_AMPL as SA 
import qualified AMPL.PrintAMPL as PA 
import qualified AMPL.AMPL_am as AM 
-}

import System.Environment
import Control.Monad.State
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as Map 


myLLexerMPL = resolveLayout True .myLexer
  
mainRunner = do 
   args <- getArgs
   case args of 
     [] -> do
        putStrLn "Error:Expecting a file name as an argument.Try Again."
     (fname:xs) -> do
        conts <- readFile fname
        let 
          tokens   = myLLexerMPL conts
          errPTree = pMPL tokens
        case errPTree of 
            Bad s -> do 
                putStrLnRed $ "Error in Parsing \n" ++ show  s     
                
            Ok pTree -> do 
                let 
                  astMPL   = evalState (transMPL  pTree) []
                  preP_MPL = preprocessBefTyping astMPL
                putStrLn $ prettyStyle zigStyle preP_MPL
                case typeMPL preP_MPL of 
                   Left emsg -> do 
                     putStrLn emsg 
                    
                   Right typemsg -> do
                     let 
                       msg  = "Do you want to see the types?"
                       msgF = unlines 
                                [equalS,equalS,msg]
                     putStrLn msgF
                     bool <- getChoice
                     printMsg typemsg Nothing bool
                     let 
                       finMPL_AST = pattCompile preP_MPL
                     case finMPL_AST of 
                       Left mplEmsg -> 
                          putStrLn $ unlines 
                            [equalS,equalS,mplEmsg,equalS,equalS]

                       Right fMPL  -> do                          
                          {- 
                          let 
                            astCMPL   = convMPL fMPL

                            ptreeAMPL = CP.convEverything astCMPL
                            ampl_prog = PA.printTree ptreeAMPL
                            astAMPL   = SA.transAMPLCODE ptreeAMPL
                          
                            mach      = CALL.compile_all astAMPL
                          putStrLn $ prettyStyle zigStyle mach   
                          ans <- evalStateT (AM.run_cm' mach) (Map.empty)
                          -}
                          return () 


printMsg :: String -> Maybe String ->  Bool -> IO ()
printMsg msg mayVal bool 
    = case bool of 
        True -> 
          putStrLn msg 
        False ->
          case mayVal of 
            Just msgFalse ->
              putStrLn msgFalse
            Nothing ->
              return  ()


getChoice :: IO Bool
getChoice  = do
   let 
     msg  = "Enter y/Y/Yes/yes for yes or n/N/No/no for No"
     msgF = unlines [equalS,equalS,msg,equalS,equalS]

   putStrLn msgF  
   ch <- getLine
   let 
     yess = ["y","Y","yes","Yes"]
     nos  = ["n","N","no","No"]
   case ch `elem` yess of 
      True ->
        return True 
      False ->
        case ch `elem` nos of 
          True ->
            return False 
          False -> do 
            putStrLn "Invalid Choice."
            getChoice  


