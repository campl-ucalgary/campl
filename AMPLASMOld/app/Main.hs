module Main where


import qualified CMPL.CMPLtoPTree as CP 
import qualified CMPL.CompileAll as CALL

import qualified STConverter_AMPL as SA 
import qualified Language.PrintAMPLGrammar as PA 

import System.Environment
import Control.Monad.Trans.State.Lazy
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import Control.Monad.Trans.Either
import qualified Data.Map as Map 

main :: IO ()
main = undefined

{-
myLLexerMPL = resolveLayout True .myLexer
  
main = do 
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
                          let 
                            astCMPL   = convMPL fMPL

                            ptreeAMPL = CP.convEverything astCMPL
                            ampl_prog = PA.printTree ptreeAMPL
                            astAMPL   = SA.transAMPLCODE ptreeAMPL
                          
                            mach      = CALL.compile_all astAMPL
                          putStrLn $ prettyStyle zigStyle mach   
                          -- ans <- evalStateT (AM.run_cm' mach) (Map.empty)
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
-}
