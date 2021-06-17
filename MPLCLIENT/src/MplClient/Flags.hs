{-# LANGUAGE TypeApplications #-}
module MplClient.Flags where

import Optics

import System.IO
import System.Environment
import qualified System.Console.GetOpt as GetOpt

import Data.Coerce

import MplMach.MplMachTypes
import MplClient.MplClientStack

import Control.Exception


getOpts :: IO MplClientEnv
getOpts = getArgs >>= \args -> case GetOpt.getOpt GetOpt.Permute options args of
    (envf, [], []) -> return $ foldr id defaultMplClientEnv envf 
    (_,_,errs) -> throwIO
        $ GetOptsException 
        $ concat
            [ concat errs
            , GetOpt.usageInfo header options 
            ]
  where
    header :: String
    header = "Usage: mpl-client [OPTIONS ...]"

    options :: [GetOpt.OptDescr (MplClientEnv -> MplClientEnv )]
    options =
        [ GetOpt.Option ['h'] ["hostname"]
            (GetOpt.ReqArg (\hn -> set hostname hn) "HOSTNAME") 
            "hostname"

        , GetOpt.Option ['p'] ["port"]
            (GetOpt.ReqArg (\hn -> set port hn) "PORT") 
            "port"

        , GetOpt.Option ['s'] ["service-ch"]
            (GetOpt.ReqArg (\hn -> set serviceCh (coerce @Int @ServiceCh $ (read :: String -> Int) $ hn)) "INT") 
            "service-ch"
        ]

newtype GetOptsException = GetOptsException String
  deriving Show

instance Exception GetOptsException where
