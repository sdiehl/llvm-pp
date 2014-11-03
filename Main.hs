module Main where

import Library

import qualified LLVM.General.Module as M
import LLVM.General.Context
import LLVM.General.PrettyPrint

import Control.Monad.Error

import System.IO
import System.Exit
import System.Environment

-------------------------------------------------------------------------------
-- Harness
-------------------------------------------------------------------------------

readir :: FilePath -> IO ()
readir fname = do
  putStrLn $ replicate 80 '='
  putStrLn fname
  putStrLn $ replicate 80 '='
  str <- readFile fname
  withContext $ \ctx -> do
    res <- runErrorT $ M.withModuleFromLLVMAssembly ctx str $ \mod -> do
      ast <- M.moduleAST mod
      putStrLn $ showPretty ast
      let str = ppllvm ast
      putStrLn str
      trip <- runErrorT $ M.withModuleFromLLVMAssembly ctx str (const $ return ())
      case trip of
        Left err -> do
          print err
          exitFailure
        Right ast -> putStrLn "round tripped!"

    case res of
      Left err -> print err
      Right _ -> return ()

main :: IO ()
main = do
  files <- getArgs
  mapM readir files
  return ()
