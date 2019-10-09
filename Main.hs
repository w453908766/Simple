{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Syntax
import Infer
import Parser
--import Pretty
import Eval

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type IState = (Env, TermEnv)

tyctx = fst

initState :: IState
initState = (mempty, mempty)

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

exec :: Bool -> L.Text -> Repl ()
exec update source = do

  (tys, tms) <- get

  mod <- hoistErr $ parseModule "<stdin>" source

  ty' <- hoistErr $ inferTop tys mod

  let tms' = foldl' evalDef tms mod
  let tys' = tys <> ty'

  when update (put (tys', tms'))

  let (Define name _) = last mod

  when (name=="it") $ liftIO $ putStrLn $ show $ (Map.!) tms' "it"

    
cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ fmap show (tyctx st)

-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec True contents

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Map.lookup arg (tyctx st) of
    Just val -> liftIO $ putStrLn $ show (arg, val)
    Nothing -> exec False (L.pack arg)

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  --, (":type"  , values)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  ctx <- gets tyctx
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

options :: [(String, [String] -> Repl ())]
options = [
    ("load"   , load)
  , ("browse" , browse)
  , ("quit"   , quit)
  , ("type"   , Main.typeof)
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher


shell :: Repl a -> IO ()
shell pre = flip evalStateT initState
     $ evalRepl (return "Poly> ") cmd options (Just ':') completer pre

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> shell (return ())
    [fname] -> shell (load [fname])
    ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
    _ -> putStrLn "invalid arguments"

