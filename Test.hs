{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Syntax
import Infer
import Parser
import Pretty
import Eval
import qualified Env

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

data IState = IState
  { tyctx :: Env.Env  -- Type environment
  , tmctx :: TermEnv  -- Value environment
  }

initState :: IState
initState = IState Env.empty emptyTmenv

st = initState 

Right mod' = parseModule "<stdin>" source

tyctx' = inferTop (tyctx st) mod'

source = "let f x = x+1"


