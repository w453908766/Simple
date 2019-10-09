
{-# LANGUAGE OverloadedStrings #-}

import Syntax
import Infer
import Parser
import Eval



data IState = IState
  { tyctx :: Env  -- Type environment
  , tmctx :: TermEnv  -- Value environment
  }

initState :: IState
initState = IState mempty emptyTmenv

st = initState 

Right mod' = parseModule "<stdin>" source

tyctx' = inferTop (tyctx st) mod'

source = "let f x = x+1"

