module Data.BitCode.Plugin (registerPlugin) where

import DynFlags
import Plugins
import Hooks

-- for outputfn
import Stream           (Stream)
import Module

import qualified Stream
-- debugging
import Debug.Trace
import Cmm
import Cmm.Pretty

import DynFlags

import Data.BitCode.Hooks (outputFn, phaseInputExt, phaseHook)

-- We are a plugin!
registerPlugin :: [CommandLineOption] -> Plugin
registerPlugin opts = defaultPlugin
  { pluginHooks = Just $ emptyHooks
    { codeOutputHook    = Just (outputFn opts)
    , phaseInputExtHook = Just phaseInputExt
    , runPhaseHook      = Just phaseHook
    }
  }

