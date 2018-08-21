{-# LANGUAGE CPP#-}
module Paths (getStaticDir, samplesURL) where

import Control.Monad
import System.FilePath

#if defined(CABAL)
-- using cabal
import qualified Paths_gen_rnd_list (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = (</> "app/static") `liftM` Paths_gen_rnd_list.getDataDir

#elif defined(FPCOMPLETE)

getStaticDir :: IO FilePath
getStaticDir = return "app/static"

#else
-- using GHCi

getStaticDir :: IO FilePath
getStaticDir = return "static"

#endif

-- | Base URL for the example source code.
samplesURL :: String
samplesURL = "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/"
