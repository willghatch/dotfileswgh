{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module MyAntigen where

import Antigen (AntigenConfiguration (..)
              , bundle
              , antigen
              , developFromFileSystem)
import Shelly (shelly)

bundles =
  [ bundle "zsh-users/zsh-history-substring-search"
  , bundle "hchbaw/opp.zsh.git"
  , bundle "alfredodeza/zsh-plugins.git vi/zle_vi_visual.zsh"
  , bundle "robbyrussell/oh-my-zsh plugins/wd"
  , bundle "robbyrussell/oh-my-zsh plugins/cabal"
  , bundle "robbyrussell/oh-my-zsh plugins/catimg"
  , bundle "robbyrussell/oh-my-zsh plugins/cp"
  , bundle "zsh-users/zsh-syntax-highlighting"
  , bundle "zsh-users/zaw"
  , bundle "willghatch/zsh-zaw-extras"
  , bundle "willghatch/zsh-snippets"
  , bundle "willghatch/vzsh"
  -- , developFromFileSystem "/home/foo/bar"
  ]

config = AntigenConfiguration bundles

main :: IO ()
main = shelly $ antigen config
