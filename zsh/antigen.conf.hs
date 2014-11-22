{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module MyAntigen where

import Antigen (AntigenConfiguration (..)
              , bundle
              , antigen
              , ZshPlugin (..)
              , antigenSourcingStrategy
              , filePathsSourcingStrategy
              , developFromFileSystem)
import Shelly (shelly)

bundles =
  [ bundle "zsh-users/zsh-history-substring-search"
  , bundle "hchbaw/opp.zsh.git"
  , (bundle "alfredodeza/zsh-plugins.git")
     {sourcingStrategy = filePathsSourcingStrategy ["vi/zle_vi_visual.zsh"]}
  , (bundle "robbyrussell/oh-my-zsh") { sourcingLocations =
        [ "plugins/cabal"
        , "plugins/catimg"
        , "plugins/cp"
        ]}
  , bundle "zsh-users/zsh-syntax-highlighting"
  , bundle "zsh-users/zaw"
  , bundle "willghatch/zsh-zaw-extras"
  , bundle "willghatch/zsh-snippets"
  , bundle "willghatch/vzsh"
  , bundle "tarrasch/zsh-colors"
  , bundle "tarrasch/zsh-bd"
  , bundle "tarrasch/zsh-functional"
  , bundle "voronkovich/gitignore.plugin.zsh"
  , bundle "jocelynmallon/zshmarks"
  , bundle "skx/sysadmin-util"
  -- , developFromFileSystem "/home/foo/bar"
  ]

config = AntigenConfiguration bundles

main :: IO ()
main = shelly $ antigen config
