{ pkgs ? import <nixpkgs> {} }:
let pp = import ./reach-env-pin.nix {};
in
(pp.buildFHSUserEnv {
  name = "reach-fhs-env";
  targetPkgs = pkgs: import ./reach-env-packages.nix {};
  profile = with pp; ''
    # The prisma package needs these variables set to work, or it complains about not having precompiled binaries for nixos.
    export PRISMA_MIGRATION_ENGINE_BINARY="${prisma-engines}/bin/migration-engine"
    export PRISMA_QUERY_ENGINE_BINARY="${prisma-engines}/bin/query-engine"
    export PRISMA_QUERY_ENGINE_LIBRARY="${prisma-engines}/lib/libquery_engine.node"
    export PRISMA_INTROSPECTION_ENGINE_BINARY="${prisma-engines}/bin/introspection-engine"
    export PRISMA_FMT_BINARY="${prisma-engines}/bin/prisma-fmt"
    # Yarn seems to need this option to work for me...
    export NODE_OPTIONS=--openssl-legacy-provider
  '';
  runScript = ./reach-post-env.sh;
}).env
