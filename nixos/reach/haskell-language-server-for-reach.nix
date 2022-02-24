{stdenv, lib, pkgs, fetchurl}:

# Reach has a specific version of GHC pinned, and I need a version of the haskell language server that (A) supports that version of GHC at all, and (B) was actually built against that version.  So let's just download the latest binary release that fits that.

stdenv.mkDerivation rec {
  name = "mo-${version}";
  version = "2.3.3";
  src = fetchurl {
    url = "https://github.com/haskell/haskell-language-server/releases/download/1.4.0/haskell-language-server-Linux-8.10.4.gz";
    sha256 = "0fvjgxgg070qzb8p349j616rglry8yimdb8wj1viazdlkgkkfb0k";
  };
  builder = "{bash}/bin/bash";
  args = [./haskell-language-server-for-reach-builder.sh "${version}"];

}

