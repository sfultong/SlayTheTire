{ rev ? "95a8cb3ade1ad0e2649f0f3128e90c53964af5e1",
  outputSha256 ? "0jxn25l8d65h6qnmx9f4dsi2fscqyxc0gvhnkj1bc7kicn9rr5hj",
  enableLLVMAssertions ? true
}:
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = outputSha256;
  };
  pkgs = import nixpkgs {
  };

  haskellPkgs = with pkgs.haskell.lib; pkgs.haskell.packages.ghc843.override(old: {
    all-cabal-hashes = builtins.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/cc4387cface30e2720afff1cd7c412c43317e996.tar.gz";
      sha256 = "16agk33n7kzz5hdjq805mpdcv0cvgxqkvjb5ipq7bn7ahqw0lfil";
    };
    overrides = self: super: {
      slay = super.callCabal2nix "slay" ./. {};
    };
  });
in
haskellPkgs.shellFor {
  packages = p: [p.slay];
}
