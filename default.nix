{ pkgs ? import <nixpkgs> {} }: with pkgs;
let
  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs;[  ]);
in
stdenv.mkDerivation rec {
  version = "0.1";
  pname = "f4visual";
  src = ./.;
  buildInputs = [
    ghc
  ];
  # buildPhase = "ghc --make xmonadctl.hs";
  # installPhase = ''
  #   mkdir -p $out/bin
  #   cp xmonadctl $out/bin/
  #   chmod +x $out/bin/xmonadctl
  # '';
  meta = with lib; {
    author = "Luca Leon Happel";
    description = "Programm zum Visualisieren des PF_4^2";
    homepage = "https://github.com/Quoteme/prog";
    platforms = platforms.all;
    mainProgram = "prog";
  };  
}
