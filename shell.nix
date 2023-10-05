{ pkgs ? import <nixpkgs> {} }:

let
  # URL to your Bash script
  installScriptURL = "https://github.com/QB64Official/qb64/raw/master/setup_lnx.sh";
in

pkgs.mkShell {
  buildInputs = [
    pkgs.opengl
    pkgs.alsaLib
    pkgs.gcc
  ];

  shellHook = ''
    # Download and execute the remote Bash script
    curl -o install_tool.sh "${installScriptURL}"
    chmod +x install_tool.sh
    ./install_tool.sh
  '';
}
