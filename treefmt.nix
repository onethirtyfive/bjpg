{ pkgs, ... }:
{
  enableDefaultExcludes = true;

  projectRootFile = "flake.nix";

  programs.nixfmt.enable = true;
  programs.nixfmt.includes = [ "*.nix" ];
  programs.ormolu.enable = true;
  programs.shfmt.enable = true;
}
