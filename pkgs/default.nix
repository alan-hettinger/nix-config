{pkgs, ...}: {
  # roll = pkgs.callPackage ./roll {};
  environment.systemPackages = [(pkgs.callPackage ./roll {})];
}
