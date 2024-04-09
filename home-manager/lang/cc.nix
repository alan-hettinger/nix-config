{pkgs, ...}: {
  home.packages = with pkgs; [
    libclang
    cmake
    glslang
    gcc
    rtags
  ];
}
