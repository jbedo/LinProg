{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "lp_solve-${version}";
  version = "5.5.2.0";

  src = fetchurl {
    url = "mirror://sourceforge/lpsolve/lpsolve/${version}/lp_solve_${version}_source.tar.gz";
    sha256 = "176c7f023mb6b8bfmv4rfqnrlw88lsg422ca74zjh19i2h5s69sq";
  };

  buildPhase = ''
    cd lpsolve55
    so=1 sh ccc
  '';

  installPhase = ''
    mkdir -p $out/include
    cp ../*.h $out/include

    mkdir -p $out/lib
    cp bin/ux64/* $out/lib
  '';
}
