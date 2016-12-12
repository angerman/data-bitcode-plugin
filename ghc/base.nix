{ stdenv, fetchurl, fetchgit, bootPkgs, perl, gmp, ncurses, libiconv, binutils, coreutils
, autoconf, automake, happy, alex
, ghcVersion, ghcRevision, ghcSha256
, ghcDiffs ? []
, ghcFlavour ? null
}:

let
  inherit (bootPkgs) ghc;

  commonBuildInputs = [ ghc perl autoconf automake happy alex ];

  version = ghcVersion;
  rev     = ghcRevision;

  commonPreConfigure =  ''
    sed -i -e 's|-isysroot /Developer/SDKs/MacOSX10.5.sdk||' configure
  '' + stdenv.lib.optionalString (!stdenv.isDarwin) ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -rpath $out/lib/ghc-${version}"
  '' + stdenv.lib.optionalString stdenv.isDarwin ''
    export NIX_LDFLAGS+=" -no_dtrace_dof"
  '' + stdenv.lib.optionalString (ghcFlavour != null) ''
    cp mk/build.mk.sample mk/build.mk
    sed -i -e 's|^#BuildFlavour = ${ghcFlavour}$|BuildFlavour = ${ghcFlavour}|' mk/build.mk
  ''
  ;

  # download diffs from phabricator.
  # (diff, id) should uniquley identify the diff.
  fetchDiff = { diff, id, sha256 }: fetchurl {
    inherit sha256;
    name = "D${toString diff}";
    url = "https://phabricator.haskell.org/D${toString diff}?id=${toString id}&download=true";
    curlOpts = "--location"; # follow 3XX location responses
  };

in stdenv.mkDerivation (rec {
  inherit version rev;

  name = "ghc-${version}";

  src = fetchgit {
    url = "git://git.haskell.org/ghc.git";
    inherit rev;
    sha256 = ghcSha256;
  };

  patches = map fetchDiff ghcDiffs;

  postPatch = ''
    echo ${version} >VERSION
    echo ${rev} >GIT_COMMIT_ID
    patchShebangs .
    ./boot
  '';

  buildInputs = commonBuildInputs;

  enableParallelBuilding = true;

  preConfigure = commonPreConfigure;

  configureFlags = [
    "CC=${stdenv.cc}/bin/cc"
    "--with-gmp-includes=${gmp.dev}/include" "--with-gmp-libraries=${gmp.out}/lib"
    "--with-curses-includes=${ncurses.dev}/include" "--with-curses-libraries=${ncurses.out}/lib"
  ] ++ stdenv.lib.optional stdenv.isDarwin [
    "--with-iconv-includes=${libiconv}/include" "--with-iconv-libraries=${libiconv}/lib"
  ];

  dontStrip = stdenv.lib.optional (stdenv.isDarwin) true;
  # required, because otherwise all symbols from HSffi.o are stripped, and
  # that in turn causes GHCi to abort
  stripDebugFlags = [ "-S" ] ++ stdenv.lib.optional (!stdenv.isDarwin) "--keep-file-symbols";

  postInstall = ''
    # Install the bash completion file.
    install -D -m 444 utils/completion/ghc.bash $out/share/bash-completion/completions/ghc

    # Patch scripts to include "readelf" and "cat" in $PATH.
    for i in "$out/bin/"*; do
      test ! -h $i || continue
      egrep --quiet '^#!' <(head -n 1 $i) || continue
      sed -i -e '2i export PATH="$PATH:${stdenv.lib.makeBinPath [ binutils coreutils ]}"' $i
    done
  '';

  passthru = {
    inherit bootPkgs;
  };

  meta = {
    homepage = "http://haskell.org/ghc";
    description = "The Glasgow Haskell Compiler";
    maintainers = with stdenv.lib.maintainers; [ marcweber andres peti ];
    inherit (ghc.meta) license platforms;
  };

})
