{ mkDerivation, base, base16-bytestring, binary, bytestring
, data-bitcode-edsl, data-bitcode-llvm, directory, filepath, ghc
, pretty, stdenv, transformers
}:
mkDerivation {
  pname = "data-bitcode-plugin";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base16-bytestring binary bytestring data-bitcode-edsl
    data-bitcode-llvm directory filepath ghc pretty transformers
  ];
  homepage = "https://github.com/angerman/data-bitcode-plugin#readme";
  description = "bitcode plugin for ghc";
  license = stdenv.lib.licenses.bsd3;
}
