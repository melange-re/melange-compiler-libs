final: prev:

{
  ocamlPackages = prev.ocamlPackages.overrideScope' (ofinal: oprev:
    {
      melange-compiler-libs = prev.callPackage ./. { };
    });
}
