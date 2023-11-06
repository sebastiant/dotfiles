swarm: final: prev:
let
  swarm-mode = final.emacsPackages.trivialBuild {
    pname = "swarm-mode";
    version = "0.3.0.1";

    src = swarm.outPath + "/editors/emacs/";

    buildInputs = [ final.emacsPackages.lsp-mode ];

    meta = {
      homepage = "https://github.com/swarm-game/swarm";
      description = "Resource gathering + programming game";
      inherit (final.emacs.meta) platforms;
    };
  };
  overrides = efinal: eprev: { inherit swarm-mode; };
in {
  emacsPackagesFor = emacs:
    (prev.emacsPackagesFor emacs).overrideScope' overrides;
}
