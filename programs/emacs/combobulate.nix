src: final: prev:
let
  trivialBuild = (prev.emacsPackagesFor prev.emacs29).trivialBuild;
  combobulate = trivialBuild {
    pname = "combobulate";
    version = "0.1.0.0";
    inherit src;
    meta = {
      homepage = "https://github.com/mickeynp/combobulate";
      description = "Structured Navigation and Editing with Combobulate";
      inherit (final.emacs.meta) platforms;
    };
  };
  overrides = efinal: eprev: { inherit combobulate; };
in {
  emacsPackagesFor = emacs:
    (prev.emacsPackagesFor emacs).overrideScope overrides;
}
