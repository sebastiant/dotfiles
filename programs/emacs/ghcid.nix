src: final: prev:
let
  trivialBuild = (prev.emacsPackagesFor prev.emacs29).trivialBuild;
  ghcid = trivialBuild {
    pname = "ghcid";
    version = "0.1.0.0";
    inherit src;
    meta = {
      homepage = "https://github.com/ndmitchell/ghcid";
      description = "Really basic ghcid+stack support in emacs with compilation-mode";
      inherit (final.emacs.meta) platforms;
    };
  };
  overrides = efinal: eprev: { inherit ghcid; };
in {
  emacsPackagesFor = emacs:
    (prev.emacsPackagesFor emacs).overrideScope overrides;
}
