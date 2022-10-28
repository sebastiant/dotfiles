final: prev: {
  zoom-us = prev.zoom-us.overrideAttrs (oldAttrs: rec {
    postFixup = oldAttrs.postFixup + ''
      mv $out/bin/{zoom,zoom-x11}
      makeWrapper $out/bin/zoom-x11 $out/bin/zoom \
        --unset XDG_SESSION_TYPE
    '';
  });

  slack = prev.slack.overrideAttrs (oldAttrs: rec {
    installPhase = oldAttrs.installPhase + ''
      mv $out/bin/{slack,slack-x11}
      makeWrapper $out/bin/slack-x11 $out/bin/slack \
        --add-flags "--enable-features=WebRTCPipeWireCapturer"
    '';
  });
}
