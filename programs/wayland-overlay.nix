final: prev: {
  slack = prev.slack.overrideAttrs (oldAttrs: rec {
    installPhase = oldAttrs.installPhase + ''
      mv $out/bin/{slack,slack-x11}
      makeWrapper $out/bin/slack-x11 $out/bin/slack \
        --add-flags "--enable-features=WebRTCPipeWireCapturer"
    '';
  });
}
