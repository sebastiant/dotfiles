Config { font = "xft:iosevka-regular:pixelSize=14:antialias=true"
       , additionalFonts = []
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Network "wlp0s20f3" ["-L","0","-H","32",
                                          "--normal","green","--high","red"] 10
                    , Run Cpu ["-L","3","-H","50",
                               "--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    , Run Kbd [("se", "SE"), ("us", "US")]
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }{%kbd% | <fc=#ee9a00>%date%</fc> | %wlp0s20f3% | %cpu% | %memory% %swap% "
       }

