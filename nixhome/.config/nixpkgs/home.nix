{ config, pkgs, ... }:

{
    # Yep makes sense
    programs.home-manager.enable = true;

    home.packages = with pkgs; [
        stow pass gopass fzf

        # X stuff required by scripts
        feh rofi xorg.xmodmap xss-lock xclip xorg.xkill
        xdotool dunst xcompmgr scrot libnotify i3lock
        xbindkeys

        # Developing
        rustup gcc go clang-tools
        gdb php ghc stack

        # Email and calendar
        evolution gnome3.gnome-calendar evolution-data-server

        # Gui applications
        firefox pavucontrol evince leafpad inkscape gimp 
        meld pinentry gnome3.gnome-system-monitor
        termite quasselClient wireshark spotify
        gnome3.file-roller gnome3.gnome-calculator 
        gnome3.simple-scan vlc mpv xorg.xev vimHugeX
        (mumble.override { pulseSupport = true; })
        libreoffice-fresh liferea qutebrowser mate.caja
        audacity gnome3.eog veracrypt xournal remmina
        musescore fractal gnome3.gnome-disk-utility ipcalc playerctl
        xarchive gtkwave

        # Other stuff
        ( texlive.combine { inherit (texlive)
            scheme-medium minted fvextra upquote catchfile xstring framed
            multirow makecell ntheorem cleveref enumitem todonotes
            lastpage biblatex glossaries pgfplots csquotes soul
            mfirstuc xfor datatool mdframed zref needspace;
        })
        biber
        youtube-dl sshpass python38Packages.pynvim
        aspell aspellDicts.da aspellDicts.en
        ffmpegthumbnailer tio imagemagick sox poppler_utils

        # The python env created in overlay
        defaultPythonEnv
    ];
}
