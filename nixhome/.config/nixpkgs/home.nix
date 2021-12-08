{ config, pkgs, ... }:

let
    unstable = import <unstable> {};
in {
    # Yep makes sense
    programs.home-manager.enable = true;

    home.packages = with unstable; [
        stow pass gopass fzf entr

        # X stuff required by scripts
        feh rofi xorg.xmodmap xss-lock xclip xorg.xkill
        xdotool dunst xcompmgr scrot libnotify i3lock
        xbindkeys pywal

        # Developing
        gcc go clang-tools unstable.haskell-language-server shards crystal
        gdb php unstable.ghc stack racket chez rustc cargo rls kak-lsp
        editorconfig-core-c

        # Email and calendar
        evolution gnome3.gnome-calendar evolution-data-server evolution-ews

        # Gui applications
        pkgs.firefox pavucontrol evince leafpad inkscape gimp
        meld pinentry gnome3.gnome-system-monitor guake
        termite quasselClient wireshark spotify
        gnome3.file-roller gnome3.gnome-calculator 
        gnome3.simple-scan vlc mpv xorg.xev vimHugeX
        (mumble.override { pulseSupport = true; })
        libreoffice-fresh liferea qutebrowser mate.caja
        audacity gnome3.eog veracrypt xournal remmina
        musescore fractal gnome3.gnome-disk-utility ipcalc playerctl
        xarchive gtkwave gnome3.cheese pandoc filelight zathura

        # Other stuff
        ( texlive.combine { inherit (texlive)
            scheme-medium minted fvextra upquote catchfile xstring framed
            multirow makecell ntheorem cleveref enumitem todonotes
            lastpage biblatex glossaries pgfplots csquotes soul
            mfirstuc xfor datatool mdframed zref needspace placeins
            wrapfig tcolorbox environ listingsutf8 subfiles acmart totpages
            hyperxmp ifmtarg ncctools comment libertine inconsolata newtx;
        })
        biber

        youtube-dl sshpass python38Packages.pynvim
        aspell aspellDicts.da aspellDicts.en
        ffmpegthumbnailer tio imagemagick sox poppler_utils
        ffmpeg fortune

        # The python env created in overlay
        pkgs.defaultPythonEnv
    ];
}
