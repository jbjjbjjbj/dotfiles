{ config, pkgs, ... }:

let
    unstable = import <unstable> {};
in {
    # Yep makes sense
    programs.home-manager.enable = true;

    home.packages = (with unstable; [
        stow pass gopass fzf entr

        # X stuff required by scripts
        feh rofi xorg.xmodmap xss-lock xclip xorg.xkill
        xdotool dunst xcompmgr scrot libnotify i3lock
        xbindkeys pywal

        # Developing
        gcc go clang-tools shards crystal
        gdb php unstable.ghc stack racket chez rustc cargo rls kak-lsp
        editorconfig-core-c chez
        gh glab

        # Gui applications
        pkgs.firefox pavucontrol evince leafpad inkscape gimp
        meld pinentry  guake
        termite kitty quasselClient wireshark spotify
        vlc mpv xorg.xev vimHugeX
        (mumble.override { pulseSupport = true; })
        qutebrowser mate.caja
        audacity veracrypt xournal
        musescore fractal  ipcalc playerctl
        xarchive pandoc filelight zathura okular

        # Other stuff
        ( texlive.combine { inherit (texlive)
            scheme-medium minted fvextra upquote catchfile xstring framed
            multirow makecell ntheorem cleveref enumitem todonotes
            lastpage biblatex glossaries pgfplots csquotes soul
            mfirstuc xfor datatool mdframed zref needspace placeins
            wrapfig tcolorbox environ listingsutf8 subfiles acmart totpages
            hyperxmp ifmtarg ncctools comment libertine inconsolata newtx
            filecontents pgf-pie mwe
            ;
        })
        biber

        youtube-dl sshpass
        aspell aspellDicts.da aspellDicts.en
        ffmpegthumbnailer tio imagemagick sox poppler_utils
        ffmpeg fortune

        # The python env created in overlay
        pkgs.defaultPythonEnv
    ]) ++ (with pkgs; [
        gnome3.gnome-calendar
        gnome3.gnome-disk-utility gnome3.gnome-system-monitor
        gnome3.cheese gnome3.gnome-calculator liferea
        gnome3.file-roller gnome3.simple-scan
        # Email and calendar
        evolution evolution-data-server evolution-ews

        libreoffice-fresh 
    ]);
}
