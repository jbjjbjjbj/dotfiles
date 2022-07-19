{ config, pkgs, ... }:

let
    unstable = import <unstable> {};
    osu-nixos = (import (builtins.fetchGit { url = "https://github.com/notgne2/osu-nixos.git"; rev = "2df43333e6ab7cd9d839607acd65645567d75536"; }));
in {
    # Yep makes sense
    programs.home-manager.enable = true;

    home.packages = (with unstable; [
        pass fzf entr

        # X stuff required by scripts
        feh rofi xorg.xmodmap xss-lock xclip xorg.xkill
        xdotool dunst xcompmgr scrot libnotify i3lock
        xbindkeys

        # Developing
        gcc go clang-tools
        gdb php unstable.ghc stack racket chez rustc cargo rls kak-lsp
        editorconfig-core-c chez
        
        # Gui applications
        pkgs.firefox pavucontrol evince leafpad inkscape gimp meld pinentry
        termite wireshark spotify vlc mpv xorg.xev vimHugeX
        (mumble.override { pulseSupport = true; })
        qutebrowser mate.caja audacity xournal ipcalc playerctl sxiv
        xarchive pandoc okular

        # Other stuff
        texlive.combined.scheme-full

        youtube-dl
        aspell aspellDicts.da aspellDicts.en aspellDicts.nl
        ffmpegthumbnailer tio imagemagick sox poppler_utils
        ffmpeg fortune

        osu-nixos.packages.${pkgs.system}.osu-lazer

        # The python env created in overlay
        pkgs.defaultPythonEnv
    ]) ++ (with pkgs; [
        gnome3.gnome-calendar
        gnome3.gnome-disk-utility gnome3.gnome-system-monitor
        gnome3.cheese gnome3.gnome-calculator
        gnome3.file-roller gnome3.simple-scan
        # Email and calendar
        # evolution evolution-data-server evolution-ews

        libreoffice-fresh 
    ]);
}
