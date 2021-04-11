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
        gdb php python-language-server

        # Gui applications
        firefox pavucontrol evince leafpad inkscape gimp 
        meld pinentry gnome3.gnome-system-monitor
        termite quasselClient wireshark spotify
        gnome3.file-roller gnome3.gnome-calculator 
        gnome3.simple-scan vlc mpv xorg.xev
        gnome3.evolution evolution-data-server vimHugeX
        (mumble.override { pulseSupport = true; })
        libreoffice-fresh liferea qutebrowser mate.caja
        audacity gnome3.eog veracrypt xournal

        # Other stuff
        texlive.combined.scheme-medium biber
        youtube-dl sshpass python38Packages.pynvim
        aspell aspellDicts.da aspellDicts.en
        ffmpegthumbnailer tio imagemagick sox poppler_utils

        # The python env created in overlay
        defaultPythonEnv
    ];
}
