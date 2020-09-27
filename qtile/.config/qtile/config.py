from typing import List  # noqa: F401

from libqtile import bar, layout, widget
from libqtile.widget import base
from libqtile.config import Click, Drag, Group, Key, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod1"
terminal = guess_terminal()
prompt = ["rofi", "-show", "drun"]
extrap = ["Scripts/lc", "--dmenu"]

cmd_back_inc = ["xbacklight", "-inc", "5"]
cmd_back_dev = ["xbacklight", "-dec", "5"]
cmd_vol_down = ["pactl", "set-sink-volume", "@DEFAULT_SINK@", "-5%"]
cmd_vol_up =   ["pactl", "set-sink-volume", "@DEFAULT_SINK@", "+5%"]
cmd_vol_mute = ["pactl", "set-sink-mute", "@DEFAULT_SINK@", "toggle"]

def latest_group(qtile):
    qtile.current_screen.set_group(qtile.current_screen.previous_group)

keys = [
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(),
        desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    Key([mod], "Tab", lazy.function(latest_group), desc="Alt tab like thing"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "i", lazy.spawn(terminal), desc="Launch terminal"),

    # Toggle between different layouts as defined below
    Key([mod], "u", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "m", lazy.group.setlayout("max")),
    Key([mod], "t", lazy.group.setlayout("columns")),

    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),

    Key([mod, "shift"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod], "e", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "d", lazy.spawn(prompt),
        desc="Spawn a command using a prompt widget"),
    Key([mod], "semicolon", lazy.spawn(extrap),
        desc="Spawn a prompt with common options"),

    # Media keys
    Key([], "XF86MonBrightnessUp", lazy.spawn(cmd_back_inc)),
    Key([], "XF86MonBrightnessDown", lazy.spawn(cmd_back_dev)),
    Key([], "XF86AudioRaiseVolume", lazy.spawn(cmd_vol_up)),
    Key([], "XF86AudioLowerVolume", lazy.spawn(cmd_vol_down)),
    Key([], "XF86AudioMute", lazy.spawn(cmd_vol_mute)),
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(i.name)),
        # Or, use below if you prefer not to switch to that group.
        # # mod1 + shift + letter of group = move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
        #     desc="move focused window to group {}".format(i.name)),
    ])

layouts = [
    layout.Columns(border_focus_stack='#d75f5f'),
    layout.Max(),
    # layout.Tile(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='monospace',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

gconfig = {
        "update_interval": 5,
        }

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(),
                widget.CurrentLayout(),
                widget.WindowName(),
                # widget.Memory(),
                # widget.PulseVolume(),
                widget.Volume(fmt="V:[{}]", **gconfig),
                widget.Battery(fmt="B:[{}]", format="{char},{percent:2.0%},{watt:.2f}W", **gconfig),
                widget.Clock(format='%Y-%m-%d %a %H:%M', **gconfig),
                widget.Systray(),
                widget.QuickExit(default_text="[s]", countdown_format="[{}]"),
            ],
            24,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
