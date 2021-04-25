import argparse
import yaml

from . import apply_cmd
from . import status_cmd
from . import remove_cmd


def parse_config(path):
    config = {}
    with open(path, "r") as f:
        config = yaml.safe_load(f)

    return config


sub_cmds = {"apply": apply_cmd, "status": status_cmd, "remove": remove_cmd}

parser = argparse.ArgumentParser()
parser.add_argument("--apply-dir", "-a", default=None,
                    help="Directory to load dots from")
parser.add_argument("--dry-run", "-n", default=False,
                    help="Do not make filesystem changes",
                    action="store_true")
parser.add_argument("--override-existing", "-o", default=False,
                    help="Override existing files,dirs,links",
                    action="store_true")


sub_parsers = parser.add_subparsers(dest="cmd", help="Action")
for name, cmd in sub_cmds.items():
    sub = sub_parsers.add_parser(name, help=cmd.cmd_help)
    cmd.cmd_args(sub)

args = parser.parse_args()

if args.apply_dir is None:
    args.apply_dir = ".."
else:
    args.apply_dir = str(args.apply_dir)

if args.cmd in sub_cmds:
    config = parse_config("config.yaml")
    sub_cmds[args.cmd].cmd_func(args, config)
else:
    parser.print_usage()
