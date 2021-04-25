import argparse
from .state import StateFile
from .writer import Writer
import copy


def cmd_args(parser: argparse.ArgumentParser):
    parser.add_argument("-u", "--unused",
                        help="remove unused folders and links",
                        action="store_true")
    parser.add_argument("-a", "--all", help="remove all modules",
                        action="store_true")
    parser.add_argument("-m", "--modules", nargs="+", help="specific modules")


def cmd_func(args, config):
    state = StateFile(args.apply_dir)
    if args.all:
        args.modules = copy.copy(state.saved)

    if args.unused:
        def func(mod): return not state.is_saved(mod)
    else:
        def func(mod): return mod in args.modules

    writer = Writer()
    state.remove_by_condition(func, writer)
    if args.modules:
        for module in args.modules:
            state.remove_saved_module(module)

    writer.apply(args.dry_run)


cmd_help = "remove links and files"
