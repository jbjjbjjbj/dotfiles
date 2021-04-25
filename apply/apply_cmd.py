#!/usr/bin/env python3

import os
from pathlib import Path
import argparse

from .resolv import Resolver
from .writer import Writer
from .state import StateFile


class DirReader:
    def __init__(self, config, resolv):
        self.resolv = resolv
        self.config = config

    def read_module(self, name):
        module_root = name

        # Walk down and link dir og file
        for root, dirs, files in os.walk(module_root):
            rootrel = os.path.relpath(root, start=module_root)

            # Check if we can just link this folder
            if rootrel not in self.config["do_not_link"]:
                ok = self.resolv.do_folder_link(name, Path(rootrel))

                if ok:
                    # Do not go further down
                    dirs.clear()
                    continue

            # Link files
            for f in files:
                self.resolv.do_link(name, Path(rootrel, f))


def cmd_args(parser: argparse.ArgumentParser):
    parser.add_argument("modules", nargs="*", help="modules to apply")
    parser.add_argument("-a", "--reapply", help="reapply modules",
                        action="store_true")


def cmd_func(args, config):
    writer = Writer()
    state = StateFile(args.apply_dir)
    resolv = Resolver(args.apply_dir, writer, state,
                      args.override_existing)

    writer.attach_hook(state.dump_state)

    if args.reapply:
        args.modules = state.saved

    reader = DirReader(config, resolv)
    for module in args.modules:
        reader.read_module(module)
        state.add_saved_module(module)

    writer.apply(dry_run=args.dry_run)


cmd_help = "Apply modules from current directory"
