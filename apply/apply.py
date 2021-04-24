#!/usr/bin/env python3

import yaml
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

    def read_package(self, name):
        package_root = name

        # Walk down and link dir og file
        for root, dirs, files in os.walk(package_root):
            rootrel = os.path.relpath(root, start=package_root)

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
    parser.add_argument("packages", nargs="*", help="Packages to apply")


def cmd_func(args, config):
    writer = Writer()
    state = StateFile(args.apply_dir)
    resolv = Resolver(args.apply_dir, writer, state,
                      args.override_existing)

    reader = DirReader(config, resolv)
    for pack in args.packages:
        reader.read_package(pack)

    writer.apply(dry_run=args.dry_run)
    if not args.dry_run:
        state.dump_state()


cmd_help = "Apply modules from current directory"
