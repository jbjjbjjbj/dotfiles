#!/usr/bin/env python3

import yaml
import json
import os
from pathlib import Path
import argparse
from enum import Enum
import shutil


class Applier:
    def __init__(self):
        self.links_todo = {}
        self.dirs_todo = []
        self.delete_todo = []

    def create_link(self, target: Path, linkpath: Path):
        if linkpath in self.links_todo:
            prev = self.links_todo[linkpath]
            print(f"Link {linkpath} to {target} replaces previus {prev}")
        self.links_todo[linkpath] = target

    def create_dir(self, path: Path):
        if path in self.dirs_todo:
            return

        self.dirs_todo.append(path)

    def delete(self, path: Path):
        if path in self.delete_todo:
            return

        self.delete_todo.append(path)

    def apply_dir(self, path: Path, dry_run):
        print(f"mkdir {path}")
        if not dry_run:
            path.mkdir()

    def apply_delete(self, path: Path, dry_run):
        if path.is_dir():
            print(f"rmtree {path}!!!")
            if not dry_run:
                shutil.rmtree(str(path))
        else:
            print(f"remove {path}")
            if not dry_run:
                path.unlink()

    def apply_link(self, linkpath: Path, target: Path, dry_run):
        print(f"link {linkpath} -> {target}")
        if not dry_run:
            linkpath.symlink_to(target)

    def apply(self, dry_run=True):
        for d in self.delete_todo:
            self.apply_delete(d, dry_run)

        for d in self.dirs_todo:
            self.apply_dir(d, dry_run)

        for link, target in self.links_todo.items():
            self.apply_link(link, target, dry_run)


def add_or_create(dictio, key, value):
    if key in dictio:
        if value not in dictio[key]:
            dictio[key].append(value)
    else:
        dictio[key] = [value]


class FileState(Enum):
    Unused = 1
    Owned = 2
    Used = 3
    links_to_path: Path = Path()

    def can_write(self) -> bool:
        return self in [FileState.Unused, FileState.Owned]

    def links_to(self) -> str:
        if self is not FileState.Owned:
            raise Exception(f"Cannot call location on {self}")

        return self.links_to_path

    @staticmethod
    def create_owned(links_to: Path) -> "FileState":
        s = FileState.Owned
        s.links_to_path = links_to
        return s


class Resolver:
    def __init__(self, applydir, dotdir, override):
        self.applydir = Path(applydir)
        self.applier = Applier()
        self.dotdir = Path(dotdir)
        self.override = override

        # Load state
        self.statefile = Path("state.json")
        if self.statefile.exists():
            with self.statefile.open("r") as f:
                self.state = json.load(f)
        else:
            self.state = {"dirs": {}, "links": {}}

        self.stateclean = True

    def dump_state(self):
        with self.statefile.open("w") as f:
            json.dump(self.state, f)

    def check_location(self, path: Path) -> FileState:
        if not path.exists():
            return FileState.Unused

        if path.is_symlink():
            dest = Path(os.path.realpath(str(path)))
            if self.dotdir in dest.parents:
                return FileState.create_owned(dest)

        return FileState.Used

    def state_save_link(self, dest, packagename):
        self.state["links"][str(dest)] = packagename

    def check_parent(self, path: Path, packagename):
        """
        Check if parents exists, and if we created them mark them
        with package name
        """

        parent = path.parent
        exists = parent.exists()
        if (not exists) or parent in self.state["dirs"]:
            self.check_parent(parent, packagename)

            # Add to state
            add_or_create(self.state["dirs"], str(parent), packagename)

            if not exists:
                self.applier.create_dir(parent)

    def do_link(self, package, ppath: Path):
        dest = Path(self.applydir, ppath)
        dest_state = self.check_location(dest)

        if not self.override and not dest_state.can_write():
            # Check if it's a pointer to the correct location
            print(os.readlink(dest))
            raise Exception(f"Destination {ppath} already exists")

        # Save the link in the statefile
        self.state_save_link(dest, package)

        self.check_parent(dest, package)

        target_abs = Path.cwd().joinpath(Path(package, ppath))
        if dest_state == FileState.Owned \
                and dest_state.links_to() == target_abs:
            return

        if dest_state != FileState.Unused and self.override:
            self.applier.delete(dest)
        self.applier.create_link(target_abs, dest)

    def do_folder_link(self, package, ppath: Path) -> bool:
        self.do_link(package, ppath)
        return True

    def apply(self, dry_run=True):
        self.applier.apply(dry_run)


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


def parse_config(path):
    config = {}
    with open(path, "r") as f:
        config = yaml.safe_load(f)

    return config


parser = argparse.ArgumentParser()
parser.add_argument("--dot-dir", "-d", default=".",
                    help="Directory to load dots from")
parser.add_argument("--apply-dir", "-a", default=None,
                    help="Directory to load dots from")
parser.add_argument("--dry-run", "-n", default=False,
                    help="Do not make filesystem changes", action="store_true")
parser.add_argument("--override-existing", "-o", default=False,
                    help="Override existing files,dirs,links",
                    action="store_true")
parser.add_argument("packages", nargs="*", help="Packages to apply")

if __name__ == "__main__":
    args = parser.parse_args()

    # Just cd to dotdir, so everything can make the assumption that cwd
    # is dotdir
    os.chdir(args.dot_dir)

    if args.apply_dir is None:
        args.apply_dir = ".."
    else:
        args.apply_dir = str(Path(args.apply_dir).relative_to(args.dot_dir))

    config = parse_config("config.yaml")

    resolv = Resolver(args.apply_dir, Path.cwd(), args.override_existing)

    reader = DirReader(config, resolv)
    for pack in args.packages:
        reader.read_package(pack)

    resolv.apply(dry_run=args.dry_run)
    if args.dry_run:
        resolv.dump_state()
