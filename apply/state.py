from pathlib import Path
import json
from enum import Enum
import hashlib
import os

from .writer import Writer


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

    def link_intact(self) -> bool:
        return Path(self.links_to()).exists()

    def check_string(self, dotdir: Path) -> str:
        if self == FileState.Unused:
            return "Free"
        elif self == FileState.Owned:
            status = "Intact" if self.link_intact() else "Broken"
            return f"Owned by {self.owner(dotdir)} ({status})"
        else:
            return "Occupied"

    def owner(self, dotdir: Path) -> str:
        to = Path(self.links_to())
        return to.relative_to(dotdir).parts[0]

    @staticmethod
    def create_owned(links_to: Path) -> "FileState":
        s = FileState.Owned
        s.links_to_path = links_to
        return s

    @staticmethod
    def check_location(path: Path) -> "FileState":
        if path.is_symlink():
            dest = Path(os.readlink(str(path)))
            if Path.cwd() in dest.parents:
                return FileState.create_owned(dest)
            else:
                return FileState.Used

        if not path.exists():
            return FileState.Unused

        return FileState.Used


class StateFile:
    links = {}
    dirs = {}
    saved = []
    attr_to_save = ["links", "applydir", "dirs", "saved"]

    def __init__(self, applydir):
        # Generate unique string for each possible applydir
        ustr = hashlib.md5(applydir.encode("utf-8")).hexdigest()[10:]
        self.applydir = str(applydir)

        self.statefile = Path(f"state_{ustr}.json")
        if self.statefile.exists():
            with self.statefile.open("r") as f:
                self.set_from_dict(json.load(f))
        else:
            self.set_from_dict({})

        self.stateclean = True

    def set_from_dict(self, state):
        self.links = state.get("links", {})
        self.dirs = state.get("dirs", {})
        self.applydir = state.get("applydir", self.applydir)
        self.saved = state.get("saved", [])

    def save_to_dict(self):
        all_attr = self.__dict__
        res = {}
        for key in self.attr_to_save:
            res[key] = all_attr[key]
        return res

    def dump_state(self, dry_run):
        if dry_run:
            return
        with self.statefile.open("w") as f:
            json.dump(self.save_to_dict(), f)

    def add_dir(self, path: Path, modulename: str):
        # Add to state
        add_or_create(self.dirs, str(path), modulename)

    def add_link(self, dest, modulename):
        self.links[str(dest)] = modulename

    def add_saved_module(self, modulename):
        if modulename not in self.saved:
            self.saved.append(modulename)

    def is_saved(self, modulename):
        return modulename in self.saved

    def remove_saved_module(self, modulename):
        if modulename in self.saved:
            self.saved.remove(modulename)

    def __delete_dir(self, writer: Writer, path: str, module):
        self.dirs[path].remove(module)

        if len(self.dirs[path]) == 0:
            writer.delete(Path(path))

    def __delete_link(self, writer: Writer, path: str):
        del self.links[path]

        if FileState.check_location(Path(path)) == FileState.Owned:
            writer.delete(Path(path))

    def remove_by_condition(self, cond, writer: Writer):
        writer.attach_hook(self.dump_state)
        links = list(self.links.keys())
        for link in links:
            if cond(self.links[link]):
                self.__delete_link(writer, link)

        dirs = list(self.dirs.keys())
        for directory in dirs:
            modules = self.dirs[directory]
            for module in modules:
                if cond(module):
                    self.__delete_dir(writer, directory, module)
