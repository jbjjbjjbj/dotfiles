from pathlib import Path
import json
from enum import Enum
import hashlib
import os


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
            dest = Path(os.path.realpath(str(path)))
            if Path.cwd() in dest.parents:
                return FileState.create_owned(dest)

        if not path.exists():
            return FileState.Unused

        return FileState.Used


class StateFile:
    links = {}
    dirs = {}
    attr_to_save = ["links", "applydir", "dirs"]

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

    def save_to_dict(self):
        all_attr = self.__dict__
        res = {}
        for key in self.attr_to_save:
            res[key] = all_attr[key]
        return res

    def dump_state(self):
        with self.statefile.open("w") as f:
            json.dump(self.save_to_dict(), f)

    def add_dir(self, path: Path, packagename: str):
        # Add to state
        add_or_create(self.dirs, str(path), packagename)

    def add_link(self, dest, packagename):
        self.links[str(dest)] = packagename
