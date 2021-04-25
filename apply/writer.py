import shutil
from pathlib import Path


class Writer:
    def __init__(self):
        self.links_todo = {}
        self.dirs_todo = []
        self.delete_todo = []
        self.apply_hook = []

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

    def attach_hook(self, hook):
        self.apply_hook.append(hook)

    def __apply_dir(self, path: Path, dry_run):
        print(f"mkdir {path}")
        if not dry_run:
            path.mkdir()

    def __apply_delete(self, path: Path, dry_run):
        if not path.is_symlink() and path.is_dir():
            print(f"rmtree {path}")
            if not dry_run:
                shutil.rmtree(str(path))
        else:
            print(f"remove {path}")
            if not dry_run:
                path.unlink()

    def __apply_link(self, linkpath: Path, target: Path, dry_run):
        print(f"link {linkpath} -> {target}")
        if not dry_run:
            linkpath.symlink_to(target)

    def apply(self, dry_run=True):
        for d in self.delete_todo:
            self.__apply_delete(d, dry_run)

        for d in self.dirs_todo:
            self.__apply_dir(d, dry_run)

        for link, target in self.links_todo.items():
            self.__apply_link(link, target, dry_run)

        for hook in self.apply_hook:
            hook(dry_run)
