from pathlib import Path
from .writer import Writer
from .state import StateFile, FileState


class Resolver:
    def __init__(self,
                 applydir,
                 writer: Writer,
                 state: StateFile,
                 override: bool):

        self.applydir = Path(applydir)
        self.writer = writer
        self.override = override
        self.state = state

    def check_parent(self, path: Path, packagename):
        """
        Check if parents exists, and if we created them mark them
        with package name
        """

        parent = path.parent
        exists = parent.exists()
        if (not exists) or parent in self.state.dirs:
            self.check_parent(parent, packagename)

            # Add to state
            self.state.add_dir(parent, packagename)

            if not exists:
                self.writer.create_dir(parent)

    def do_link(self, package, ppath: Path):
        dest = Path(self.applydir, ppath)
        dest_state = FileState.check_location(dest)

        if not self.override and not dest_state.can_write():
            # Check if it's a pointer to the correct location
            raise Exception(f"Destination {ppath} already exists")

        # Save the link in the statefile
        self.state.add_link(dest, package)

        self.check_parent(dest, package)

        target_abs = Path.cwd().joinpath(Path(package, ppath))
        if dest_state == FileState.Owned and dest_state.link_intact()\
                and dest_state.links_to() == target_abs:
            return

        if dest_state != FileState.Unused and self.override:
            self.writer.delete(dest)
        self.writer.create_link(target_abs, dest)

    def do_folder_link(self, package, ppath: Path) -> bool:
        self.do_link(package, ppath)
        return True
