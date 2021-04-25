import argparse
from .state import StateFile, FileState
from pathlib import Path
from tabulate import tabulate


def cmd_args(_: argparse.ArgumentParser):
    pass


def cmd_func(args, config):
    state = StateFile(args.apply_dir)
    rows = []
    for link in state.links:
        st = FileState.check_location(Path(link))
        rows.append((link, st.check_string(Path.cwd())))

    print(tabulate(rows, headers=["Link", "Status"]), "\n")

    rows = state.dirs.items()
    print(tabulate(rows, headers=["Directory", "Owned by"]))


cmd_help = "Check status on owned links"
