# newDotFiles

This is where i keep my dotfiles and scripts.

## Dotfile Management

Dotfiles are stored in modules which map to the home directory.
For example does the file `vim-core/.vim/vimrc` map to the file `~/.vim/vimrc`, where `vim-core` is the module.
This means that different configurations can be selectively added.

Modules are applied with the included `apply` python module.
To apply the two vim modules run:

```bash
python -m apply apply vim-core vim-extra
```

If changes have been made to modules, they can be reapplied with:

```bash
python -m apply apply -a
```

File ownership status can be queried with:

```bash
python -m apply status
```

## Ignored Folders

The file `config.yaml` specifies the directories which must not be linked.
These are directories such as `~/.config` or `~` itself.

## Stored State

State and ownership of files in the home directory are saved in the `state_*.json` files.
A hash has been added to enable multiple target directies, without them interfering.

## Bugs and Missing Features in apply

- [ ] Clean up unused files
- [ ] Do not crash on `python -m apply remove`
- [ ] Make launching apply software easier
