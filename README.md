# shy

Safely build shell one-liners with live feedback!

[![asciicast](https://asciinema.org/a/450836.svg)](https://asciinema.org/a/450836)

## Features

- See live output of your shell command as you're writing it.
- 'Dangerous' commands are printed but not run (currently `mv`, `cp`, and `rm`).
- Currently supports Bash, Fish, and Zsh.

## Usage

Run `shy` and start typing your shell command. The output of the command is printed below, and will refresh automatically if you make changes. Once you're happy press escape, enter, or Ctrl+C to exit. The final version of your shell one-liner will be printed.

## Installation

Using [nix][] (without flakes):

```sh
$ nix-env -if https://github.com/jwoudenberg/shy/archive/main.tar.gz
```

Using [nix][] (with flakes):

```sh
$ nix profile install github:jwoudenberg/shy
```

## Building

The easiest way to get the tooling is using [direnv][] and [nix][].
After installing these tools, in the root of the shy repository, run:

```sh
$ direnv allow    # Add the dev tooling to your local shell.
$ cabal build     # Compile the code.
$ cabal run       # Compile, then run the current version of the code.
$ ghcid           # Start a code watcher providing live feedback while you work.
$ hpack           # Update shy.cabal after making changes to package.yaml.
```

[direnv]: https://direnv.net/
[nix]: https://nixos.org/download.html
