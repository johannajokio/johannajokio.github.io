![screenshot](screenshot.png)

If you don't have Haskell in your system you can install it using [GHCup](https://www.haskell.org/ghcup/).

Run `cabal new-run blog` for help :)

To deploy use `./deploy.sh` from the *master* branch. This script replaces the current website stored on the *pages* orphan branch with the latest build found on *master*.

