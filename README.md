# propellor-game

Here is a fork of https://git.joeyh.name/index.cgi/secret-project.git/, the
concept of a new Debian-installer from [Joe Hess](https://joeyh.name/).

This repository is created mainly to reproduce a funny game in browser that Joe showed
on his [presentation](http://joeyh.name/blog/entry/unifying_OS_installation_and_configuration_management/).

I use `GHC-8.2.1` now and `cabal-install-2.0.0.0`, so some tweaks are needed. Hope, that with `stack` all
will work out of the box. Try for that the original [https](https://git.joeyh.name/git/secret-project.git) or 
git://git.joeyh.name/secret-project.git repo and run there

```
stack build
stack exec propellor-config --test-ui
```

To deal with `cabal`, we have to download [modified by Joe version](https://github.com/alogic0/threepenny-gui-joeyh) of `three-penny-gui`, where I additionally removed the upper boundaries for dependencies, and install it.

```
cd threepenny-gui-joeyh
cabal install --only-dep
cabal install
```

Then `cd` to the `propellor-game`, build it and run.

```
cabal install --only-dep
cabal build
./dist/build/propellor-game/propellor-game
```

The game waits you on the address http://127.0.0.1:8023


