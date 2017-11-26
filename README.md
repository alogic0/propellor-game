# propellor-game

Here is a fork of https://git.joeyh.name/index.cgi/secret-project.git/, the
concept of a new Debian-installer from [Joe Hess](https://joeyh.name/).

This repository created mainly to reproduce a funny game in browser that Joe showed
on his [presentation](http://joeyh.name/blog/entry/unifying_OS_installation_and_configuration_management/).

I use `GHC-8.2.1` now and `cabal-install-2.0.0.0`, so some tweaks are needed. Hope, that with `stack` all
will work out of the box. Try for that original [https](https://git.joeyh.name/git/secret-project.git) or 
git://git.joeyh.name/secret-project.git repo and run there

```
stack build
stack exec propellor-config --test-ui
```

To deal with `cabal`, we have to download [`three-penny-gui`](http://github.com/joeyh/threepenny-gui/archive/59242cf93bdb8eaa805f5c2b0241e9a1cba9a70f.zip), remove upper boundary for `template-haskell` there and build it in a sandbox.

```
cabal sandbox init --sandbox=$HOME/src/sandbox-three
cabal update
cabal install
```

Then, `cd propellor-game` and use that sandbox running 

```
cabal sandbox init --sandbox=$HOME/src/sandbox-three
cabal install --only-dep
cabal build
./dist/build/propellor-game/propellor-game
```
The game waits you on adress http://127.0.0.1:8023


