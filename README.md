# propellor-game

Here is a fork of https://git.joeyh.name/index.cgi/secret-project.git/
concept of new Debian-installer from Joe Hess.

This repository created mainly to reproduce funny game that Joe showed
on his [presentation](http://joeyh.name/blog/entry/unifying_OS_installation_and_configuration_management/).

I use `GHC-8.2.1` now and `cabal-install-2.0.0.0`, so some tweaks are needed. Hope, that with `stack` all
will work out of the box. Try for that original [https](https://git.joeyh.name/git/secret-project.git) or 
[git](git://git.joeyh.name/secret-project.git) repo and run there

```
stack build
stack exec propellor-config --test-ui
```
