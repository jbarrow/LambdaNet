LambdaNet
=====

### Using LambdaNet

### First Time Setup

Install the [Haste Compiler](http://haste-lang.org). The second step will take a while, so go get some hard-earned shawarma while you wait.

```
  cabal update
  cabal install hastec
```

Install [leiningen](http://leiningen.org).
  1. Download the [lein script](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein)
  2. Place it on your $PATH where your shell can find it (eg. `~/bin`)
  3. Set it to be executable (`chmod a+x ~/bin/lein`)
  4. Run it (`./lein`) and it will download the self-install package

### Documentation

## Build and run frontend
```
  cd frontend
  lein cljsbuild once
  lein ring server
```
The app will then be running on port 3000.

__Generating the Documentation Images:__

All the documentation for the network was generated in the following manner. First, from the Haskell REPL, use the following commands:

```
  :l docs.hs
  writeDat "docs/sigmoid.txt" (computeApproximation (evaluate Sigmoid)) 5
  writeDat "docs/reclu.txt" (computeApproximation (evaluate Reclu)) 5
  writeDat "docs/logistic.txt" (computeApproximation (evaluate Logistic)) 5
```

Then, in the docs folder, run:

```
  python analysis.py
```

## Our fearless leader
<p>
  <img src="http://fc07.deviantart.net/fs71/f/2013/009/f/a/gabe_newell__the_hero_of_us_all_by_radulfgreyhammer-d5r0ecr.jpg?raw=true" alt="Our fearless leader" height="250"/>
</p>
