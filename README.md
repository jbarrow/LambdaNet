LambdaNet
=====

### Using LambdaNet

Todo

### Documentation

Todo

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
