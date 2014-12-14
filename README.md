LambdaNet
=====

LambdaNet is an attempt at a functional "box-of-LEGO" style neural network built around higher order
functions. The goal is to provide a framework that promotes experimentation with neural networks by
allowing users to define custom:
  - Neuron types
  - Layer connectivities
  - Weight initialization functions
  - Cost functions
  - Regularization functions
  - Trainers

It comes with a pre-defined, and pretty standard, set of all of the above so you can quickly get
started building networks for use on real data sets.

## Using LambdaNet

TODO

# Neural Networks in the Browser

## First Time Setup

Install [leiningen](http://leiningen.org).
  1. Download the [lein script](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein)
  2. Place it on your $PATH where your shell can find it (eg. `~/bin`)
  3. Set it to be executable (`chmod a+x ~/bin/lein`)
  4. Run it (`./lein`) and it will download the self-install package

### Build and run frontend
```
  cd frontend
  lein cljsbuild once
  lein ring server
```
The front end will then be running on port 3000.

### Routes
```
/create
```
Takes a vector of layer config objects and a weight initialization function key and returns a vector of weight matrices.

```
/train
```
Takes a vector of weight matrices, a list of training inputs, and a list of expected outputs. Returns a trained vector of weight matrices.

```
/evaluate
```
Takes a vector of weight matrices and input data and returns the result of running the input data through the network represented by the vector of weight matrices.

# Documentation

The in-depth documentation covering much of the math behind the network can be found at

## Generating the Documentation Images:

All the documentation for the network was generated in the following manner. First, from the Haskell REPL, use the following commands:

```
  :l docs.hs
  writeDat "docs/sigmoid.txt" (computeApproximation sigmoid) 5
  writeDat "docs/reclu.txt" (computeApproximation reclu) 5
  writeDat "docs/tanh.txt" (computeApproximation tanh) 5
  writeDat "docs/derivative_sigmoid.txt" (computeApproximation sigmoid') 5
  writeDat "docs/derivative_reclu.txt" (computeApproximation reclu') 5
  writeDat "docs/derivative_tanh.txt" (computeApproximation tanh') 5

  let g = mkStdGen 4
  writeDat "docs/normal.txt" (take 20000 (randomList normals g)) 5
  writeDat "docs/uniform.txt" (take 20000 (randomList uniforms g)) 5
```

Then, in the docs folder, run:

```
  python analysis.py
```

### Building a Network

In the Haskell REPL ```ghci``` in the main directory,

```
  :l docs.hs
  let g = mkStdGen 4
  let l = LayerDefinition sigmoidNeuron 3 connectFully
  let l' = LayerDefinition sigmoidNeuron 4 connectFully
  let l'' = LayerDefinition sigmoidNeuron 2 connectFully
  let network = createNetwork normals g [l, l'', l''']
```

## Our fearless leader
<p>
  <img src="http://fc07.deviantart.net/fs71/f/2013/009/f/a/gabe_newell__the_hero_of_us_all_by_radulfgreyhammer-d5r0ecr.jpg?raw=true" alt="Our fearless leader" height="250"/>
</p>
