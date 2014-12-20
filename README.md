LambdaNet
=====

LambdaNet is an _in progress_ attempt at a functional "box-of-LEGO" style neural network built around higher order
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

We will walk you through how to use the LambdaNet Haskell library to create, train, and use an XOR network.

### Getting the Training Data

LambdaNet doesn't provide a means of acquiring and packaging the training data. In the following example, we will use some simple training data.

We'll go into a little more detail about each of these elements a bit later. But the entire process can be done in the Haskell REPL ```ghci``` in the main directory:

```
:l docs.hs
let trainData = [
  (fromList [0.0, 0.0], fromList [0.0]),
  (fromList [0.0, 1.0], fromList [1.0]),
  (fromList [1.0, 0.0], fromList [1.0]),
  (fromList [1.0, 1.0], fromList [0.0])
]
```

These are the 4 states an XOR network can be in given two inputs.

### Creating a New Network

The process of creating a new neural network is quite simple:
  - Create a list of layer definitions with a neuron type, neuron count, and connectivity function, and
  - Pass that list of layer definitions into the createNetwork function, along with the weight initialization function and an entropy generator of choice.

```
let g = mkStdGen 4
let l = LayerDefinition sigmoidNeuron 2 connectFully
let l' = LayerDefinition sigmoidNeuron 2 connectFully
let l'' = LayerDefinition sigmoidNeuron 1 connectFully
let network = createNetwork normals g [l, l', l'']
```

Note, the mkStdGen is our (very random) source of entropy. Feel free to exchange it with your favorite entropy
generator of choice.

Et voila, you have a functioning neural network that simply needs to be trained. The network created above has three layers, the first
with three neurons, the second with 4, and the output with 2.

### Training the Network

Although the above creation through layer definitions may seem unfamiliar, LambdaNet does strive to provide some level of familiarity to machine learning practitioners. In order to achieve this, training and predicting with LambdaNet follows a similar naming convention to Scikit Learn -- namely `predict` and `fit` are used as test and train functions, respectively.

However, be aware that you first need to create a trainer (the library provides a backpropagation trainer) before you can use fit:

```
let t = BackpropTrainer 0.3 quadraticCost quadraticCost'
let network' = fit 100 t 2 trainData network
```

### Using the Network

Using the network to predict values is the simplest part of the entire process. Given that you've followed the code above to create and train the network, you can use it to predict values like so:

```
predict (fromList [1.0, 1.0]) network'
```

That's all there is to it. Pass in a vector (which, in this library is a matrix) and a network object to get out a predicted output. Given that the network trained properly, you should get a value close to 1.

### Extending the Library

The entire library is mean to be as extensible as possible and to allow you to create new types of, well, anything.

## Neural Networks in the Browser

### First Time Setup

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

## Our fearless leader
<center>
  <img src="http://fc07.deviantart.net/fs71/f/2013/009/f/a/gabe_newell__the_hero_of_us_all_by_radulfgreyhammer-d5r0ecr.jpg?raw=true" alt="Our fearless leader" height="250"/>
</center>
