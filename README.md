LambdaNet
=====

LambdaNet is an artificial neural network library written in Haskell
that abstracts network creation, training, and use as higher order
functions. The benefit of this approach is that it provides a framework
in which users can:
  - quickly iterate through network designs by using different functional components
  - experiment by writing small functional components to extend the library

The library comes with a pre-defined set of functions that can be composed
in many ways to operate on real-world data. These will be enumerated later
in the documentation.

## Installation

LambdaNet can be installed through Cabal:

```
cabal update
cabal install LambdaNet
```

## Using LambdaNet

Using LambdaNet to rapidly prototype networks using built-in functions
requires only a minimal level of Haskell knowledge (although getting
the data into the right form may be more difficult). However, extending
the library may require a more in-depth knowledge of Haskell and
functional programming techniques.

You can find a quick example of using the network in `XOR.hs`. Once LambdaNet
is installed, download XOR.hs, and then you can run the file in your REPL to
see the results:

```
runhaskell examples/XOR.hs
```

The rest of this section dissects the XOR network in order to talk about
the design of LambdaNet.

### Training Data

Before you can train or use a network, you must have training data. The
training data is a tuple of vectors, the first value being the input
to the network, and the second value being the expected output.

For the XOR network, the data is easily hardcoded:

```
let trainData = [
  (fromList [0.0, 0.0], fromList [0.0]),
  (fromList [0.0, 1.0], fromList [1.0]),
  (fromList [1.0, 0.0], fromList [1.0]),
  (fromList [1.0, 1.0], fromList [0.0])
]
```

However, for any non-trivial application the most difficult work will be
getting the data in this form. Unfortunately, LambdaNet does not currently
have tools to support data handling.

### Layer Definitions

The first step in creating a network is to define a list of layer
definitions. The type layer definition takes a neuron type, a count of
neurons in the layer, and a connectivity function.

Creating the layer definitions for a three-layer XOR network, with
2 neurons in the input layer, 2 hidden neurons, and 1 output neuron
can be done as:

```
let l = LayerDefinition sigmoidNeuron 2 connectFully
let l' = LayerDefinition sigmoidNeuron 2 connectFully
let l'' = LayerDefinition sigmoidNeuron 1 connectFully
```

#### Neuron Types

A neuron is simply defined as an activation function and its derivative,
and the LambdaNet library provides three built-in neuron types:
  - `sigmoidNeuron` - A neuron with a sigmoid activation function
  - `tanhNeuron` - A neuron with a hyperbolic tangent activation function
  - `recluNeuron` - A neuron with a rectified linear activation function

By passing one of these functions into a LayerDefinition, you can
create a layer with neurons of that type.

#### Connectivity

A connectivity function is a bit more opaque. Currently, the library
only provides `connectFully`, a function which creates a fully
connected feed-forward network.

Simply, the connectivity function takes in the number of neurons in layer l
and the number of neurons in layer l + 1, and returns a boolean matrix
of integers (0/1) that represents the connectivity graph of the layers
-- a 0 means two neurons are not connected and a 1 means they are. The
starting weights are defined later.

### Creating the Network

The `createNetwork` function takes in a random transform, an entropy
generator, and a list of layer definitions, and returns a network.

For the XOR network, the createNetwork function is:

```
let n = createNetwork normals (mkStdGen 4) [l, l', l'']
```

Our source of entropy is the very random: `mkStdGen 4`, which will
always result in the same generator.

#### Random Transforms

The random transform function is a transform that operates on a
stream of uniformly distributed random numbers and returns a stream
of floating point numbers.

Currently, the two defined distributions are:
  - `uniforms` - A trivial function that returns a stream of uniformly distributed random numbers
  - `normals` - A slightly less-trivial function that uses the Box-Muller transform to create a stream of numbers ~ N(0, 1)

Work is being done to offer a student t-distribution, which would require
support for a chi-squared distribution transformation.

### Training the Network

In order to train a network, you must create a new trainer:

```
let t = BackpropTrainer (3 :: Float) quadraticCost quadraticCost'
```

The BackpropTrainer type takes in a learning rate, a cost function, and
its derivative.

The actual training of the network, the `fit` function uses the trainer, a
network, and the training data, and returns a new, trained network.
For the XOR network, this is:

```
let n' = trainUntilErrorLessThan n t dat 0.01
```

LambdaNet provides three training methods:
  - `trainUntil`
  - `trainUntilErrorLessThan`
  - `trainNTimes`

The `trainUntil` function takes a TrainCompletionPredicate (check Network/Trainer.hs)
for more information, and the last two are simply wrappers for the first one that
provide specific predicates.

#### Selection Functions

The first argument to the `fit` function is a selection function, which
breaks the shuffled training data into batches of a specified size.
They are not currently fully supported, and progress is discussed a little
later.

### Using the Network

Once the network is trained, you can use it with your test data or
production data:

```
predict (fromList [1, 0]) n'
```

LambdaNet at least attempts to follow a Scikit-Learn style naming scheme
with `fit` and `predict` functions.

### Storing and Loading

Once a network has been trained, the weights and biases can be stored in
a file:

```
saveNetwork "xor.ann" n'
```

By calling `saveNetwork` with a file path, you can save the state of the
network.

Loading a network requires passing in a list of layer definitions
for the original network, but will load all the weights and biases of the
saved network:

```
n'' <- loadNetwork "xor.ann" [l, l', l'']
```

Note that the loadNetwork function returns an IO (Network), you can't simply
call predict or train on the object returned by loadNetwork. Using the
approach in XOR.hs should allow you to work with the returned object.

## Currently Under Development

What has been outlined above is only the first stages of LambdaNet. I intend
to support some additional features, such as:
  - Selection functions
  - Regularization functions
  - Additional trainer types (RProp, RMSProp)
  - Additional cost functions

### Selection Functions

I am close to providing full support for selection functions, which
break up a dataset for each round of training. The currently provided
selection functions are:
  - `minibatch n` - You must provide an n and partially apply it to minibatch to get a valid selection function. This function updates the network after every n passes.
  - `online` - Using this function means that the network updates after every training example.

### Regularization Functions and Momentum

Standard backprop training is subject to overfitting and falling into local
minima. By providing support for regularization and momentum, LambdaNet
will be able to provide more extensible and robust training.

## Generating the Documentation Images

All the documentation for the network was generated in the following manner. In the docs folder, run:

```
runhaskell docs.hs
python analysis.py
```

Note that I am currently working on removing the Python image analysis
from the library, and switching it with Haskell and gnuplot. I'm also
working on using the generated images in network documentation.
