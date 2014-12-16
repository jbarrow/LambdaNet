let l = Layer [[-11.62, 12.88], [10.99, -13.13]] [[-6.06, -7.19]] sigmoidNeuron
let l' = Layer [[13.34], [13.13]] [[-6.56]] sigmoidNeuron
let n = Network [l, l']
predict [[0, 1]] n
