module Main where

type Weight = [Double]
type LearningRate = Double
data Input = Input {
    pattern :: [Double],
    target :: Int
} deriving (Show, Eq)

main :: IO()
main = print $ perceptron [Input [0, 0, 1, 1] 0, Input [1, 1, 1, 1] 1, Input [1, 0, 1, 1] 1, Input [0, 1, 1, 1] 0] [0, 0, 0, 0]

perceptron :: [Input] -> Weight -> Weight
perceptron input weight = if weight == newWeight
                      then
                        newWeight
                      else
                        perceptron input newWeight
                   where
                      newWeight = epoch input weight


epoch :: [Input] -> Weight -> Weight
epoch [] weight = weight
epoch input weight = epoch (tail input) (calculate (head input) weight)

calculate :: Input -> Weight -> Weight
calculate input weight = if result == targ
                             then weight
                             else zipWith (\w i -> w + diff * rate * i) weight (pattern input)
                         where
                             result = neuron input weight
                             targ = (target input)
                             diff = fromIntegral (targ - result)
                             rate = learningRate


neuron :: Input -> Weight -> Int
neuron input weight = if sum (zipWith (*) (pattern input) weight) >= 0
                            then 1
                            else 0

learningRate :: LearningRate
learningRate = 1


