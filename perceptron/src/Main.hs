module Main where

import System.Environment
import Text.ARFF
import Data.Maybe
import Data.List
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS

import Options

type Weight = [Double]
type LearningRate = Double
type Threshold = Double

data Input = Input {
    pattern :: [Double],
    target  :: Int
} deriving (Show, Eq)

main :: IO()
main = do
   args <- getArgs
   let maybeOptions = getOptions args
       options = fromMaybe (error $ optionsError "Missing parameters.") maybeOptions
   fileBody <- BS.readFile (file options)
   let arffObj = parseArff fileBody
   print arffObj
   print $ perceptron [
      Input [-0.4, 0.3] 1,
      Input [-0.3, -0.1] 1,
      Input [0.2, 0.4] 1,
      Input [-0.1, 0.1] 1,
      Input [0.1, -0.5] 0,
      Input [0.2, -0.9] 0,
      Input [0.3, 0.2] 0,
      Input [0.4, -0.6] 0
    ] [0, 0]
   print $ perceptron [
      Input [0, 0, 1, 1] 0,
      Input [1, 1, 1, 1] 1,
      Input [1, 0, 1, 1] 1,
      Input [0, 1, 1, 1] 0
    ] [0, 0, 0, 0]

parseArff :: BS.ByteString -> (Header, [[Maybe AttributeValue]])
parseArff fileBody = case parseOnly arff fileBody of
                      Right obj -> obj
                      Left  err -> error err

perceptron :: [Input] -> Weight -> Weight
perceptron input weight = if weight == newWeight
                              then
                                  newWeight
                              else
                                  perceptron input newWeight
                          where
                              newWeight = epoch input weight


epoch :: [Input] -> Weight -> Weight
epoch []    weight = weight
epoch input weight = epoch (tail input) (calculate (head input) weight)

calculate :: Input -> Weight -> Weight
calculate input weight = if result == targ
                             then weight
                             else zipWith (\w i -> w + diff * rate * i) weight (pattern input)
                         where
                             result = neuron input weight
                             targ = target input
                             diff = fromIntegral (targ - result)
                             rate = learningRate

neuron :: Input -> Weight -> Int
neuron input weight = if sum (zipWith (*) (pattern input) weight) > threshold
                            then 1
                            else 0

learningRate :: LearningRate
learningRate = 1

threshold :: Threshold
threshold = 0
