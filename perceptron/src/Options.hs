module Options (Options, algorithm, file, evaluationMethod, evaluationParams, normalize, seed, getOptions, optionsError) where

import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

data Options = Options {
  algorithm :: String,
  file :: String,
  evaluationMethod :: String,
  evaluationParams :: [String],
  normalize :: Bool,
  seed :: Maybe Int
} deriving (Show)

getOptions :: [String] -> Maybe Options
getOptions [] = Nothing
getOptions arguments = Just (Options alg fle evalMethod evalParams norm maybeSeed)
                       where
                         args = getArguments arguments ["L", "A", "E", "N", "R"]
                         getArg = getOrThrow args
                         alg = head $ getArg "L"
                         fle =  head $ getArg "A"
                         eval = getArg "E"
                         evalMethod =  head eval
                         evalParams = tail eval
                         norm = isJust (Map.lookup "N" args)
                         maybeSeed = mapIfJust (\s -> read (head s) :: Int) (Map.lookup "R" args)

getArguments :: [String] -> [String] -> Map.Map String [String]
getArguments arguments keys = Map.fromList $ mapMaybe (getArgument arguments) keys

getArgument :: [String] -> String -> Maybe (String, [String])
getArgument arguments key = if isNothing params
                              then
                                Nothing
                              else
                                Just (key, fromJust params)
                             where
                              params = parameters arguments ('-':key)

parameters :: [String] -> String -> Maybe [String]
parameters args arg = case elemIndex arg args of
                    Just val -> Just $ parameters' (snd (splitAt (val + 1) args)) []
                    Nothing  -> Nothing

parameters' :: [String] -> [String] -> [String]
parameters' []             params = params
parameters' (('-':_):args) params = params
parameters' (param:args)   params = parameters' args (params ++ [param])

optionsError :: String -> String
optionsError err = err ++ "\nUsage: "++
                            "-L [learningAlgorithm] -A [ARFF_File] -E [EvaluationMethod] {[ExtraParamters]} [-N] [-R seed]"

mapIfJust :: (a -> b) -> Maybe a -> Maybe b
mapIfJust func optA = if isJust optA then Just (func (fromJust optA)) else Nothing

getOrThrow :: Map.Map String b -> String -> b
getOrThrow dataMap key = fromMaybe (error $ optionsError ("Missing parameter : " ++ key)) value
                         where
                          value = Map.lookup key dataMap
