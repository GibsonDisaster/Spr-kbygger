module Types where
  import qualified Data.Map as M
  import Control.Monad.Random

  data Rule = End String [(String, Rational)]
            | Composition String [(String, Rational)]
            | Text String
            deriving (Show, Eq, Ord)

  data Grammar = Grammar [Rule] deriving (Show, Eq, Ord)

  -- Grammar monad so you can string addRule together
  -- Need to make Grammar * -> * instead of current *
  -- How to smartly make a Grammar a = Grammar [Rule]
  -- Maybe just do Grammar a = Grammar [(a, Rational)] ?

  testInput :: String
  testInput = "The <N> <V> <AV> and <Repeat>."

  testRule :: Rule
  testRule = End "<N>" [("Cat", 3), ("Dog", 4), ("Person", 5)]

  testGrammar :: Grammar
  testGrammar = Grammar [ Composition "Start" [("The <N> <V> <AV> and <Repeat>.", 1)],
                          End "<N>" [("Cat", 3), ("Dog", 4), ("Person", 5)],
                          End "<V>" [("meows", 5), ("barks", 5), ("talks", 5)],
                          End "<AV>" [("loudly", 4), ("softly", 5), ("weirdly", 1)],
                          Composition "<Repeat>" [("the <N> <V> <AV>", 1)] ]

  getChoice :: MonadRandom m => [(String, Rational)] -> m String
  getChoice = fromList

  parseCompositionString :: String -> [String]
  parseCompositionString r
    | null r = []
    | head r == '<' = let (tag, rest) = span (/= '>') r in [tag ++ [head rest]] ++ parseCompositionString (drop 1 rest)
    | otherwise = let (ws, rest) = span (/= '<') r in [ws] ++ parseCompositionString rest

  getRuleName :: String -> String
  getRuleName r = takeWhile (/= '>') $ drop 1 r

  solved :: String -> Bool
  solved s = '<' `elem` s