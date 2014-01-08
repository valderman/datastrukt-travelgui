module Lab3Help
  ( BStop(..)
  , BLineTable(..)
  , BLineStop(..)
  , readStops
  , readLines
  ) where

import Control.Applicative
import Control.Monad
import Data.List

-- | Description of a bus stop.

data BStop = BStop
  { name     :: String
                -- ^ The bus stop's name.
  , position :: (Integer, Integer)
                -- ^ Coordinates (x, y).
                --
                -- Invariant: The coordinates range from 0 to 1000.
  } deriving Show

-- | Description of a bus line.

data BLineTable = BLineTable
  { lineNumber :: Integer
                  -- ^ The line number.
  , stops      :: [BLineStop]
                  -- ^ Bus stops (in the order in which they are
                  -- visited).
                  --
                  -- Invariant: The list contains at least one
                  -- element.
  } deriving Show

-- | Description of a bus stop, as part of a bus line.

data BLineStop = BLineStop
  { stopName :: String
                -- ^ The bus stop's name.
  , time     :: Integer
                -- ^ The travel time (in minutes) from the previous
                -- bus stop.
                --
                -- Invariant: A non-negative number, 0 if this is the
                -- first stop.
  } deriving Show

-- | Tries to parse a non-negative integer.

readNat :: String -> Maybe Integer
readNat s = case filter (null . snd) $ reads s of
  [(x, _)] | x >= 0 -> Just x
  _                 -> Nothing

-- | @split n xs@ splits up @xs@ into groups of size @n@ (and possibly
-- a final, shorter group).

split :: Integer -> [a] -> [[a]]
split n [] = []
split n xs = ys : split n zs
  where (ys, zs) = genericSplitAt n xs

-- | Tries to parse a bus stop. If an error is encountered, then an
-- error message is returned (@'Left' msg@).

readStop :: [String] -> Either String BStop
readStop ss = case ss of
  name : coordinates@[_, _] ->
    case mapM readNat coordinates of
      Just cs@[x, y] | all inRange cs ->
        Right (BStop { name = name, position = (x, y) })
      _ -> err
  _ -> err
  where
  inRange c = 0 <= c && c <= 1000
  err       = Left $ "Incorrectly formatted bus stop:\n" ++ unwords ss

-- | Tries to read a list of bus stops from the given file. If an
-- error is encountered, then an error message is returned (@'Left'
-- msg@).

readStops :: FilePath -> IO (Either String [BStop])
readStops f = mapM readStop . split 3 . words <$> readFile f

-- | Tries to parse a \"bus line stop\" (not the first one for a line,
-- i.e., the 'time' must be given). If an error is encountered, then
-- an error message is returned (@'Left' msg@).

readBLineStop :: [String] -> Either String BLineStop
readBLineStop ss = case ss of
  [name, time] -> case readNat time of
    Just time -> Right (BLineStop { stopName = name, time = time })
    _         -> err
  _ -> err
  where
  err = Left $ "Incorrectly formatted bus line stop:\n" ++ unwords ss

-- | Tries to read information about bus lines from the given file. If
-- an error is encountered, then an error message is returned (@'Left'
-- msg@).

readLines :: FilePath -> IO (Either String [BLineTable])
readLines f = parse . words <$> readFile f
  where
  parse []           = return []
  parse (l : n : ss) =
    case mapM readNat [l, n] of
      Just [line, noStops]
        | noStops <= 0 -> Left $ "Empty lines not allowed:\n" ++
                                 unwords [l, n]
        | otherwise    -> do
          let noTokens      = 2 * noStops - 1
              (stops, rest) = genericSplitAt noTokens ss
          when (genericLength stops < noTokens) $ do
            Left $ "Incorrectly formatted line:\n" ++
                   unwords (l : n : stops)
          case stops of
            stop : stops -> do
              stop  <- return (BLineStop { stopName = stop, time = 0 })
              stops <- mapM readBLineStop (split 2 stops)
              rest  <- parse rest
              return (BLineTable { lineNumber = line
                                 , stops      = stop : stops
                                 } : rest)
      _ -> Left $ "Incorrectly formatted line header:\n" ++
                  unwords [l, n]
