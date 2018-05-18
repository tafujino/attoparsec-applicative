module Data.Attoparsec.Applicative
where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Control.Monad

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
infixr 3 <&&>

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
infixr 2 <||>

asciiRange :: Char -> Char -> Char -> Bool
asciiRange x y = (x <=) <&&> (<= y)

(<->) :: Char -> Char -> Char -> Bool
(<->) = asciiRange
infix 4 <->

-- | possibly faster implementation of inClass (range notation is unsupported)
-- use Template Haskell for much faster implementation?
inClass' :: String -> Char -> Bool
inClass' = foldl1 (<||>) . map (==)

-- original isSpace gives True when EOL is input
isSpace' :: Char -> Bool
isSpace' = (== ' ') <||> (== '\t')

-- original skipSpace skips EOLs
skipSpace' :: Parser ()
skipSpace' = void $ takeWhile isSpace'

-- original isEndOfLine has type Word8 -> Bool
isEndOfLine' :: Char -> Bool
isEndOfLine' = (== '\n') <||> (== '\r')

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)
infixr 5 <++>
