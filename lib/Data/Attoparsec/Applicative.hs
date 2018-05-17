module Data.Attoparsec.Applicative
where

import Control.Applicative

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

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr 5 <:>

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)
infixr 5 <++>
