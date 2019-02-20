--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 7: Applicative functors                                                --
--------------------------------------------------------------------------------

module Lab7 where

import Data.Char
import Data.Maybe

--------------------------------------------------------------------------------
-- Parsers

data Parser a = MkParser (String -> Maybe (a, String))

-- | Runs a parser on some input. If successful, the result of the parser
-- is returned along with any remaining input.
parse :: Parser a -> String -> Maybe (a, String)
parse (MkParser f) xs = f xs

-- | A function which, given a predicate, constructs a parser that succeeds
-- if the first character in the input satisfies the predicate.
ch :: (Char -> Bool) -> Parser Char
ch p = MkParser $ \xs -> case xs of
    (y:ys) | p y -> Just (y,ys)
    _            -> Nothing

--------------------------------------------------------------------------------
-- Parsers are functors

instance Functor Parser where
    fmap f (MkParser g) =
        MkParser $ \xs -> g xs >>= \(x,y) -> Just (f x, y)


--------------------------------------------------------------------------------
-- Parsers are applicative functors

instance Applicative Parser where
    pure x = MkParser $ \y -> Just (x, y)

    (MkParser a) <*> p = MkParser (\xs -> a xs >>=
        \(f, ys) -> let (MkParser b) = p in
          b ys >>= \(x, zs) -> Just (f x, zs))

--------------------------------------------------------------------------------
-- Alternative

infixl 3 <|>
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

    some  :: f a -> f [a]
    some p = (:) <$> p <*> many p

    many  :: f a -> f [a]
    many p = some p <|> pure []

instance Alternative Parser where
    empty = MkParser $ const Nothing

    (MkParser a) <|> (MkParser b) =
        MkParser (\xs -> case a xs of
          Nothing -> b xs
          r       -> r)

--------------------------------------------------------------------------------

nat :: Parser Integer
nat = read <$> some (ch isDigit)

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

oneOf :: [Char] -> Parser Char
oneOf = choice . map (ch . (==))

whitespace :: Parser String
whitespace = many (oneOf [' ', '\t', '\n', '\r'])

token :: Parser a -> Parser a
token p = whitespace *> p


between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

--------------------------------------------------------------------------------

data Expr = Val Integer | Add Expr Expr
    deriving Eq

instance Show Expr where
    show (Val n)   = show n
    show (Add l r) = concat ["( ", show l, " + ", show r, " )"]

eval :: Expr -> Integer
eval (Val n)   = n
eval (Add l r) = eval l + eval r

--------------------------------------------------------------------------------

lparen :: Parser Char
lparen = token (ch (=='('))

rparen :: Parser Char
rparen = token (ch (==')'))

plus :: Parser Char
plus = token (ch (=='+'))

val :: Parser Expr
val = Val <$> token nat

add :: Parser Expr
add = between lparen rparen $
        Add <$> expr
            <*> (plus *> token expr)

expr :: Parser Expr
expr = undefined

parseAndEval :: String -> Maybe Integer
parseAndEval xs = eval . fst <$> parse expr xs

--------------------------------------------------------------------------------
