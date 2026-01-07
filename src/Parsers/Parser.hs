{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Parsers.Parser (
     Parser(..),
     item,
     char,
     string,
     satisfy,
     elem',
     elems,
     sepBy0,
     sepBy1,
     digit,
     digits0,
     digits1,
     int,
     nat,
     splitOn',
     ws,
     lexeme
  )
where

import           Control.Applicative (Alternative (..))
import qualified Data.Char           as Char
import           Data.Data           (Proxy (Proxy))
import           Data.Foldable       (Foldable (foldl'))
import           Data.Functor        (($>))
import           Data.String         (IsString)

newtype Parser s a = Parser {parse :: s -> Either String (a, s)}

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) =
    Parser $ \input ->
      fmap (\(x, xs) -> (f x, xs)) (p input)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ \input -> Right (x, input)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  pf <*> px =
    Parser $ \input -> do
      (f, rest) <- parse pf input
      (x, rest') <- parse px rest
      pure (f x, rest')

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  p >>= f =
    Parser $ \input -> do
      (x, xs) <- parse p input
      parse (f x) xs

instance MonadFail (Parser s) where
  fail :: String -> Parser s a
  fail msg = Parser $ \_ -> Left msg

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ \_ -> Left ""

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  px <|> py =
    Parser $ \input ->
      case parse px input of
        Right v  -> Right v
        Left err -> case parse py input of
                      Right v   -> Right v
                      Left err' -> Left $
                        "All given alternatives failed, with errors: " ++ "\n" ++ err ++ "\n" ++ err'

class Parsable s where
  type Elem s
  uncons :: s -> Maybe (Elem s, s)

instance Parsable [a] where
  type Elem [a] = a

  uncons :: [a] -> Maybe (a, [a])
  uncons []     = Nothing
  uncons (x:xs) = Just (x, xs)

class IsDigit a where
  isDigit :: a -> Bool

instance IsDigit Char where
  isDigit :: Char -> Bool
  isDigit = Char.isDigit

class DigitToInt a where
  digitToInt :: a -> Int

instance DigitToInt Char where
  digitToInt :: Char -> Int
  digitToInt = Char.digitToInt

class (Parsable s) => MinusSign s where
  minus :: Proxy s -> Elem s

instance MinusSign String where
  minus :: Proxy String -> Char
  minus _ = '-'

item :: (Parsable s) => Parser s (Elem s)
item =
  Parser $ \input ->
    case uncons input of
      Nothing      -> Left "Unexpected end of input"
      Just (x, xs) -> Right (x, xs)

char :: (Parsable s, IsString s, Eq (Elem s), Show (Elem s)) => Elem s -> Parser s (Elem s)
char c = satisfy (==c)

string :: (Parsable s, IsString s, Eq (Elem s), Show (Elem s)) => [Elem s] -> Parser s [Elem s]
string = mapM char

ws :: Parser String ()
ws = many (satisfy (`elem` " \n\t")) $> ()

lexeme :: Parser String a -> Parser String a
lexeme p = ws *> p <* ws

satisfy :: (Parsable s, Show (Elem s)) => (Elem s -> Bool) -> Parser s (Elem s)
satisfy predicate = do
  x <- item
  if predicate x
     then return x
     else fail $ "Item: " ++ show x ++ " does not satisfy given predicate"

elem' :: (Parsable s, Show (Elem s), Eq (Elem s)) => Elem s -> Parser s (Elem s)
elem' t = do
  x <- item
  if t == x
    then return x
    else fail $ "Expected: " ++ show t ++ " given: " ++ show x

elems :: (Parsable s, Show (Elem s), Eq (Elem s)) => [Elem s] -> Parser s [Elem s]
elems = mapM elem'

sepBy0 :: Parser s sep -> Parser s a -> Parser s [a]
sepBy0 psep p = sepBy1 psep p <|> pure []

sepBy1 :: Parser s sep -> Parser s a -> Parser s [a]
sepBy1 psep p = (:) <$> p <*> many (psep *> p)

digit :: (Parsable s, Show (Elem s), IsDigit (Elem s)) => Parser s (Elem s)
digit = satisfy isDigit

digits0 :: (Parsable s, Show (Elem s), IsDigit (Elem s)) => Parser s [Elem s]
digits0 = many digit

digits1 :: (Parsable s, Show (Elem s), IsDigit (Elem s)) => Parser s [Elem s]
digits1 = some digit

nat :: (Parsable s, Show (Elem s), IsDigit (Elem s), DigitToInt (Elem s)) => Parser s Int
nat = foldl' step 0 <$> digits1
  where
    step acc d = acc * 10 + digitToInt d

int ::
    forall s.
    (MinusSign s, Parsable s, Show (Elem s), IsDigit (Elem s), DigitToInt (Elem s), Eq (Elem s)) =>
    Parser s Int
int = do
    f <- option id (negate <$ elem' (minus (Proxy :: Proxy s)))
    f <$> nat
  where
    option :: (Alternative f) => a -> f a -> f a
    option def p = p <|> pure def

splitOn' :: (Parsable s, Show (Elem s), Eq (Elem s)) => [Elem s] -> Parser s a -> Parser s b -> Parser s (a, b)
splitOn' s px py = do
  x <- px
  _ <- elems s
  y <- py
  return (x, y)
