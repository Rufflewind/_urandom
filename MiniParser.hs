{-|

Module      : MiniParser
Description : A simple monadic parser.
Copyright   : (c) Phil Ruffwind, 2014
License     : MIT
Maintainer  : Phil Ruffwind
Stability   : experimental
Portability : portable

The module provides a monadic, backtracking parser with an emphasis on
/simplicity/: the aim is to provide a flexible, barebone parser that has no
external dependencies.

Only the most primitive parsers and combinators are provided.  There is no
optimization at all.

The interface is intentionally minimalistic: most of the features are meant to
be accessed via the type classes.

-}
module MiniParser
  ( Parser(Parser, runParser)
  , get
  , look ) where
import Control.Applicative
  ( Applicative((<*>), pure)
  , Alternative((<|>), empty) )
import Control.Monad
  ( ap, MonadPlus(mplus, mzero) )

-- | A monadic parser that accepts tokens of type @t@ and produces a result of
--   type @a@.
newtype Parser t a = Parser {
    runParser :: [t] -> Maybe (a, [t])
    -- ^ Runs the parser on the given input.
    --
    --   * If parsing succeeds, returns @'Just' (result, remainingInput)@.
    --
    --   * On failure, returns 'Nothing'.
  }

instance Functor (Parser t) where
  fmap f p = return f `ap` p

instance Applicative (Parser t) where
  pure  = return
  (<*>) = ap

instance Alternative (Parser t) where
  empty = mzero
  (<|>) = mplus

instance Monad (Parser t) where
  return x       = Parser $ \ s -> return (x, s)
  Parser p >>= f = Parser $ \ s -> do
    (x, s') <- p s
    runParser (f x) s'

instance MonadPlus (Parser t) where
  mzero                     = Parser $ return empty
  Parser p `mplus` Parser q = Parser $ \ s -> p s `mplus` q s

-- | Consumes and returns the next token.  Fails if there is no more input.
get :: Parser t t
get = Parser $ \ s -> case s of
  t : s' -> return (t, s')
  []     -> mzero

-- | Returns the remaining input without consuming any of it.
look :: Parser t [t]
look = Parser $ \ s -> return (s, s)
