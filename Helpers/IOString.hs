-- MittraSW version 0.1, Copyright (C) 30/May/2016
-- File creator: rhvall
-- Idea creator: AndrewC
-- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
-- This is free software under GNU General Public License as published by
-- the Free Software Foundation; either version 3 of the License, or
-- (at your option) any later version.
-- This code was found as a response to a Stack Overflow question
-- http://stackoverflow.com/questions/27925558/reading-lines-in-haskell-until-non-empty-string
--   _________
--  /_  ___   \
-- /@ \/@  \   \
-- \__/\___/   /
--  \_\/______/
--  /     /\\\\\
-- |     |\\\\\\
--  \      \\\\\\\
--    \______/\\\\\
--     _||_||_

module Helpers.IOStrings
where

-- Declare the infix precedence for the operator
infixr 6 <||>

-- Making a Monoid of a Monad of Eq Monoids
-- We can make a monoid out of IO String because we can check the value returned
-- to see if it's "" and then do the next action if not. That's equivalent to
-- using == to check whether we have mempty, so we can generalise to IO s as
-- long as s is a Monoid with an Eq instance. Secondly, we don't need it to
-- be IO, we could use any Monad
(<||>) :: (Monoid s, Eq s, Monad m) => m s -> m s -> m s
m <||> n = do
    x <- m
    if x == mempty then n else return x

-- Identity
-- Mathematically that's a monoid with identity
msempty :: (Monoid s, Monad m) => m s
msempty = return mempty

-- List concatenation
-- Let's also define the equivalent of mconcat :: Monoid s => [s] -> s:
msconcat :: (Monoid s, Eq s, Monad m) => [m s] -> m s
msconcat = foldr (<||>) (return mempty)

-- Lazily combining infinitely many monadic monoids
doWhileNotEmpty :: IO ()
doWhileNotEmpty = msconcat (repeat getLine) >>= print
