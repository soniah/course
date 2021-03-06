{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()
  sequence :: List (f a) -> f (List a)

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

   (<$>) :: (a -> b) -> f a -> f b   "fmap"
   (<*>) :: f (a -> b) -> f a -> f b "apply"
   (>>=) :: f a -> (a -> f b) -> f b "bind flipped"
   (=<<) :: (a -> f b) -> f a -> f b "bind" (flatmap?)
   pure  :: a -> fa
   (>>)  :: f a -> f b -> f b

   f >> g
   do f
      g

   f >>= (\x -> g x)
   do x <- f
      g

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

main :: IO ()
main = do
  lc <- getArgs
  let (hd :. _) = lc
  run hd

type FilePath = Chars

run :: Chars -> IO ()
run mainFilePath = do
  (_, mainFileLines) <- getFile mainFilePath
  let files = lines mainFileLines
  contents <- getFiles files
  printFiles contents

getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles paths = sequence $ map getFile paths

getFile :: FilePath -> IO (FilePath, Chars)
getFile filePath = do
  chars <- readFile filePath
  return (filePath, chars)

printFiles :: List (FilePath, Chars) -> IO ()
printFiles x = void $ sequence $ map (\(fp,cs) -> printFile fp cs) x

printFile :: FilePath -> Chars -> IO ()
printFile filePath chars = do
  putStrLn $ (replicate 12 '=') ++ (' ' :. filePath)
  putStrLn chars
