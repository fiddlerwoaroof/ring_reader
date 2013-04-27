{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import System.IO
import System.Process
import System.Environment
import System.Console.CmdArgs
import System.Exit


import Control.Concurrent

import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.STRef

import qualified UI.HSCurses.Curses as HSCurses
import qualified UI.HSCurses.CursesHelper as HSCursesHelper

a = runST $ do
   n <- newSTRef 0
   x <- readSTRef n
   return x

mvWAddStr2 w y x s = do
   (rows, cols) <- HSCurses.scrSize
   when ((y >= 0) && (x >=0) && (x < cols) && (y < rows)) $ do
      let space = (cols - x)
      let s2 = take space s
      HSCurses.mvWAddStr w y x s2

dispatch :: HSCurses.Key -> IO ()
dispatch kc = undefined

b lim filein cols infinitep = do
   n <- stToIO $ newSTRef 1
   e <- forever $ do
      x <- stToIO $ readSTRef n

      eofp <- hIsEOF filein
      if eofp
         then do
            if infinitep
               then do threadDelay 100
               else do threadDelay 100
         else do
            line <- hGetLine filein
            
            mvWAddStr2 HSCurses.stdScr ( (x`mod`lim) - 1) 0 $ replicate (cols+1) ' '
            mvWAddStr2 HSCurses.stdScr ( (x`mod`lim) - 1) 0 $ (show x) ++ (' ':line)

            let marker = "--- break ---"
                div = marker ++ (replicate (cols - (length marker)+1) '-')
               in mvWAddStr2 HSCurses.stdScr ( ((x+1)`mod`lim) - 1) 0 $ div

            if infinitep && ( (x+1) `mod` lim == 0)
               then do
                  kc <- HSCurses.getCh
                  -- dispatch kc
                  return ()
               else
                  return ()

            stToIO $ writeSTRef n (x+1)
            HSCurses.refresh
   return ()

allocate = do
   HSCurses.initCurses
   hasColors <- HSCurses.hasColors
   if hasColors
      then do
         HSCurses.startColor
         HSCurses.initPair
            (HSCurses.Pair 1)
            (HSCursesHelper.black)
            (HSCursesHelper.white)
         return ()
   else
      return ()


deallocate = do
   HSCurses.endWin

work enc run infinitep = do
   let (runprog:args) = run
   (rows, cols) <- HSCurses.scrSize
   (_, Just hout, _, _) <-
      createProcess (proc runprog args) {
         std_out=CreatePipe
      }

   -- enc <- mkTextEncoding "ISO-8859-1"
   enc <- mkTextEncoding enc
   hSetEncoding hout enc

   b rows hout cols infinitep
   return ()


data MyOptions = MyOptions {
   infinite :: Bool,
   run :: [String],
   encoding :: String
} deriving (Data, Typeable, Show, Eq)

myProgOpts :: MyOptions
myProgOpts = MyOptions {
   infinite = def &= name "i",
   encoding = "UTF-8" &= name "e",
   run = def &= args
}

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts

main = do
   args <- getArgs
   opts <- getOpts
   optionHandler opts

optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..} = do
   when (null run) $ putStrLn "No Command to Run!" >> exitWith (ExitFailure 1)
   Exception.bracket_ allocate deallocate (work encoding run infinite)

-- mainloop = runST $ do
--       n <- newSTRef 0
--       a <- getLine
--       x <- readSTRef n
-- 
--       putStrLn (show x) ++ a

--      forever $ numberLines n
--    where
--       numberLines n = do
--          line <- liftIO getLine
--          n' <- readSTRef n
--          putStrLn (show n') ++ ('\t':line)
--          modifySTRef n (+1)
