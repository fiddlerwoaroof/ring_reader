{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import System.IO
import System.Process
{-import System.Environment-}
import System.Console.CmdArgs
import System.Exit

import Control.Concurrent

import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.ST
import Data.STRef

import qualified UI.HSCurses.Curses as HSCurses
import qualified UI.HSCurses.CursesHelper as HSCursesHelper

a :: Integer
a = runST $ do
  n <- newSTRef 0
  readSTRef n

mvWAddStr2 :: HSCurses.Window -> Int -> Int -> String -> IO ()
mvWAddStr2 w y x s = do
    (rows, cols) <- HSCurses.scrSize
    when ((y >= 0) && (x >=0) && (x < cols) && (y < rows)) $ do
      let space = cols - x
      let s2 = take space s
      HSCurses.mvWAddStr w y x s2

dispatch :: HSCurses.Key -> IO ()
dispatch _ = return ()

mainLoop :: Int -> Handle -> Int -> Bool -> IO ()
mainLoop lim filein cols infinitep = do
  n <- stToIO $ newSTRef 1
  when True $ forever $ do
    let getX = stToIO $ readSTRef n
    x <- getX
    stToIO $ writeSTRef n (x+1)
    nextX <- getX
    let handleInputp = not infinitep && ( nextX `mod` lim == 0)

    eofp <- atEnd
    if eofp then threadDelay 100
      else do
        displayNextLine x
        handleInput handleInputp

    HSCurses.refresh
  where
    atEnd = hIsEOF filein
    getLineFromFile = hGetLine filein
    showDivider x lim =
      moveAndAddString ( ((x+1)`mod`lim) - 1) 0
    handleInput handleInputp = when handleInputp $ HSCurses.getCh >>= dispatch
    moveAndAddString = mvWAddStr2 HSCurses.stdScr

    displayNextLine x = do
      let xModLim = x `mod` lim - 1
      let marker = "--- break ---"
      let divider = marker ++ replicate (cols - length marker + 1) '-' :: String
      let clearLine x cols line = moveAndAddString xModLim 0 $ replicate (cols+1) ' ' -- overwrite line
      let showLine  x cols line = moveAndAddString xModLim 0 $ show x ++ line   -- show new line

      line <- getLineFromFile
      let line = ' ':line

      clearLine x cols line
      showLine x cols line
      showDivider x lim divider


allocate :: IO ()
allocate = do
   HSCurses.initCurses
   hasColors <- HSCurses.hasColors
   when hasColors $ do
     HSCurses.startColor
     HSCurses.initPair
        (HSCurses.Pair 1)
        HSCursesHelper.black
        HSCursesHelper.white


deallocate :: IO ()
deallocate = HSCurses.endWin

work :: String -> [FilePath] -> Bool -> String -> IO ()
work enc run infinitep stream = do
   let (runprog:myArgs) = run
   (rows, cols) <- HSCurses.scrSize
   (_, Just hout, Just herr, _) <-
      createProcess (proc runprog myArgs) {
         std_out=CreatePipe,
         std_err=CreatePipe
      }

   -- enc <- mkTextEncoding "ISO-8859-1"
   encoding <- mkTextEncoding enc
   hSetEncoding hout encoding
   hSetEncoding herr encoding

   case stream of
      "stdout" -> mainLoop rows hout cols infinitep
      _        -> mainLoop rows herr cols infinitep
   return ()


data MyOptions = MyOptions {
   infinite :: Bool,
   run :: [String],
   encoding :: String,
   stream :: String
} deriving (Data, Typeable, Show, Eq)

myProgOpts :: MyOptions
myProgOpts = MyOptions {
   infinite = def &= name "i",
   encoding = "UTF-8" &= name "e",
   stream = "stdout" &= name "s",
   run = def &= args
}

getOpts :: IO MyOptions
getOpts = cmdArgs myProgOpts

main :: IO ()
main = do
   opts <- getOpts
   optionHandler opts

optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..} = Exception.bracket_ allocate deallocate (work encoding run infinite stream)

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
