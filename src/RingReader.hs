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

mvWAddStr2 :: HSCurses.Window -> Int -> Int -> String -> IO ()
mvWAddStr2 w y x s = do
  (rows, cols) <- HSCurses.scrSize
  when ((y >= 0) && (x >= 0) && (x < cols) && (y < rows)) $
      let space = cols - x
          s2 = take space s
       in
       HSCurses.mvWAddStr w y x s2

dispatch :: HSCurses.Key -> IO ()
dispatch _ = return ()

(--->) :: Monad m => (a -> m b) -> (a -> m b) -> a -> m b
(--->) = liftM2 (>>)

passTo :: a -> (a -> b) -> b
passTo = flip ($)


mainLoop :: Int -> Handle -> Int -> Bool -> IO ()
mainLoop lim filein cols infinitep = stToIO (newSTRef 1) >>= loop
  where
    loop = discardResult . forever . wrapCode step HSCurses.refresh . stToIO . initialize

    initialize n = readSTRef n >>= incSTRef n `keepOldBinding` addSTRefToTuple n

    step (x,nextX) = discardResult $ do
      let handleInputp = not infinitep && ( nextX `mod` lim == 0)
      eofp <- atEnd
      if eofp then threadDelay 100
              else do
                line <- hGetLine filein
                x `passTo` (clearLine ---> showLine line ---> showDivider)
                handleInput handleInputp

    clearLine x = moveAndAddString (getLinePos x) 0 $ replicate (cols+1) ' ' -- overwrite line
    showLine  line x = moveAndAddString (getLinePos x) 0 $ show x ++ (' ':line)   -- show new line
    showDivider x = moveAndAddString ( getLinePos (x+1) ) 0 div

    addSTRefToTuple n = flippedLiftM (readSTRef n) . (,)
    atEnd = hIsEOF filein
    discardResult = when True
    div = marker ++ replicate (cols - length marker + 1) '-'
    flippedLiftM = flip liftM
    getLinePos = subtract 1 . (`mod` lim)
    handleInput handleInputp = when handleInputp $ HSCurses.getCh >>= dispatch
    incSTRef n = writeSTRef n . (+ 1)
    keepOldBinding = liftM2 (>>)
    marker = "--- break ---"
    moveAndAddString = mvWAddStr2 HSCurses.stdScr
    wrapCode run finalize init = (init >>= run) >> finalize

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
