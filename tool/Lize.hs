--module Lize where
module Main where

import Control.Monad
import Control.Applicative ((<$>))
import System.Directory
import System.FilePath
import System.IO
import System.Environment (getArgs)
import Data.List
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

stateGPL = ["This program is free software: you can redistribute it and/or modify",
    "it under the terms of the GNU General Public License as published by",
    "the Free Software Foundation, either version 3 of the License, or",
    "(at your option) any later version.",
    "",
    "This program is distributed in the hope that it will be useful,",
    "but WITHOUT ANY WARRANTY; without even the implied warranty of",
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
    "GNU General Public License for more details.",
    "",
    "You should have received a copy of the GNU General Public License",
    "along with this program.  If not, see <http://www.gnu.org/licenses/>."]

magicName = "GNU General Public License"

authorInfo = "Liu Xinyu (liuxinyu95@gmail.com)"

exts = [("C/C++", [".c", ".cpp", ".h", ".hpp", ".cxx", ".hxx", ".cc"]),
        ("Script", [".py", ".pl", ".pm", ".sh"]),
        ("Haskell", [".hs"]),
        ("Lisp", [".scm"])]

comments = [("C/C++", ["/*", " * ", " */"]), 
            ("Script", ["", "# ", ""]),
            ("Haskell", ["", "-- ", ""]),
            ("Lisp", ["", ";; ", ""])]

toLan :: FilePath -> Maybe String
toLan ext = fst <$> find (\p->ext `elem` (snd p)) exts 

createComments :: FilePath -> String -> String -> String -> [String]
createComments filename lan author y = 
    [head] ++ (map (\x -> lead++x) lns) ++ [tail] where
        lns = [filename, "Copyright (C) "++y++" "++author, ""]++stateGPL
        [head, lead, tail] = maybe ["", "", ""] id (lookup lan comments)
 
year:: IO String
year = do 
  (y, _, _) <- date 
  return (show y) where
       date :: IO (Integer,Int,Int) -- :: (year,month,day)
       date = getCurrentTime >>= return . toGregorian . utctDay

insertComments :: FilePath -> String -> String -> [String] -> String
insertComments filename lan y lns = dropWhile ('\n'==) $ unlines $ 
    xs ++ 
    (createComments (takeFileName filename) lan authorInfo y) ++ 
    ys where
        (xs, ys) = splitAt pos lns
        pos = if lan == "Script" 
                  then maybe 0 (+1) (findIndex ("#!" `isPrefixOf`) lns)
                  else 0

gplize:: FilePath -> IO ()
gplize filename = 
    case toLan $ takeExtension filename of
      Nothing -> return ()
      Just lan -> do
        s <- readFile filename
        y <- year
        if magicName `isInfixOf` s 
            then return ()
            else do 
              putStrLn filename
              renameFile filename (filename++"~")
              writeFile filename $ insertComments filename lan y (lines s)

walk :: (FilePath->IO ()) -> FilePath ->IO ()
walk f dir = do
  isDir <- doesDirectoryExist dir
  if isDir 
      then getDirectoryContents dir >>= 
           mapM_ (\x -> walk f $ dir </> x) . (filter (`notElem` [".", ".."]))
      else f dir

main = do
  getArgs >>= mapM_ (walk gplize)
  