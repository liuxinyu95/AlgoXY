--module Lize where
module Main where

import Control.Monad
import Control.Applicative ((<$>))
import System.Directory
import System.FilePath
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

exts = [("C/C++", [".c", ".cpp", ".h", ".hpp", ".cxx", ".hxx"]),
        ("Script", [".py", ".pl", ".pm", ".sh"]),
        ("Haskell", [".hs"]),
        ("Lisp", [".scm"])]

comments = [("C/C++", ["/*", " *", " */"]), 
            ("Script", ["", "# ", ""]),
            ("Haskell", ["", "-- ", ""]),
            ("Lisp", ["", ";; ", ""])]

toLan :: FilePath -> Maybe String
toLan ext = fst <$> find (\p->ext `elem` (snd p)) exts 

createComments :: FilePath -> String -> String -> String -> String
createComments filename lan author year = 
    join "\n" ([head] ++ (map (\x -> lead++x) lines) ++ [tail]) where
        join s = concat . map (\x -> x++s)
        lines = [filename, "Copyright (C) "++year++" "++author]++stateGPL
        [head, lead, tail] = maybe ["", "", ""] id (lookup lan comments)
 
date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

walk :: FilePath -> IO [FilePath]
walk dir = do
  names <- getDirectoryContents dir
  paths <- forM (filter (`notElem` [".", ".."]) names) $ \name -> do
             let path = dir </> name
             isDir <- doesDirectoryExist path
             if isDir then walk path else return [path]
  return (concat paths)

main = do
  items <- walk "."
  putStrLn $ show items
  