{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception              (try)
import           Control.Monad                  (forM, join)
import           Data.Either                    (either)
import           Data.Text                      (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Data.Set                       (Set)
import qualified Data.Set        as Set
import qualified System.FilePath as Path
import           Text.Regex.TDFA                ((=~))
import           System.IO.Error                (catchIOError)
import           Options.Applicative
                                                (Alternative(some),
                                                 (<**>),
                                                 fullDesc,
                                                 info,
                                                 long,
                                                 metavar,
                                                 progDesc,
                                                 short,
                                                 strArgument,
                                                 switch,
                                                 execParser,
                                                 helper,
                                                 Parser)

data Options = Options {
    paths :: [FilePath]
} deriving (Show, Read, Eq, Ord)

parser :: Parser Options
parser = Options <$>
    some (strArgument $ metavar "[files]")

referenceRegex :: Text
referenceRegex = "\\[\\[(.*)\\]\\]"

splitAtChar :: Char -> String -> [String]
splitAtChar c s =  case dropWhile (==c) s of
    "" -> []
    s' -> w : splitAtChar c s''
        where (w, s'') = break (==c) s'

matchToFilePath :: FilePath -> Text -> FilePath
matchToFilePath dir = (dir ++) . (++ ".md") . head . splitAtChar '#' . head . splitAtChar '|' . T.unpack

getReferences :: FilePath -> IO [FilePath]
getReferences file = do
    let dir = Path.joinPath . init . Path.splitPath $ file
    contents <-   catchIOError (TIO.readFile file) (const . return $ "")
    let matches = contents =~ referenceRegex :: [[Text]]
    return $ map (matchToFilePath dir . (!! 1)) matches

loop :: Set FilePath -> [FilePath] -> IO (Set FilePath)
loop visited []     =  return visited
loop visited (f:fs) = do
    refs <- getReferences f
    let unvisited = filter (`Set.notMember` visited) refs
    let visited' = foldl (flip Set.insert) visited unvisited
    loop visited' $ unvisited ++ fs

main :: IO ()
main = do
    opts <- execParser (info (parser <**> helper) $ fullDesc <>
                                      progDesc "Analyzes reachability of Markdown files")
    references <- concat <$> forM (paths opts) getReferences
    visited <- loop Set.empty references
    mapM_ putStrLn visited