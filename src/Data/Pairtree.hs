{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Data.Pairtree
  ( pairtreeObjects
  ) where
import Control.Exception (Exception, throw)
import Data.List (partition)
import qualified Data.Map as M
import Data.Monoid (mappend)
import Data.Pairtree.Mapping (unclean)
import Data.Text.Lazy (Text, pack)
import Data.Typeable (Typeable)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

data PairtreeException
  = EncapsulationException FilePath
  deriving (Show, Typeable)

instance Exception PairtreeException

getRealContents :: FilePath -> IO [FilePath]
getRealContents = fmap (filter (`notElem` [".", ".."])) . getDirectoryContents

pairtreeObjects :: FilePath -> IO (M.Map Text FilePath)
pairtreeObjects = children ""
  where
    mortyMap ident path = do
      contents <- getRealContents path
      if length contents /= 1
        then throw $ EncapsulationException path
        else return . M.singleton (unclean ident) $ path </> head contents
    children :: Text -> FilePath -> IO (M.Map Text FilePath)
    children ident path = do
      contents <- getRealContents path
      let (morties, more) = partition ((1 ==) . length) contents
      let (shorties, rest) = partition ((2 ==) . length) more
      mortyObjects <- mapM (uncurry mortyMap . childIdentPath) morties 
      shortyObjects <- mapM (uncurry children . childIdentPath) shorties
      let restObjects = case rest of
            []         -> []
            child : [] -> [M.singleton (unclean ident) $ path </> child]
            _          -> throw $ EncapsulationException path
      return . M.unions $ mortyObjects ++ shortyObjects ++ restObjects
      where
        childIdentPath child = (ident `mappend` pack child, path </> child)

