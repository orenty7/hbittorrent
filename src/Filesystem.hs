module Filesystem where



import Torrent
import System.IO 
import System.Directory
import System.FilePath
import qualified Data.Text.IO as TIO
import qualified Data.Text as T


create :: Maybe T.Text -> File -> IO ()
create parentDir file = let
  createDirWithParents = createDirectoryIfMissing True

  createPath path =
    case parentDir of
      Just name -> joinPath $ map T.unpack $ name: path
      Nothing -> joinPath $ map T.unpack path    
  in
    do
      createDirWithParents $ createPath $ init $ path file 
      handle <- openFile (createPath $ path file) WriteMode
      hSetFileSize handle (Torrent.length file) 

  

markup :: Torrent -> IO ()
markup torrent = do
  case fs torrent of
    SingleFile file -> create Nothing file
    Files files dirname -> mapM_ (create $ Just dirname) files
                           
  putStrLn "Done"


