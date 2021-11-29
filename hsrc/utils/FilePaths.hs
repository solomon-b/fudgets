module FilePaths(AFilePath,rootPath,aFilePath,filePath,
                 compactPath,isAbsolute,joinPaths,pathRelativeTo,
		 extendPath,pathTail,pathHead,pathLength) where
import Data.List(intersperse)
--import IO(openDirectory, statFile)
--import ListUtil(chopList,breakAt)
import Utils(segments)

newtype AFilePath = P [String] deriving (Eq,Ord)
-- data AFilePath = Root | Cwd | AFilePath :/ String

aFilePath :: FilePath -> AFilePath
aFilePath = P . splitpath

rootPath = P [""]

filePath :: AFilePath -> FilePath
filePath (P path) = joinpath path

compactPath (P path) = P (compactpath path)

extendPath (P path) node = P (node:path)

pathTail :: AFilePath -> String
pathTail (P []) = "." -- ??
pathTail (P [""]) = "/" -- ??
pathTail (P (t:_)) =  t

pathHead (P []) = (P []) -- ??
pathHead (P (t:h)) = P h

pathLength (P path) = length path

isAbsolute (P ns) = isabsolute ns

joinPaths (P parent) (P child) =
 if isabsolute child
 then P child
 else P (child++parent) -- compactpath?

P file `pathRelativeTo` P dir =
    if take dirlen rfile == rdir
    then P (reverse (drop dirlen rfile))
    else P file
  where
    rdir = reverse dir
    rfile = reverse file
    dirlen = length rdir

isabsolute [] = False
isabsolute ns = null (last ns)

splitpath = reverse . segments (/='/')

joinpath [] = "."
joinpath [""] = "/"
joinpath ns = concat (intersperse "/" (reverse ns))

compactpath [] = []
compactpath (".." : xs) =
  case compactpath xs of
    [""] -> [""] -- parent of root directory, stay in root directory
    ys@("..":_) -> "..":ys -- relative path to grandparent, keep ".."
    _:ys -> ys -- parent of child, optimize
    ys -> "..":ys -- other, keep ".."
--compactpath ["..",""] = [""] -- parent of root directory
--compactpath (".." : "." : xs) = compactpath ("..":xs)
--compactpath (".." : x : xs) | x /= ".." = compactpath xs
compactpath ("" : xs@(_:_)) = compactpath xs
compactpath ("." : xs) = compactpath xs
compactpath (x : xs) = x : compactpath xs

{-
ls s =
    let paths = map (: s) . sort . filter (/= ".")
    in  case openDirectory (joinpath s) of
          Right files -> paths files
          Left msg -> [msg : s]

isdir s =
    case statFile (joinpath s) of
      Right ns -> let mode = ns !! (3 - 1)
                  in  bitand mode 61440 == 16384
      Left _ -> False

-}
