module Main where

import qualified Data.Map as M
import ProjectEuler.Problem001
import ProjectEuler.Problem002
import ProjectEuler.Problem009
import ProjectEuler.Problem010
import ProjectEuler.Problem012
import ProjectEuler.Problem014
import ProjectEuler.Problem021
import ProjectEuler.Problem024
import ProjectEuler.Problem025
import ProjectEuler.Problem031
import ProjectEuler.Problem033
import ProjectEuler.Problem034
import ProjectEuler.Problem036
import ProjectEuler.Problem038
import ProjectEuler.Problem041
import ProjectEuler.Problem043
import ProjectEuler.Problem052
import ProjectEuler.Problem055
import ProjectEuler.Problem057
import ProjectEuler.Problem065
import ProjectEuler.Problem070
import ProjectEuler.Problem071
import ProjectEuler.Problem072
import ProjectEuler.Problem073
import ProjectEuler.Problem074
import ProjectEuler.Problem092
-- import ProjectEuler.Problem187
import ProjectEuler.Problem214
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess))

solutions :: M.Map String Integer
solutions = M.fromList [
    ("001", solution001),
    ("002", solution002),
    ("009", solution009),
    ("010", solution010),
    ("012", solution012),
    ("014", solution014),
    ("021", solution021),
    ("024", solution024),
    ("025", solution025),
    ("031", solution031),
    ("033", solution033),
    ("034", solution034),
    ("036", solution036),
    ("038", solution038),
    ("041", solution041),
    ("043", solution043),
    ("052", solution052),
    ("055", solution055),
    ("057", solution057),
    ("065", solution065),
    ("070", solution070),
    ("071", solution071),
    ("072", solution072),
    ("073", solution073),
    ("074", solution074),
    ("092", solution092),
    -- ("187", solution187),
    ("214", solution214)]

solution ::  String -> Maybe Integer
solution number = M.lookup number solutions

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage >> exit
        ["-h"] -> usage >> exit
        -- [number] -> print $ solution number
        [number] -> case solution number of
            Just result -> print result
            Nothing -> usage
        _ -> usage >> exit
    where
        usage = putStrLn "Usage: cabal run problem [number]"
        exit = exitWith ExitSuccess
