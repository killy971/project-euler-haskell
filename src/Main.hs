module Main where

import qualified Data.Map as M
import ProjectEuler.Problem001
import ProjectEuler.Problem002
import ProjectEuler.Problem004
import ProjectEuler.Problem008
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
import ProjectEuler.Problem064
import ProjectEuler.Problem065
import ProjectEuler.Problem070
import ProjectEuler.Problem071
import ProjectEuler.Problem072
import ProjectEuler.Problem073
import ProjectEuler.Problem074
import ProjectEuler.Problem076
import ProjectEuler.Problem077
import ProjectEuler.Problem085
import ProjectEuler.Problem092
import ProjectEuler.Problem093
import ProjectEuler.Problem122
import ProjectEuler.Problem203
import ProjectEuler.Problem204
import ProjectEuler.Problem214
import ProjectEuler.Problem225
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Util

solutions :: M.Map Integer Integer
solutions = M.fromList [
    (1, solution001),
    (2, solution002),
    (4, solution004),
    (8, solution008),
    (9, solution009),
    (10, solution010),
    (12, solution012),
    (14, solution014),
    (21, solution021),
    (24, solution024),
    (25, solution025),
    (31, solution031),
    (33, solution033),
    (34, solution034),
    (36, solution036),
    (38, solution038),
    (41, solution041),
    (43, solution043),
    (52, solution052),
    (55, solution055),
    (57, solution057),
    (64, solution064),
    (65, solution065),
    (70, solution070),
    (71, solution071),
    (72, solution072),
    (73, solution073),
    (74, solution074),
    (76, solution076),
    (77, solution077),
    (85, solution085),
    (92, solution092),
    (93, solution093),
	(122, solution122),
    (203, solution203),
    (204, solution204),
    (214, solution214),
    (225, solution225)]

solution :: Integer -> Maybe Integer
solution number = M.lookup number solutions

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage >> exitSuccess
        ["-h"] -> usage >> exitSuccess
        [number] -> do
            case solution (read number :: Integer) of
                Just result -> time result >>= print
                Nothing -> putStrLn "There is no solution yet for this problem"
        _ -> usage >> exitSuccess
    where
        usage = putStrLn "Usage: cabal run problem [number]"
