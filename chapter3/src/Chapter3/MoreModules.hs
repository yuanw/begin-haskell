module Chapter3.MoreModules

import qualified Data.List (filter, permutations)

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = Data.List.filter (\ l -> head l == letter) .
                                  Data.List.permutations


