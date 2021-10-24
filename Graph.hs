module Graph where
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.Sequence (Seq, (|>), (><), (<|))
import qualified Data.Sequence as Sq
import qualified Data.Foldable as F



g ? x = M.findWithDefault [] x g
--need to add initialization of empty to it
createAdjList :: (Ord k) => [(k, a)] -> M.Map k [a]
createAdjList = M.fromListWith (++) . fixing
    where
        fixing = map (\(x,y) -> (x, [y]))


createUniDirectional :: (Ord k) => [(k, k)] -> M.Map k [k]
createUniDirectional = createAdjList . fixing 
    where
        fixing x = x >>= (\(x,y) -> [(x,y), (y,x)])


add g (a,b) = M.insertWith (++) (a,[b])

degree g x = length $ g ? x

dfs g x = dfs' g x S.empty

dfs' g x s = dfs'' [x] s Sq.empty
     where
         dfs'' [] s acc = (s, acc)
         dfs'' (x:xs) s acc
             | x `S.member` s = dfs'' xs s acc
             | otherwise = 
                 let (outSet, outList) = dfs'' (g ? x) (S.insert x s) acc
                 in dfs'' xs outSet (x <| outList)

getNodes = map fst . M.toList

topoSort :: (Ord k) => M.Map k [k] -> Seq k
topoSort g = fst . foldr doStuff (Sq.empty,S.empty) . getNodes $ g
    where 
        doStuff item (topoed,s) =
            let (s2, items) = dfs' g item s
            in (items >< topoed, s2)
