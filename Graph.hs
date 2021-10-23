module Graph where
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S




g ? x = M.findWithDefault [] x g

createAdjList :: (Ord k) => [(k, a)] -> M.Map k [a]
createAdjList = M.fromListWith (++) . fixing
    where
        fixing = map (\(x,y) -> (x, [y]))


createUniDirectional :: (Ord k) => [(k, k)] -> M.Map k [k]
createUniDirectional = createAdjList . fixing 
    where
        fixing x = x >>= (\(x,y) -> [(x,y), (y,x)])


degree g x = length $ g ? x

dfs g x = dfs' g x S.empty

dfs' g x s = dfs'' x s
    where
        dfs'' x s
            | x `S.member` s = (S.empty, [])
            | otherwise = foldr doThis  (S.insert x s, [x]) (g ? x)
            where
                doThis = \item (s1, traversed) ->
                    let (s2,traversed') = dfs'' item s1
                    in  (S.union s2 s1, traversed' ++ traversed)
