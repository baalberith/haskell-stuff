import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
 

-- Continuations
 
cfold :: (a -> b -> b) -> b -> [a] -> b
cfold f z l = cfold' (\x t g -> f x (g t)) z l 

cfold' :: (a -> b -> (b -> b) -> b) -> b -> [a] -> b
cfold' f z [] = z
cfold' f z (x:xs) = f x z (\y -> cfold' f y xs)
 
exl = cfold' (\x t g -> (x : g t)) [] [1..3]
 
-- cfold' (\x t g -> (x : g t)) [] [1,2,3]
-- cfold' f [] [1,2,3]
-- f 1 [] (\y -> cfold' f y [2,3])
-- 1 : ((\y -> cfold' f y [2,3]) [])
-- 1 : (cfold' f [] [2,3])
-- 1 : (f 2 [] (\y -> cfold' f y [3]))
-- 1 : (2 : ((\y -> cfold' f y [3]) []))
-- 1 : (2 : (cfold' f [] [3]))
-- 1 : (2 : (f 3 [] (\y -> cfold' f y [])))
-- 1 : (2 : (3 : (cfold' f [] [])))
-- 1 : (2 : (3 : []))
-- [1,2,3]

exr = cfold' (\x t g -> g (x : t)) [] [1..3]

-- cfold' (\x t g -> g (x:t)) [] [1,2,3]
-- cfold' f [] [1,2,3]
-- (\x t g -> g (x:t)) 1 [] (\y -> cfold' f y [2,3])
-- (\g -> g [1]) (\y -> cfold' f y [2,3])
-- (\y -> cfold' f y [2,3]) [1]
-- cfold' f [1] [2,3]
-- (\x t g -> g (x:t)) 2 [1] (\y -> cfold' f y [3])
-- cfold' f (2:[1]) [3]
-- cfold' f [2,1] [3]
-- (\x t g -> g (x:t)) 3 [2,1] (\y -> cfold' f y [])
-- cfold' f (3:[2,1]) []
-- [3,2,1]
   

-- Tree folding   

data BTree a
    = BLeaf a
    | BBranch (BTree a) a (BTree a)
    
treeFoldl :: (a -> b -> a) -> a -> BTree b -> a
treeFoldl f i (BLeaf x) = f i x
treeFoldl f i (BBranch left x right) = treeFoldl f (f (treeFoldl f i left) x) right

elementsl t = treeFoldl (\i a -> i ++ [a]) [] t

treeFoldr :: (a -> b -> b) -> b -> BTree a -> b
treeFoldr f i (BLeaf x) = f x i
treeFoldr f i (BBranch left x right) = treeFoldr f (f x (treeFoldr f i right)) left

elementsr t = treeFoldr (\a i -> a:i) [] t


-- More on lists

plusplus :: [a] -> [a] -> [a]
plusplus []     = id
plusplus (x:xs) = (x:) . plusplus xs

plusplus1 = foldr (\a b -> (a:) . b) id
plusplus2 = foldr ((.) . (:)) id


-- Computations

data Graph v e = Graph [(Int,v)] [(Int,Int,e)]

search1 :: Graph v e -> Int -> Int -> Maybe [Int]
search1 g@(Graph vl el) src dst
  | src == dst = Just [src]
  | otherwise = search el
  where search [] = Nothing
        search ((u,v,_):es)
          | src == u =
              case search1 g v dst of
                Just p -> Just (u:p)
                Nothing -> search es
          | otherwise = search es
          
data Failable a = Success a | Fail String
          
search2 :: Graph v e -> Int -> Int -> Failable [Int]
search2 g@(Graph vl el) src dst
  | src == dst = Success [src]
  | otherwise = search el
  where search [] = Fail "No path"
        search ((u,v,_):es)
          | src == u =
              case search2 g v dst of
                Success p -> Success (u:p)
                _         -> search es
          | otherwise = search es

search3 :: Graph v e -> Int -> Int -> [[Int]]
search3 g@(Graph vl el) src dst
  | src == dst = [[src]]
  | otherwise = search el
  where search [] = []
        search ((u,v,_):es)
          | src == u =
              map (u:) (search3 g v dst) ++
              search es
          | otherwise = search es

class Computation c where
  success :: a -> c a
  failure :: String -> c a
  augment :: c a -> (a -> c b) -> c b
  combine :: c a -> c a -> c a

instance Computation Maybe where
  success = Just
  failure = const Nothing
  augment (Just x) f = f x
  augment Nothing _ = Nothing
  combine Nothing y = y
  combine x _ = x

instance Computation Failable where
    success = Success
    failure = Fail
    augment (Success x) f = f x
    augment (Fail s) _ = Fail s
    combine (Fail _) y = y
    combine x _ = x

instance Computation [] where
    success a = [a]
    failure = const []
    augment l f = concat (map f l)
    combine = (++)

searchAll :: (Computation c) => Graph v e -> Int -> Int -> c [Int]
searchAll g@(Graph vl el) src dst
  | src == dst = success [src]
  | otherwise = search el
  where search [] = failure "no path"
        search ((u,v,_):es)
          | src == u = (searchAll g v dst `augment`
                          (success . (u:)))
                        `combine` search es
          | otherwise = search es


-- Functors

data Tree a = Leaf a
  | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch left right) =
      Branch (fmap f left) (fmap f right)

instance Eq a => Eq (Tree a) where
    Leaf a == Leaf b = a == b
    Branch l r == Branch l' r' = l == l' && r == r'
    _ == _ = False


-- Monads

mapTreeState :: (a -> state -> (state, b)) -> Tree a -> state -> (state, Tree b)
mapTreeState f (Leaf a) state =
    let (state', b) = f a state
    in (state', Leaf b)
mapTreeState f (Branch lhs rhs) state =
    let (state' , lhs') = mapTreeState f lhs state
        (state'', rhs') = mapTreeState f rhs state'
    in (state'', Branch lhs' rhs')

mapTreeStateM :: (a -> State st b) -> Tree a -> State st (Tree b)
mapTreeStateM f (Leaf a) =
  f a >>= \b ->
  return (Leaf b)
mapTreeStateM f (Branch lhs rhs) =
  mapTreeStateM f lhs >>= \lhs' ->
  mapTreeStateM f rhs >>= \rhs' ->
  return (Branch lhs' rhs')
   
mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f (Leaf a) = do
  b <- f a
  return (Leaf b)
mapTreeM f (Branch lhs rhs) = do
  lhs' <- mapTreeM f lhs
  rhs' <- mapTreeM f rhs
  return (Branch lhs' rhs')

numberTree :: Tree a -> State Int (Tree (a, Int))
numberTree tree = mapTreeM number tree
    where number v = do
            cur <- get
            put (cur+1)
            return (v,cur)

numberedTree = runState (numberTree testTree) 1

fluffLeaves :: Tree a -> State [a] (Tree [a])
fluffLeaves tree = mapTreeM fluff tree
    where fluff v = do
            cur <- get
            put (v:cur)
            return (v:cur)

fluffedTree = runState (fluffLeaves testTree) []

testTree =
  Branch
    (Branch
      (Leaf 'a')
      (Branch
        (Leaf 'b')
        (Leaf 'c')))
    (Branch
      (Leaf 'd')
      (Leaf 'e'))

searchAll1 :: (Monad m) => Graph v e -> Int -> Int -> m [Int]
searchAll1 g@(Graph vl el) src dst
    | src == dst = return [src]
    | otherwise = search el
    where search [] = fail "no path"
          search ((u,v,_):es)
              | src == u =
                   searchAll1 g v dst >>= \path ->
                   return (u:path)
              | otherwise = search es

testGraph = Graph [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd')]
            [(0,1,'l'), (0,2,'m'), (1,3,'n'), (2,3,'m')]

ex1 = searchAll1 testGraph 0 3 :: Maybe [Int]
ex2 = searchAll1 testGraph 0 3 :: [[Int]]

instance Monad (Either String) where
  return = Right
  Left s >>= _ = Left s
  Right x >>= f = f x
  fail = Left

ex3 = searchAll1 testGraph 3 0 :: Either String [Int]
ex4 = searchAll1 testGraph 3 0 :: IO [Int]


-- Combinators

a = mapM print [1,2,3,4,5]
b = foldM (\a b ->
  putStrLn (show a ++ "+" ++ show b ++ "=" ++ show (a+b)) >> return (a+b)) 0 [1..5]
c = sequence [print 1, print 2.0, print '3']
d = liftM (+1) (Just 2)
e = mapM (\l -> when (not $ null l) (putStrLn l)) ["","abc","def","","","ghi"]
f = join (Just (Just 'a'))
g = join (return (putStrLn "hello"))


-- MonadPlus

searchAll2 :: (MonadPlus m) => Graph v e -> Int -> Int -> m [Int]
searchAll2 g@(Graph vl el) src dst
    | src == dst = return [src]
    | otherwise = search el
    where search [] = fail "no path"
          search ((u,v,_):es)
              | src == u =
                (searchAll2 g v dst >>= \path ->
                  return (u:path)) `mplus`
                    search es
              | otherwise = search es

ex5 = searchAll2 testGraph 0 3 :: [[Int]]


-- Monad transformers

mapTreeM1 :: (MonadTrans t, Monad (t IO), Show a) =>
            (a -> t IO a1) -> Tree a -> t IO (Tree a1)
mapTreeM1 action (Leaf a) = do
  lift (putStrLn ("Leaf " ++ show a))
  b <- action a
  return (Leaf b)
mapTreeM1 action (Branch lhs rhs) = do
  lift (putStrLn "Branch")
  lhs' <- mapTreeM1 action lhs
  rhs' <- mapTreeM1 action rhs
  return (Branch lhs' rhs')
  
getT :: Monad m => StateT s m s
getT = StateT (\s -> return (s, s))

putT :: Monad m => s -> StateT s m ()
putT s = StateT (\_ -> return ((), s))

numberTree1 :: (Num s, Show a) => Tree a -> StateT s IO (Tree (a, s))
numberTree1 tree = mapTreeM1 number tree
    where number v = do
            cur <- getT
            putT (cur+1)
            return (v,cur)
            
numberedTree1 = evalStateT (numberTree1 testTree) 0

searchAll3 :: (MonadPlus m) => Graph v e -> Int -> Int -> StateT [Int] m [Int]
searchAll3 g@(Graph vl el) src dst
  | src == dst = do
      visited <- getT
      putT (src:visited)
      return [src]
  | otherwise = do
      visited <- getT
      putT (src:visited)
      if src `elem` visited
        then mzero
        else search el
  where
    search [] = mzero
    search ((u,v,_):es)
        | src == u =
          (do path <- searchAll3 g v dst
              return (u:path)) `mplus`
          search es
        | otherwise = search es

testGraph' = Graph [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd')]
             [(0,1,'l'), (1,0,'m'), (0,2,'n'), (1,3,'o'), (2,3,'p')]
             
ex6 = evalStateT (searchAll3 testGraph' 0 3) [] :: [[Int]]

newtype ListT m e = ListT { runListT :: m [e] }

instance Monad m => Monad (ListT m) where
    return x = ListT (return [x])
    fail   s = ListT (return [] )
    ListT m >>= k = ListT $ do
      l  <- m
      l' <- mapM (runListT . k) l
      return (concat l')

instance Monad m => MonadPlus (ListT m) where
    mzero = ListT (return [])
    ListT m1 `mplus` ListT m2 = ListT $ do
      l1 <- m1
      l2 <- m2
      return (l1 ++ l2)

instance MonadTrans ListT where
    lift x = ListT (do a <- x; return [a])

searchAll4 :: (MonadPlus (t IO), MonadTrans t) =>
     Graph v e -> Int -> Int -> t IO [Int]
searchAll4 g@(Graph vl el) src dst
    | src == dst = do
      lift $ putStrLn $
        "Exploring " ++ show src ++ " -> " ++ show dst
      return [src]
    | otherwise  = do
      lift $ putStrLn $
        "Exploring " ++ show src ++ " -> " ++ show dst
      search el
  where
    search [] = mzero
    search ((u,v,_):es)
        | src == u  =
          (do path <- searchAll4 g v dst
              return (u:path)) `mplus`
          search es
        | otherwise = search es

ex7 = runListT (searchAll4 testGraph 0 3)

searchAll5 :: (MonadPlus (t IO), MonadTrans t, MonadPlus (StateT [Int] (t IO))) =>
              Graph v e -> Int -> Int -> StateT [Int] (t IO) [Int]
searchAll5 g@(Graph vl el) src dst
    | src == dst = do
      lift $ lift $ putStrLn $
        "Exploring " ++ show src ++ " -> " ++ show dst
      visited <- getT
      putT (src:visited)
      return [src]
    | otherwise  = do
      lift $ lift $ putStrLn $
        "Exploring " ++ show src ++ " -> " ++ show dst
      visited <- getT
      putT (src:visited)
      if src `elem` visited
        then mzero
        else search el
  where
    search [] = mzero
    search ((u,v,_):es)
        | src == u  =
          (do path <- searchAll5 g v dst
              return (u:path)) `mplus`
          search es
        | otherwise = search es
        
ex8 = runListT (evalStateT (searchAll5 testGraph' 0 3) [])

