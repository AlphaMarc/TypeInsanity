module TypeSolution (u) where


import Prelude hiding (all, flip, map, filter, and, Bool, True, False)

u :: t
u = undefined

data R -- Red
data G -- Green
data B -- Blue
data W -- White

data Cube u f r b l d

type Cubes = (Cube1 ::: Cube2 ::: Cube3 ::: Cube4 ::: Nil)
type ColorCubes = (CubeRed ::: CubeBlue ::: Nil)
type Solution1Begin = (SCube1:::SCube2:::SCube3:::Nil)
type Solution2Begin = (SCube2:::SCube3:::Nil)
type SolutionsBegin = Solution1Begin ::: Solution2Begin ::: Nil

type SCube1 = Cube G B B R W G
type SCube2 = Cube R G R W B W
type SCube3 = Cube R W G B R R
type SCube4 = Cube W R W G G B

type CubeRed  = Cube R R R R R R
type CubeBlue = Cube B B B B B B

type Cube1 = Cube B G W G B R
type Cube2 = Cube W G B W R R
type Cube3 = Cube G W R B R R
type Cube4 = Cube B R G G W W

class Transforms u f r b l d where
  rot :: Cube u f r b l d -> Cube u r b l f d
  twist :: Cube u f r b l d -> Cube f r u l d b
  flip :: Cube u f r b l d -> Cube d l b r f u


instance Transforms u f r b l d  where
  rot   = u
  twist = u
  flip  = u

-- create boolean algebra
data True
data False

class And b1 b2 b | b1 b2 -> b where
  and :: b1 -> b2 -> b

instance And True True True where
   and = u
instance And True False False where
   and = u
instance And False True False where
   and = u
instance And False False False where
   and = u

-- define lists in type system
data Nil
--data Cons x xs (is cumbersome to see)
data x ::: xs
infixr 5 :::

class ListConcat l1 l2 l | l1 l2 -> l where
  listConcat :: l1 -> l2 -> l

instance ListConcat Nil l l where
  listConcat = u

instance (ListConcat xs ys zs) => ListConcat (x:::xs) ys (x:::zs) where
  listConcat = u

class Apply f a b | f a -> b where
  apply :: f -> a -> b


-- we need to define types so that we can use our functions at the type level
data Rotation
data Twist
data Flip

instance Apply Rotation (Cube u f r b l d) (Cube u r b l f d) where
  apply = u
instance Apply Twist    (Cube u f r b l d) (Cube f r u l d b) where
  apply = u
instance Apply Flip     (Cube u f r b l d) (Cube d l b r f u) where
  apply = u


class Map f xs zs | f xs -> zs where
  map :: f -> xs -> zs

instance Map f Nil Nil where
  map = u
-- we need two types constraints : 1) to call apply on the head 2) to call map on the tail (recursive call)
instance (Apply f x y, Map f xs ys) => Map f (x:::xs) (y:::ys) where
  map = u


-- we can create a filter using the same methods
class Filter f xs zs | f xs -> zs where
  filter :: f -> xs -> zs

instance Filter f Nil Nil where
  filter = u
instance (Apply f x b , Filter f xs ys, AppendIf b x ys zs) => Filter f (x ::: xs) zs where
  filter = u

-- we append the element of type x to the list only if b is True
-- otherwise we return the list of type ys
class AppendIf b x ys zs | b x ys -> zs where
  appendIf :: b -> x -> ys -> zs
instance AppendIf True x ys (x ::: ys) where
  appendIf = u
instance AppendIf False x ys ys where
  appendIf = u


-- this is the implementation of join for lists
-- Are we implementing the list monad at the type level to
-- be able to represent list comprehension
class MapAppend f xs zs | f xs -> zs where
  mapAppend :: f -> xs -> zs

instance MapAppend f Nil Nil where
  mapAppend = u
instance (Map f xs ys, ListConcat xs ys zs) => MapAppend f xs zs where
  mapAppend = u


-- two times reduction (for all twists)
class MapAppend2 f xs zs | f xs -> zs where
  mapAppend2 :: f -> xs -> zs

instance MapAppend2 f Nil Nil where
  mapAppend2 = u
instance (Map f xs ys, MapAppend f ys ys2, ListConcat xs ys2 zs) => MapAppend2 f xs zs where
  mapAppend2 = u


  -- three times reduction (for all rotations)
class MapAppend3 f xs zs | f xs -> zs where
    mapAppend3 :: f -> xs -> zs
instance MapAppend3 f Nil Nil where
    mapAppend3 = u
instance (Map f xs ys, MapAppend2 f ys ys2,  ListConcat xs ys2 zs) => MapAppend3 f xs zs where
    mapAppend3 = u

-- from a cube generate all combinations
data Orientations
instance (MapAppend Flip (c:::Nil) fs, MapAppend2 Twist fs ts, MapAppend3 Rotation ts zs) => Apply Orientations c zs where
  apply = u


-- Create not equal to test for face of different colors
class NE x y b | x y -> b where
  ne :: x -> y -> b

instance NE R R False where ne = u
instance NE R G True  where ne = u
instance NE R B True  where ne = u
instance NE R W True  where ne = u
instance NE G R True  where ne = u
instance NE G G False where ne = u
instance NE G B True  where ne = u
instance NE G W True  where ne = u
instance NE B R True  where ne = u
instance NE B G True  where ne = u
instance NE B B False where ne = u
instance NE B W True  where ne = u
instance NE W R True  where ne = u
instance NE W G True  where ne = u
instance NE W B True  where ne = u
instance NE W W False where ne = u


-- define class ALl to check if all elements of a list are True
class All l b | l -> b where
  all :: l -> b

-- define class compatible to check if all faces of two cubes are of different colors
class Compatible c1 c2 b | c1 c2 -> b where
  compatible :: c1 -> c2 -> b


-- Allowed class generalizes Compatible over lists
class Allowed c cs b | c cs -> b where
  allowed :: c -> cs -> b


-- And last but not least we can create the Solutions class that will
-- take a list of cubes and return the list of all possible solutions
class Solutions cs ss | cs -> ss where
  solutions :: cs -> ss



-- this class recurses over the orientations generated and current solutions to
-- remove the ones that do not fit
class AllowedCombinations os sols zs | os sols -> zs where
    allowedCombinations :: os -> sols -> zs


-- Recurses over the orientations of the new cube checking against existing solutions
class MatchingOrientations os sol zs | os sol -> zs where
  matchingOrientations :: os -> sol -> zs
