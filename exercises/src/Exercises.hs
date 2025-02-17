{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where

import Optics.Core
import Optics.TH
import Data.Char
import Control.Monad.State

{-
Here are some example data types from the slides, and some test data to
experiment with.
-}

data Person = MkPerson  {  _personName  :: String
                        ,  _personAge   :: Int
                        ,  _personPets  :: [Pet] }
  deriving (Show, Eq)

data Pet = MkPet  {  _petName   :: String
                  ,  _petAge    :: Int }
  deriving (Show, Eq)

alice, bob, pepe :: Person
alice = MkPerson { _personName = "Alice", _personAge = 65, _personPets = [] }
bob = MkPerson { _personName = "Bob"
               , _personAge  = 42
               , _personPets = [ MkPet { _petName = "Mr Scruffy", _petAge = 3 } ]
               }
pepe = MkPerson { _personName = "Pepe"
               , _personAge  = 2
               , _personPets = [ MkPet { _petName = "pepita", _petAge = 3 },
                                 MkPet { _petName = "nala", _petAge = 5 }
                               ]
               }

$(makeLenses ''Person)
$(makeLenses ''Pet)

{-
The 'ages' fold extracts all the ages containined within a 'Person'.

 * Define a function that tests whether any of the ages exceed the given
   integer:

     anyOlderThan :: Int -> Person -> Bool

   (Avoid producing an intermediate list. The 'Optics.Fold' module has many
   useful eliminators for folds.)

 * Define the same function using 'has', which tests whether a fold returns any
   values.  Hint: check out 'filtered :: (a -> Bool) -> AffineFold a a'.
-}

ages :: Fold Person Int
ages = personAge `summing` (personPets % folded % petAge)

anyOlderThan :: Int -> Person -> Bool
anyOlderThan age person = anyOf ages (> age) person

anyOlderThan' :: Int -> Person -> Bool
anyOlderThan' age person = has (ages % filtered (> age)) person


{-
 * Define a `Fold Person Pet` that visits every Pet whose age is greater than
   the given integer:

     petsOlderThan :: Int -> Fold Person Pet

 * Use it to define a function that returns the list of names of such pets.

     petNamesOlderThan :: Int -> Person -> [String]

 * Why is 'filtered' a fold rather than a traversal?  Hint: read the Haddocks
   for 'unsafeFiltered'.
-}

petsOlderThan :: Int -> Fold Person Pet
petsOlderThan age = personPets % folded % (filtered ((> age) . (view petAge)))

petNamesOlderThan :: Int -> Person -> [String]
petNamesOlderThan age person = toListOf (petsOlderThan age % petName) person


{-
 * Define a traversal that visits the name of a person and all the names of
   their pets:

     names :: Traversal' Person String

 * Use it to implement a function that capitalises all the names (using
   'Data.Char.toUpper'):

     capitaliseNames :: Person -> Person
-}

names :: Traversal' Person String
names = personName `adjoin` (personPets % traversed % petName)

capitaliseNames :: Person -> Person
capitaliseNames person = over (names % traversed) toUpper person


{-
 * Define a function that takes a list of 'Person's, each for each name that
   each of them contain (including pets' names), print out the name and read a
   replacement value from standard input:

    replaceNames :: [Person] -> IO [Person]

  For example:

    ghci> replaceNames [alice,bob]
    Replacement for Alice: Charlie
    Replacement for Bob: Bob
    Replacement for Mr Scruffy: Tiddles
    [MkPerson {_personName = "Charlie", _personAge = 65, _personPets = []},MkPerson {_personName = "Bob", _personAge = 42, _personPets = [MkPet {_petName = "Tiddles", _petAge = 3}]}]
-}

replaceNames :: [Person] -> IO [Person]
replaceNames people = traverseOf (traversed % names) promptForName people
  where
    promptForName :: String -> IO String
    promptForName currentName = do
      putStr $ "Replacement for " ++ currentName ++ ": "
      getLine


{-
Here is a datatype of binary trees with labels at the nodes and leaves.

 * Define a traversal of the leaf labels:

     leaves :: Traversal (Tree a b) (Tree a' b) a a'

   Hint: use 'traversalVL'.

 * Test your traversal by using it to 'print' out the values stored in
   'treeExample'.  Try 'traverseOf' and 'traverseOf_'.  How and why do they
   differ?

 * Define pre-order, in-order and post-order traversals of the node
   labels:

      preorder, inorder, postorder :: Traversal (Tree a b) (Tree a b') b b'

   (A pre-order traversal visits the node label first, then the left subtree,
   then the right subtree. An in-order traversal visits the node label between
   the subtrees. A post-order traversal visits the node label after the
   subtrees.)

 * Define a function

     attachIndices :: Num e => Traversal s t a (e, a) -> s -> t

   that traverses the structure and annotates each value with its index
   (i.e. attach 0 to the first value visited by the traversal, 1 to the next,
   and so on).

   Compare the results of using 'attachIndices' with 'preorder', 'inorder' and
   'postorder' on 'treeExample'.

-}


data Tree a b = Leaf a
              | Node (Tree a b) b (Tree a b)
  deriving (Show, Eq)

treeExample :: Tree Int Char
treeExample =
    Node (Node (Node (Leaf 3)
                     'c'
                     (Leaf 2))
               'a'
               (Leaf 4))
         'b'
         (Node (Leaf 1)
               'd'
               (Leaf 2))

leaves :: Traversal (Tree a b) (Tree a' b) a a'
leaves = traversalVL go
  where
    go f (Leaf a) = fmap Leaf (f a)
    go f (Node t1 b t2) = Node <$> go f t1 <*> pure b <*> go f t2

preorder, inorder, postorder :: Traversal (Tree a b) (Tree a b') b b'

{- preorder = traversalVL go
  where
    go f (Leaf a) = pure $ Leaf a
    go f (Node t1 b t2) = do
      b' <- f b
      t1' <- go f t1
      t2' <- go f t2
      pure $ Node t1' b' t2' -}

preorder = traversalVL go
  where
    go _ (Leaf a) = pure $ Leaf a
    go f (Node t1 b t2) = flip Node <$> f b <*> go f t1 <*> go f t2
    -- go f (Node t1 b t2) = (\v t1 t2 -> Node t1 v t2) <$> f b <*> go f t1 <*> go f t2

inorder = traversalVL go
  where
    go _ (Leaf a) = pure $ Leaf a
    go f (Node t1 b t2) = do
      t1' <- go f t1
      b' <- f b
      t2' <- go f t2
      pure $ Node t1' b' t2'

postorder = traversalVL go
  where
    go f (Leaf a) = pure $ Leaf a
    go f (Node t1 b t2) = do
      t1' <- go f t1
      t2' <- go f t2
      b' <- f b
      pure $ Node t1' b' t2'


attachIndices :: Num e => Traversal s t a (e, a) -> s -> t
attachIndices traversal tree = evalState (traverseOf traversal addIndex tree) 0

{- addIndex :: Num e => a -> State e (e, a)
addIndex el = do
  currentIndex <- get
  put $ currentIndex + 1
  pure $ (currentIndex, el) -}

addIndex :: Num e => a -> State e (e, a)
addIndex el = state (\s -> ((s, el), s + 1))


{-
Here's a datatype of rose trees, where each node has a label and zero or more
children.

 * Define a (pre-order) traversal of the labels in a rose tree:

     rtreeLabels :: Traversal (RTree a) (RTree b) a b

 * Use 'attachIndices' defined above to annotate the labels of 'rtreeExample'
   with their indices.

 * Define a non-type changing traversal of the labels without using
   'traversalVL':

     rtreeLabels' :: Traversal' (RTree a) a

   Instead, use 'adjoin' to combine traversals built from the fields (you will
   also need 'traversed').  Why can't this produce a type-changing traversal?

-}

data RTree a = MkRTree { _rtreeLabel :: a
                       , _rtreeChildren :: [RTree a]
                       }
  deriving (Show, Traversable, Foldable, Functor)

$(makeLenses ''RTree)

rtreeExample :: RTree Char
rtreeExample = MkRTree 'a' [MkRTree 'b' [], MkRTree 'c' [MkRTree 'd' []], MkRTree 'e' []]

rtreeLabels :: Traversal (RTree a) (RTree b) a b
rtreeLabels = traversalVL go
  where
    go :: Applicative f => (a -> f b) -> RTree a -> f (RTree b)
    go f (MkRTree label children) = do
      label' <- f label
      children' <- traverse (go f) children
      pure $ MkRTree label' children'

rtreeLabels' :: Traversal' (RTree a) a
rtreeLabels' = rtreeLabel `adjoin` (rtreeChildren % traversed % rtreeLabels')

rtreeLabels'' :: Traversal (RTree a) (RTree b) a b
rtreeLabels'' = traversalVL traverse
{-
 * Define a "lens" focused on the value stored in 'Dubious', that uses the 'Int'
  field to count the number of times the structure has been accessed.

 * Define a type-preserving "traversal" that visits the value stored in
   'Duplicated' twice.  Can you define such a type-modifying traversal?

 * How does this violate the 'Lens' and 'Traversal' laws? Does it matter?
-}

data Dubious a = MkDubious Int a
  deriving (Show, Eq)

dubiousLens :: Lens (Dubious a) (Dubious b) a b
dubiousLens = lensVL go
  where
    go :: Functor f => (a -> f b) -> Dubious a -> f (Dubious b)
    go f (MkDubious count value) = MkDubious (count + 1) <$> f value


data Duplicated a = MkDuplicated a
  deriving Show

traverseDuplicated :: Traversal' (Duplicated a) a
traverseDuplicated = traversalVL g
  where
    g :: Applicative f => (a -> f a) -> Duplicated a -> f (Duplicated a)
    g f (MkDuplicated x) = (const MkDuplicated <$> f x) <*> f x



{-

 * The following table is scrambled. Unscramble it, then compare the type
   signatures of the class methods and the corresponding eliminators.  What do
   you notice?

|-------------|----------|-----------|------------|
| Class       | Method   | Optic     | Eliminator |
|-------------|----------|-----------|------------|
| Functor     | foldMap  | Traversal | foldMapOf  |
| Foldable    | traverse | Setter    | over       |
| Traversable | fmap     | Fold      | traverseOf |
|-------------|----------|-----------|------------|

-}
