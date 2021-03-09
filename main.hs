
import Data.List ( find )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( isNothing )

type Ptr = Char

{-

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

-}

data Tree a = Tree ( Tree a ) a ( Tree a )
instance Functor Tree where
    fmap f (Tree left_tree node_value right_tree) = Tree (fmap f left_tree) (f node_value) (fmap f right_tree)

tree_value :: Tree a -> Int -> a
tree_value ( Tree _ node_value _ ) 0 = node_value -- If depth == 0, get current value
tree_value ( Tree left_tree _ right_tree ) n = -- If > 0
    case divMod ( n - 1 ) 2 of
    (quotient, 0) -> tree_value left_tree quotient -- Even
    (quotient, 1) -> tree_value left_tree quotient -- Odd

tail' :: [ Ptr ]  -> [ Ptr ] 
tail' [ el ]  = [ el ] -- modified so that tail of single-element-list is that same list, not empty
tail' xs = tail xs;
-- tail' [0,1] = [1], tail' [1] = [1], tail' [] = undefined

el_not_in_tails :: Ptr -> [ [ Ptr ]  ]  -> Bool
el_not_in_tails el list = not $ any ( elem el ) ( map tail' list )

first_element :: [ [ Ptr ] ] -> Ptr
first_element [] = error "Can't operate on empty list"
first_element (h:_) = head h
-- ["ABC", "D", "EF] -> 'A'

dequeue_if :: Ptr -> [ Ptr ]  -> [ Ptr ] 
dequeue_if _ [ ]  = [ ] 
dequeue_if el ( h:t ) = if ( h == el ) then t else h:t
-- If el is the first character of a string, remove it
-- Otherwise, do nothing
-- dequeue_if 'a' "abc" = "bc"; dequeue_if 'b' "abc" = "abc"

dequeue_delete_list_if :: Ptr -> [ [ Ptr ] ] -> [ [ Ptr ] ]
dequeue_delete_list_if _ [ ] = [ ]
dequeue_delete_list_if el ( h:t ) = (if (null dequeued) then [ ] else [ dequeued ]) ++ dequeue_delete_list_if el t
  where dequeued = dequeue_if el h

unsafe_just :: Maybe a -> a
unsafe_just (Just x) = x
unsafe_just Nothing = error "Could not resolve consistent linearisation"

{-
removal_pass :: ( [ Ptr ] , [ [ Ptr ]  ] ) -> ( [ Ptr ] , [ [ Ptr ]  ] )
removal_pass ( chosen, unchosen ) = (chosen ++ [ fst_match_head ] , map ( dequeue_if fst_match_head ) unchosen)
    where fst_match_head = head $ unsafe_just $ find ( ( flip el_not_in_tails unchosen ) . head ) unchosen
-}

unique_concat :: [ [ Ptr ] ] -> [ Ptr ]
unique_concat [ ] = [ ]
unique_concat ( h:t ) = element_to_remove : (unique_concat $ dequeue_delete_list_if element_to_remove t)
  where element_to_remove = head h -- list head if flattened
-- E.g. ["A", "AB", "AC"] -> ["B", "C"] -> ["C"] -> [] giving "ABC"

removal_pass :: ( [ Ptr ] , [ [ Ptr ]  ] ) -> ( [ Ptr ] , [ [ Ptr ]  ] )
removal_pass ( chosen, unchosen ) = if (isNothing fst_match_maybe) then (chosen ++ unique_concat unchosen, [ ]) else (chosen ++ [ head $ unsafe_just $ fst_match_maybe ] , map ( dequeue_if $ head $ unsafe_just $ fst_match_maybe ) unchosen)
  where fst_match_maybe = find ( ( flip el_not_in_tails unchosen ) . head ) unchosen

all_passes :: ( [ Ptr ] , [ [ Ptr ]  ] ) -> ( [ Ptr ] , [ [ Ptr ]  ] )
all_passes ( chosen, unchosen ) =
  if ( null unchosen ) then ( chosen, [ ] )
  else all_passes $ removal_pass ( chosen, unchosen )

linearise :: [ [ Ptr ] ] -> [ Ptr ]
linearise list = fst $ all_passes ( [ ], list )

{-
removal_pass_ :: (Ptr, [ [ Ptr ]  ] ) -> [ [ Ptr ]  ]  -> (Ptr, [ [ Ptr ]  ] )
removal_pass_ (chosen, )
-}
{-
class O
class A extends O
class B extends O
class C extends O
class D extends O
class E extends O
class K1 extends A, B, C
class K2 extends D, B, E
class K3 extends D, A
class Z extends K1, K2, K3
-}

main = do
  let lists = [ "AO", "BO", "CO", "DO", "EO" ] 
  let memoised = Map.empty
  print $ lists
  let fst_match_h = head $ unsafe_just $ find ( ( flip el_not_in_tails lists ) . head ) lists
  print fst_match_h
  --print $ dequeue_if 'A' $ lists !! 2
  print $ map ( dequeue_if fst_match_h ) lists
  let first_pass = removal_pass ( "", lists )
  let pass =  iterate removal_pass first_pass !! 5
  print pass
  print $ concat $ snd pass
  let left = ["O", "OB", "A", "O"]
  print $ dequeue_delete_list_if 'O' $ left
  print $ first_element left
  print $ unique_concat left
  print $ all_passes ( [ ], lists )