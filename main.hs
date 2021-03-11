
import Data.List ( find )
import Data.Maybe ( isNothing )

type Ptr = Char

tail' :: [ Ptr ]  -> [ Ptr ] 
tail' [ el ]  = [ el ] -- modified so that tail of single-element-list is that same list, not empty
tail' xs = tail xs;
-- tail' [0,1] = [1], tail' [1] = [1], tail' [] = undefined

el_not_in_tails :: Ptr -> [ [ Ptr ]  ]  -> Bool
el_not_in_tails el list = not $ any ( elem el ) ( map tail' list )
-- None of the tails of list contain el

dequeue_if :: Ptr -> [ Ptr ]  -> [ Ptr ] 
dequeue_if _ [ ]  = [ ] 
dequeue_if el ( h:t ) = if ( h == el ) then t else h:t
-- If el is the first character of a string, remove it
-- Otherwise, do nothing
-- dequeue_if 'a' "abc" = "bc"; dequeue_if 'b' "abc" = "abc"

dequeue_delete_list_if :: Ptr -> [ [ Ptr ] ] -> [ [ Ptr ] ]
dequeue_delete_list_if _ [ ] = [ ]
dequeue_delete_list_if el ( h:t ) =
  (if (null dequeued) then [ ] else [ dequeued ]) ++ dequeue_delete_list_if el t
  where dequeued = dequeue_if el h

unsafe_just :: Maybe a -> a
unsafe_just (Just x) = x

unique_concat :: [ [ Ptr ] ] -> [ Ptr ]
unique_concat [ ] = [ ]
unique_concat ( h:t ) = element_to_remove : (unique_concat $ dequeue_delete_list_if element_to_remove t)
  where element_to_remove = head h -- list head if flattened
-- E.g. ["A", "AB", "AC"] -> ["B", "C"] -> ["C"] -> [] giving "ABC"

removal_pass :: ( [ Ptr ] , [ [ Ptr ]  ] ) -> ( [ Ptr ] , [ [ Ptr ]  ] )
removal_pass ( chosen, unchosen ) =
  if (isNothing fst_match_maybe)
    then (chosen ++ unique_concat unchosen, [ ])
    else let fst_match_h = head $ unsafe_just $ fst_match_maybe in
      (chosen ++ [ fst_match_h ] , map ( dequeue_if $ fst_match_h ) unchosen)
  where fst_match_maybe = find ( ( flip el_not_in_tails unchosen ) . head ) unchosen
-- el_not_in_tails: arg1 is not in any tails of arg2
-- flip el_not_in_tails:  arg2 is not in any tails of arg1-
-- flip el_not_in_tails unchosen: function taking a single argument and determines whether it's in any of the tails of unchosen

all_passes :: ( [ Ptr ] , [ [ Ptr ]  ] ) -> ( [ Ptr ] , [ [ Ptr ]  ] )
all_passes ( chosen, unchosen ) =
  if ( null unchosen ) then ( chosen, [ ] )
  else all_passes $ removal_pass ( chosen, unchosen )

linearise :: [ [ Ptr ] ] -> [ Ptr ]
linearise list = fst $ all_passes ( [ ], list )

data LinkedList a = Link a (LinkedList a) 
                | Nil deriving (Show, Eq)
{-
class O : O
class A extends O : AO
class B extends O : CO
class C extends O : CO
class D extends O : DO
class E extends O : EO
class F extends A, B, C : FABCO
class G extends D, B, E : GBEO
class H extends D, A : HDAO
class I extends F, G, H : IFGHDABCEO
-}

main = do
{-
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
  print $ all_passes ( [ ], lists )
-}
  --let lists = (Link 10 (Link 99 (Link 11 (Link 1 Nil))))
  let mros = [ "FABCO", "GDBEO", "HDAO" ]
  print $ 'I' : linearise mros
  