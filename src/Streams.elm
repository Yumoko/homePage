module Streams ( Stream
               , takeNth
               , toList
               , cycle
               , next
               , BiStream
               , biStream
               , current
               , left
               , right
               , goTo
               ) where

import List 
import Dict 

-------------------------------------------------------------------------------
-- Streams --

type Stream a = Stream (() -> (a,Stream a))

nats : Stream Int
nats = 
  let f n = Stream (\() -> (n,f(n+1)))
  in f 1

takeNth : Int -> Stream a -> a
takeNth n (Stream s) = 
  if n == 0
  then fst (s ())
  else takeNth (n-1) (snd (s ()))

toList : Stream a -> Int -> List a
toList (Stream s) n = 
  if n == 0 
  then []
  else let (v,s') = s ()
       in v :: (toList s' (n-1))  

cycle : List a -> a -> Stream a
cycle xs def = 
    let l = List.length xs
        dict = Dict.fromList (tag xs)
        safeGet i = Maybe.withDefault def (Dict.get (i % l) dict)
        f n = Stream (\() -> (safeGet n, f(n+1)))
    in f 0

next : Stream a -> (a,Stream a)
next (Stream s ) = s ()

map : (a -> b) -> Stream a -> Stream b
map f s = 
  let (v,s') = next s
  in Stream (\() -> (f v, map f s'))   

-------------------------------------------------------------------------------
-- BiStreams --

type BiStream a = BiStream a (Stream a) (Stream a)

biStream : List a -> a -> BiStream a
biStream xs def = 
  case xs of 
    [] -> BiStream def (cycle [] def)  (cycle [] def)
    (x::_) ->
      let l = List.length xs
          dict = Dict.fromList (tag xs)
          safeGet i = Maybe.withDefault def (Dict.get (i % l) dict)
            
          leftStr  n = Stream (\() -> (safeGet (l-n), leftStr (n+1))) 
          rightStr n = Stream (\() -> (safeGet n, rightStr (n+1)))

      in BiStream x (leftStr 1) (rightStr 1)    

current : BiStream a -> a
current (BiStream v l r) = v

left : BiStream a -> BiStream a
left (BiStream v l r) =
  let (newCurrent, newLeft) = next l
      newRight = Stream (\() -> (v,r))
  in BiStream newCurrent newLeft newRight  

right : BiStream a -> BiStream a
right (BiStream v l r) =
  let (newCurrent, newRight) = next r
      newLeft = Stream (\() -> (v,l))
  in BiStream newCurrent newLeft newRight

goTo : BiStream a -> (a -> Bool) -> BiStream a
goTo bs p = 
 if p (current bs)
    then bs
    else goTo (right bs) p

integers = BiStream 0 (map (\n -> (-n)) nats) nats

test2 = biStream ["blibli", "blouloub","blublu"] ""
-------------------------------------------------------------------------------
-- Misc --

tag : List a -> List (Int,a)
tag xs = 
  let go n xs = 
    case xs of
     [] -> []
     (x::xs') -> (n,x)::(go (n+1) xs')
  in go 0 xs