

data NestedList n = Element n | SubList [NestedList n] deriving (Show)
flatten :: NestedList n -> [n]

flatten (SubList (h:ls)) = flatten h ++ flatten (SubList ls)
flatten (SubList []) = []
flatten (Element e) = [e]
{-  Unfortunately this only works with sublist first so
	something like flatten (SubList [Element 5, SubList [Element 6, Element 7], Element 8]) will 
	put them into one list [5, 6, 7, 8]
 -}


rev :: NestedList n -> NestedList n
rev (Element e) = Element e
rev (SubList ls) = SubList (map rev ( reverse ls))
{- As long as sublist is first, this seems to be working correctly 
so (SubList [Element 5, SubList [Element 6, Element 7], Element 8])
yields SubList [Element 8,SubList [Element 7,Element 6],Element 5]

Map is a great built in function that calls the function (in this case rev) on every element
of (reverse ls) which is just a list that was reversed, as reverse just reverses the contents of a normal list
-}

myfunction :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]

myfunction x y z  =
	if (z x)
		then
			y {- should be like (myappend y x)-}
		else
			Nothing


{-myAppend x (Element ls) = Element (ls)
myAppend x (SubList []) = SubList [Element [h]]
myAppend x (SubList h:ls) = SubList . map (myAppend h) ( ls)
-}
{-his got messed up a bit but the last line is supposed to call my append on x and then the rest of the list
while pumping that input into sublist which was supposed to append everything while sort of flattening it
The dot operator feeds output from the right function into the left function, so sublist should have received a list
-}

--myfunction 3 Just [4, 5] (\v -> (v > 5))


--checklist :: [a] -> (a -> Bool) -> Maybe [a]

checklist list func = 
	if (func list)
		then
			Just list
		else
		Nothing

--checkappend Maybe [a] -> Maybe [a] -> (a -> Bool) -> a

checkappend monadA monadB funcA =
	if(funcA monadA)
		then
			monadB --(myAppend monadA monadB)
			else
				Nothing
