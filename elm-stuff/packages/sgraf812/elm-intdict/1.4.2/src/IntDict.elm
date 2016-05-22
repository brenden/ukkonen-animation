module IntDict
    ( IntDict
    , isValidKey
    , empty, singleton, insert, update, remove
    , isEmpty, size, member, get, findMin, findMax
    , filter, map, foldl, foldr, partition
    , uniteWith, union, intersect, diff
    , keys, values, toList, fromList
    , toString'
    ) where


{-| # IntDict

This module exposes the same API as `Dict`.

# Technicalities

Since JavaScript's number type is kind of messed up, Elm's `Int` is not particularly
well-behaved wrt. bitwise operations. Currently, JS supports 32 bit integers, so there is
probably enough room for key picks. **However, when sanitizing user input, it is mandatory
that a prior `isValidKey` or one of the safe versions in `IntDict.Safe` is used!** This is
to prevent the overflow behavior.

This library is inspired by Haskells [IntMap](http://hackage.haskell.org/package/containers-0.2.0.1/docs/Data-IntMap.html),
which in turn implements Okasaki and Gill's [Fast mergable integer maps](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf).

As noted in the [references](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf), here are some runtimes:

*O(min(n, W))*: `insert`, `update`, `remove`, `get`, `member`

*O(n + m)*: `uniteWith`, `union`, `intersection`, `diff`

where *n* and *m* are the sizes of the first and second dictionary respectively and *W*
is the number of bits in `Int` (so a constant with current value 32).

# Data
@docs IntDict, isValidKey
# Build
@docs empty, singleton, insert, update, remove
# Query
@docs isEmpty, size, member, get, findMin, findMax
# Combine
@docs uniteWith, union, intersect, diff
# Lists
@docs keys, values, toList, fromList
# Transform
@docs map, foldl, foldr, filter, partition
# String representation
@docs toString'

-}


import Bitwise
import Maybe exposing (Maybe (..))
import List
import Debug


type alias KeyPrefix =
    { prefixBits : Int
    , branchingBit : Int -- already in 2^i form -> always > 0
    }


-- only so that we don't repeat ourselves
type alias InnerType v =
    { prefix : KeyPrefix
    , left : IntDict v
    , right : IntDict v
    , size : Int
    }


{-| A dictionary mapping `Int`s to values of a type `v`. Analogous to
`Dict Int v`.
-}
type IntDict v
    = Empty
    | Leaf { key : Int, value : v }
    | Inner (InnerType v)


{-| Validates that a given integer is usable as a key.
This is necessary due to JavaScript's weird number type.
Basically this assures that we can use the functions
from `Bitwise` without risking integer overflow.

**This function is a necessity for sanitizing user input!** Alternatively,
use the safe functions from `IntDict.Safe` which perform the check for you.

As with the current version of JavaScript (2015), only 32 bit signed integers are supported.
If this ever changes, contact me! Certain parts of the implementation depend on this! -}
isValidKey : Int -> Bool
isValidKey k =              -- perform some dirty JS magic to turn the double
    k `Bitwise.or` 0 == k   -- into an integer. We can then check for overflow.
                            -- This seems easier than checking for 32 bits.
                            -- `or` 0 is similar to `mod` <32bits>


-- SMART CONSTRUCTORS

-- not exported
inner : KeyPrefix -> IntDict v -> IntDict v -> IntDict v
inner p l r =
    case (l, r) of
        (Empty, _) -> r
        (_, Empty) -> l
        (_, _) -> Inner
            { prefix = p
            , left = l
            , right = r
            , size = size l + size r
            }

-- exported as the singleton alias
leaf : Int -> v -> IntDict v
leaf k v = Leaf
    { key = k
    , value = v
    }

-- SOME PRIMITIVES

{-| Consider a branchingBit of 2^4 = 16 = 0b00010000.
Then branchingBit*2-1 = 2^5-1  = 31 = 0b00011111,
Now apply bitwise NOT to get the mask 0b11100000.
-}
higherBitMask : Int -> Int
higherBitMask branchingBit =
    Bitwise.complement <| branchingBit*2 - 1


prefixMatches : KeyPrefix -> Int -> Bool
prefixMatches p n =
    n `Bitwise.and` higherBitMask p.branchingBit == p.prefixBits


{- Clear all bits other than the highest in n.
Assumes n to be positive! For implementation notes, see [this](http://aggregate.org/MAGIC/#Most Significant 1 Bit).
-}
highestBitSet : Int -> Int
highestBitSet n =
    let shiftOr n' shift = n' `Bitwise.or` (n' `Bitwise.shiftRightLogical` shift)
        n1 = shiftOr n 1
        n2 = shiftOr n1 2
        n3 = shiftOr n2 4
        n4 = shiftOr n3 8
        n5 = shiftOr n4 16
        -- n6 = shiftOr n5 32 -- 64 bit support?!
        -- n5 has the same msb set as diff. However, all
        -- bits below the msb are also 1! This means we can
        -- do the following to get the msb:
    in n5 `Bitwise.and` Bitwise.complement (n5 `Bitwise.shiftRightLogical` 1)


{- Compute the longest common prefix of two keys.
Returns 0 as branchingBit if equal.

Find the highest bit not set in

    diff = x `xor` y -- 0b011001 `xor` 0b011010 = 0b000011

-}
lcp : Int -> Int -> KeyPrefix
lcp x y =
    let diff = x `Bitwise.xor` y
        branchingBit = highestBitSet diff
        mask = higherBitMask branchingBit
        prefixBits = x `Bitwise.and` mask   -- should equal y & mask
    in
        { prefixBits = prefixBits
        , branchingBit = branchingBit
        }


signBit : Int
signBit =
    highestBitSet -1

isBranchingBitSet : KeyPrefix -> Int -> Bool
isBranchingBitSet p n =
    let n' = n `Bitwise.xor` signBit -- This is a hack that fixes the ordering of keys.
    in n' `Bitwise.and` p.branchingBit /= 0


-- BUILD


{-| Create an empty dictionary. -}
empty : IntDict v
empty = Empty


{-| Create a dictionary with one key-value pair. -}
singleton : Int -> v -> IntDict v
singleton key value =
    leaf key value


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : Int -> v -> IntDict v -> IntDict v
insert key value dict =
    update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : Int -> IntDict v -> IntDict v
remove key dict =
    update key (always Nothing) dict


{-| Update the value of a dictionary for a specific key with a given function. -}
update : Int -> (Maybe v -> Maybe v) -> IntDict v -> IntDict v
update key alter dict =
    let alteredNode v =
            case alter v of                                     -- handle this centrally
                Just v' -> leaf key v'
                Nothing -> empty                                -- The inner constructor will do the rest

        join (k1, d1) (k2, d2) =                                -- precondition: k1 /= k2
            let prefix = lcp k1 k2
            in if isBranchingBitSet prefix k2                   -- if so, d2 will be the right child
               then inner prefix d1 d2
               else inner prefix d2 d1

    in case dict of
        Empty ->
           alteredNode Nothing
        Leaf l ->
            if l.key == key
            then alteredNode (Just l.value)                     -- This updates or removes the leaf with the same key
            else join (key, alteredNode Nothing) (l.key, dict)    -- This potentially inserts a new node
        Inner i ->
            if prefixMatches i.prefix key
            then if isBranchingBitSet i.prefix key
                 then inner i.prefix i.left (update key alter i.right)
                 else inner i.prefix (update key alter i.left) i.right
            else -- we have to join a new leaf with the current diverging Inner node
                join (key, alteredNode Nothing) (i.prefix.prefixBits, dict)


-- QUERY


{-| Check if the dictionary contains no items. -}
isEmpty : IntDict v -> Bool
isEmpty dict =
    case dict of
        Empty -> True
        _ -> False


{-| The number of items in the dictionary. `O(1)`.-}
size : IntDict v -> Int
size dict =
    case dict of
        Empty -> 0
        Leaf _ -> 1
        Inner i -> i.size


{-| Determine if a key is in a dictionary. -}
member : Int -> IntDict v -> Bool
member key dict =
    case get key dict of
        Just _ -> True
        Nothing -> False


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary. -}
get : Int -> IntDict v -> Maybe v
get key dict =
    case dict of
        Empty ->
            Nothing
        Leaf l ->
            if l.key == key
            then Just l.value
            else Nothing
        Inner i ->
            if not (prefixMatches i.prefix key)
            then Nothing
            else if isBranchingBitSet i.prefix key -- continue in left or right branch,
                 then get key i.right              -- depending on whether the branching
                 else get key i.left               -- bit is set in the key


{-| Find the minimum key and value in the dictionary. -}
findMin : IntDict v -> Maybe (Int, v)
findMin dict =
    case dict of
        Empty -> Nothing
        Leaf l -> Just (l.key, l.value)
        Inner i -> findMin i.left


{-| Find the maximum key and value in the dictionary. -}
findMax : IntDict v -> Maybe (Int, v)
findMax dict =
    case dict of
        Empty -> Nothing
        Leaf l -> Just (l.key, l.value)
        Inner i -> findMax i.right



-- TRANSFORM


{-| Keep a key-value pair when it satisfies a predicate. -}
filter : (Int -> v -> Bool) -> IntDict v -> IntDict v
filter predicate dict =
    let add k v d =
            if predicate k v
            then insert k v d
            else d
    in foldl add empty dict


{-| Apply a function to all values in a dictionary. -}
map : (Int -> a -> b) -> IntDict a -> IntDict b
map f dict =
    case dict of
        Empty -> empty
        Leaf l -> leaf l.key (f l.key l.value)
        Inner i -> inner i.prefix (map f i.left) (map f i.right)

{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key. -}
foldl : (Int -> v -> a -> a) -> a -> IntDict v -> a
foldl f acc dict =
    case dict of
        Empty -> acc
        Leaf l ->
            f l.key l.value acc
        Inner i ->
            foldl f (foldl f acc i.left) i.right

{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key. -}
foldr : (Int -> v -> a -> a) -> a -> IntDict v -> a
foldr f acc dict =
    case dict of
        Empty -> acc
        Leaf l ->
            f l.key l.value acc
        Inner i ->
            foldr f (foldr f acc i.right) i.left


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest. -}
partition : (Int -> v -> Bool) -> IntDict v -> (IntDict v, IntDict v)
partition predicate dict =
    let add key value (d1, d2) =
            if predicate key value
                then (insert key value d1, d2)
                else (d1, insert key value d2)
    in
        foldl add (empty, empty) dict


-- COMBINE

type InnerRelation v
    = Same
    | LeftChild { p : InnerType v, l : InnerType v }
    | RightChild { p : InnerType v, r : InnerType v }
    | Siblings { parentPrefix : KeyPrefix, l : InnerType v, r : InnerType v }

{-  Take bits from a or b, depending on the value of the bit in that position in mask.
0 -> a, 1 -> b. Implemented as a & ~mask | b & mask -}
combineBits : Int -> Int -> Int -> Int
combineBits a b mask =
    Bitwise.or
        (Bitwise.and a (Bitwise.complement mask))
        (Bitwise.and b mask)

{-  While merging/uniting 2 inner nodes, we encounter the 4 possible base cases
represented by InnerRelation. This function computes that relation. -}
determineInnerRelation : InnerType v -> InnerType v -> InnerRelation v
determineInnerRelation l r =
    let lp = l.prefix
        rp = r.prefix
        mask = highestBitSet (max lp.branchingBit rp.branchingBit) -- this is the region where we want to force different bits
        modifiedRightPrefix = combineBits rp.prefixBits (Bitwise.complement lp.prefixBits) mask
        prefix = lcp lp.prefixBits modifiedRightPrefix -- l.prefixBits and modifiedRightPrefix are guaranteed to be different
        parentOf p c =
            if isBranchingBitSet p.prefix c.prefix.prefixBits
            then RightChild { p = p, r = c }
            else LeftChild { p = p, l = c }
    in 
        if l.prefix == r.prefix then Same
        else if prefix == l.prefix then l `parentOf` r
        else if prefix == r.prefix then r `parentOf` l
        else
            if isBranchingBitSet prefix rp.prefixBits
            then Siblings { parentPrefix = prefix, l = l, r = r }
            else Siblings { parentPrefix = prefix, l = r, r = l }


{-| `uniteWith merger d1 d2` combines two dictionaries. If there is a collision, `merger`
is called with the conflicting key, the value from `d1` and that from `d2`. -}
uniteWith : (Int -> v -> v -> v) -> IntDict v -> IntDict v -> IntDict v
uniteWith merger d1 d2 =
    let mergeWith key left right =
            case (left, right) of
                (Just l, Just r) -> Just (merger key l r)
                (Just l, _) -> left
                (_, Just r) -> right
                (Nothing, Nothing) ->
                    Debug.crash "IntDict.uniteWith: mergeWith was called with 2 Nothings. This is a bug in the implementation, please file a bug report!"
    in case (d1, d2) of
        (Empty, r) -> r
        (l, Empty) -> l
        (Leaf l, r) -> update l.key (\r' -> mergeWith l.key (Just l.value) r') r
        (l, Leaf r) -> update r.key (\l' -> mergeWith r.key l' (Just r.value)) l
        (Inner i1, Inner i2) -> case determineInnerRelation i1 i2 of
            Same -> -- Merge both left and right sub trees
                inner i1.prefix (uniteWith merger i1.left i2.left) (uniteWith merger i1.right i2.right)
            RightChild {p,r} -> -- Merge with the right sub tree
                if p == i1 -- We need to maintain the left bias
                then inner p.prefix p.left (uniteWith merger p.right (Inner r))
                else inner p.prefix p.left (uniteWith merger (Inner r) p.right)
            LeftChild {p,l} -> -- Merge with the left sub tree
                if p == i1
                then inner p.prefix (uniteWith merger p.left (Inner l)) p.right
                else inner p.prefix (uniteWith merger (Inner l) p.left) p.right
            Siblings {parentPrefix,l,r} -> -- Create a new inner node with l and r as sub trees
                inner parentPrefix (Inner l) (Inner r)


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary. -}
union : IntDict v -> IntDict v -> IntDict v
union =
    uniteWith (\key old new -> old)


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary. -}
intersect : IntDict v -> IntDict v -> IntDict v
intersect d1 d2 =
    case (d1, d2) of
        (Empty, _) -> Empty
        (_, Empty) -> Empty
        (Leaf l, r) -> if member l.key r then d1 else Empty
        (l, Leaf r) -> case get r.key l of
            Just v -> leaf r.key v
            Nothing -> Empty
        (Inner i1, Inner i2) -> case determineInnerRelation i1 i2 of
            Same -> -- Intersect both left and right sub trees
                inner i1.prefix (intersect i1.left i2.left) (intersect i1.right i2.right)
            RightChild {p} ->
                if p == i1
                then intersect i1.right d2
                else intersect d1 i2.right
            LeftChild {p} ->
                if p == i1
                then intersect i1.left d2
                else intersect d1 i2.left
            Siblings _ -> Empty -- We have no common keys


{-| Keep a key-value pair when its key does not appear in the second dictionary.
Preference is given to the first dictionary. -}
diff : IntDict v -> IntDict v -> IntDict v
diff d1 d2 =
    case (d1, d2) of
        (Empty, _) -> Empty
        (l, Empty) -> l
        (Leaf l, r) -> if member l.key r then Empty else d1
        (l, Leaf r) -> remove r.key l
        (Inner i1, Inner i2) -> case determineInnerRelation i1 i2 of
            Same -> -- Diff both left and right sub trees
                inner i1.prefix (diff i1.left i2.left) (diff i1.right i2.right)
            RightChild {p} ->
                if p == i1
                then inner i1.prefix i1.left (diff i1.right d2)
                else diff d1 i2.right
            LeftChild {p} ->
                if p == i1
                then inner i1.prefix (diff i1.left d2) i1.right
                else diff d1 i2.left
            Siblings _ -> d1 -- d1 and d2 contain different keys


-- LISTS


{-| Get all of the keys in a dictionary. -}
keys : IntDict v -> List Int
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary. -}
values : IntDict v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs. -}
toList : IntDict v -> List (Int, v)
toList dict =
    foldr (\key value list -> (key, value) :: list) [] dict


{-| Convert an association list into a dictionary. -}
fromList : List (Int, v) -> IntDict v
fromList pairs =
    let insert' (k, v) dict = insert k v dict
    in List.foldl insert' empty pairs


-- STRING REPRESENTATION


{-| Generates a string representation similar to what `toString`
generates for `Dict`. -}
toString' : IntDict v -> String
toString' dict = "IntDict.fromList " ++ toString (toList dict)
