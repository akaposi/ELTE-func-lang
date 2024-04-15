-- jovo ora 26 perccel rovidebb (ez mar kumulalt ertek)

-- tipuspolinomok derivalasa (Korpa Peter Zsolt)

-- jovo ora:
-- spring Notes11-bol labelT (Gergo)

data Tree = Leaf | Node Tree Int Tree

label :: Tree -> Int -> (Tree, Int)
label Leaf _ = Leaf
label (Node l _ r) i = Node (label l (i+1)) i (label r (i+2))

-- jovo ora:

-- koinduktiv Church kodolas
-- type Stream a = exists b.((b -> (a,b)),b)
-- exists b . f = forall u. (forall b . f -> u) -> u

-- parametricitas, type () = forall a . a -> a

-- hatekony Haskell forditas (Kovacs Andras): https://github.com/AndrasKovacs/staged/blob/main/newpaper/paper.pdf
