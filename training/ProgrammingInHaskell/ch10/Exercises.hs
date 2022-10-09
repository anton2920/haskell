import Prelude hiding (return, (>>=), (+++), eval)
import Main2 hiding (expr, factor, eval)
import System.IO

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ(int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ(add m n)

mult :: Nat -> Nat -> Nat
mult (Succ Zero) n = n
mult (Succ m) n = mult m (add n n)

---

data Tree = Leaf Int | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs n (Leaf v) = compare n v == EQ
occurs n (Node lt v rt) = case compare n v of
                            LT -> occurs n lt
                            EQ -> True
                            GT -> occurs n rt

---

data BTree = BLeaf Int | BNode BTree BTree

tlength :: BTree -> Int
tlength (BLeaf _) = 1
tlength (BNode lt rt) = tlength lt + tlength rt

balanced :: BTree -> Bool
balanced (BLeaf _) = True
balanced (BNode lt rt) = abs ldiff <= 1
                            where ldiff = tlength lt - tlength rt

balance :: [Int] -> BTree
balance (x:[]) = BLeaf x
balance xs = BNode (balance (take ((length xs) `div` 2) xs)) (balance (drop ((length xs) `div` 2) xs))

---

data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imply Prop Prop
            | Equal Prop Prop

instance Show Prop where
    show (Const b) = show b
    show (Var x) = show x
    show (Not p) = "!" ++ show p
    show (And p q) = show p ++ " && " ++ show q
    show (Or p q) = show p ++ " || " ++ show q
    show (Imply p q) = show p ++ " => " ++ show q
    show (Equal p q) = show p ++ " == " ++ show q

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = (eval s p) && (eval s q)
eval s (Or p q) = (eval s p) || (eval s q)
eval s (Imply p q) = (eval s p) <= (eval s q)
eval s (Equal p q) = (eval s p) == (eval s q)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equal p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:filter (/= x) xs

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

---

{-
Grammar:
    expr   ::= andtok {"||" andtok}
    andtok ::= eqtok  {"&&" eqtok}
    eqtok  ::= imltok {"==" imltok}
    imltok ::= nottok {"=>" nottok}
    nottok ::= "!" factor | factor
    factor ::= "(" expr ")" | var | bool
    var    ::= ['a'..'z']
    bool   ::= "True" | "False"
-}

expr :: Parser Prop
expr = andtok >>= \p -> (symbol "||" >>= \_ -> andtok >>= \q -> return (Or p q)) +++ return p

andtok :: Parser Prop
andtok = eqtok >>= \p -> (symbol "&&" >>= \_ -> eqtok >>= \q -> return (And p q)) +++ return p

eqtok :: Parser Prop
eqtok = imltok >>= \p -> (symbol "==" >>= \_ -> imltok >>= \q -> return (Equal p q)) +++ return p

imltok :: Parser Prop
imltok = nottok >>= \p -> (symbol "=>" >>= \_ -> nottok >>= \q -> return (Imply p q)) +++ return p

nottok :: Parser Prop
nottok = (symbol "!" >>= \_ -> factor >>= \t -> return (Not t)) +++ factor

factor :: Parser Prop
factor = (symbol "(" >>= \_ -> expr >>= \e -> symbol ")" >>= \_ -> return e) +++ var +++ bool

var :: Parser Prop
var = token lower >>= \v -> return (Var v)

bool :: Parser Prop
bool = (symbol "True" >>= \_ -> return (Const True)) +++ (symbol "False" >>= \_ -> return (Const False))

evalExpr :: String -> Prop
evalExpr xs = case parse expr xs of
                    [(n, [])] -> n
                    [(_, out)] -> error ("unused input " ++ out)
                    [] -> error "invalid input"

isExprTaut :: String -> Bool
isExprTaut = isTaut . evalExpr

---

data Expr = Val Int | Mult Expr Expr | Add Expr Expr

type Cont = [Op]
data Op = EVALA Expr | EVALM Expr | MULT Int | ADD Int

eeval :: Expr -> Cont -> Int
eeval (Val n) c = eexec c n
eeval (Mult x y) c = eeval x (EVALM y:c)
eeval (Add x y) c = eeval x (EVALA y:c)

eexec :: Cont -> Int -> Int
eexec [] n = n
eexec (EVALA y:c) n = eeval y (ADD n:c)
eexec (EVALM y:c) n = eeval y (MULT n:c)
eexec (ADD n:c) m = eexec c (n + m)
eexec (MULT n:c) m = eexec c (n * m)

value :: Expr -> Int
value e = eeval e []

---
mreturn :: a -> Maybe a
mreturn v = Just v

(>>==) :: Maybe a -> (a -> Maybe b) -> Maybe b
p >>== f  = case p of
                Nothing -> Nothing
                Just p  -> f p

lreturn :: a -> [a]
lreturn v = [v]

-- This might be wrong af
(>>===) :: [a] -> (a -> [b]) -> [b]
p >>=== f = case p of
                [] -> []
                (x:xs) -> f x ++ xs >>=== f
