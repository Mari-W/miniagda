data E : Set where

data T : Set where
  tt : T

data Eq (A : Set) (x : A) : A → Set where
  refl : Eq A x x

data N : Set where
  zero : N 
  suc  : N → N

data Leq : N → N → Set where
  lz : (n : N) → Leq zero n
  ls : (m : N) (n : N) → Leq m n → Leq (suc m) (suc n)

data Vec (A : Set) : N → Set where
  nil  : Vec A zero
  cons : (n : N) → Vec A n → A → Vec A (suc n)

foo : Vec T zero 
foo = nil {A = T}

data Sum (A : Set) (B : Set) : Set where
  left  : A → Sum A B
  right : B → Sum A B 

data Pair (A : Set) (B : Set) : Set where
  pair : A → B → Pair A B

-- proj1 : (A : Set) → (B : Set) → Pair A B → Pair A
-- proj1 _ _ (pair a b) = a
-- 
-- proj2 : (A : Set) → (B : Set) → Pair A B → A
-- proj2 _ _ (pair a b) = a