data Pair (A : Set) (B : Set) : Set where
  pair : A → B → Pair A B

proj1 : (A : Set) → (B : Set) → Pair A B → A
proj1 _ _ (pair a b) = a

proj2 : (A : Set) → (B : Set) → Pair A B → B
proj2 _ _ (pair a b) = b

proj2' : (A : Set) → (B : Set) → Pair A B → B
proj2' a b (pair x x₁) = b
