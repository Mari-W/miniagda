data T : Set where
  tt : T
  
data Pair (A : Set) (B : Set) : Set where
  pair : A → (λ (A : Set) → A) B → Pair A B

proj1 : (A : Set) → ((λ (P : Set → Set → Set) → ((B : Set) → P A B → A)) Pair)
proj1 _ _ (pair a b) = a

proj2 : (A : Set) → (B : Set) → Pair A B → B
proj2 _ _ (pair a b) = b

