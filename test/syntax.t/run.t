  $ spartan syntax.stt
  Type
       : Type
  Type
       : Type
  A is defined.
  B is assumed.
  λ (A : Type), A
       : Π (A : Type), Type
  λ (A : Type), λ (B : Type), λ (C : Type), A
       : Π (A : Type), Π (B : Type), Π (C : Type), Type
  λ (A : Type), λ (B : Type), λ (C : Type), λ (x : B), λ (y : B), x
       : Π (A : Type), Π (B : Type), Π (C : Type), Π (x : B), Π (y : B), B
  λ (A : Type), A
       : Π (A : Type), Type
  λ (A : Type), λ (B : Type), λ (C : Type), A
       : Π (A : Type), Π (B : Type), Π (C : Type), Type
  λ (x : B), λ (y : B), λ (z : B), y
       : Π (_3 : B), Π (_4 : B), Π (_5 : B), B
  λ (A : Type), λ (B : Type), λ (C : Type), λ (x : B), λ (y : B), x
       : Π (A : Type), Π (B : Type), Π (C : Type), Π (x : B), Π (y : B), B
  id is defined.
  λ (S : Type), λ (c : S), λ (T : Π (_4 : S), Type), λ (u : T c), let x :=
    id S c in u
       : Π (S : Type), Π (c : S), Π (T : Π (_4 : S), Type),
           Π (u : T c), T (id S (id S c))
