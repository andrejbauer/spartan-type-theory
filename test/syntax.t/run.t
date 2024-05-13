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
