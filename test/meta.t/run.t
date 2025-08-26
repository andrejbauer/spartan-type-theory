  $ spartan meta.stt
  A is assumed.
  P is assumed.
  f is assumed.
  λ (a : A), let X := A in ((λ (x : X), x) a)
       : Π (a : A), A
  let X := A in (λ (a : A), (λ (x : X), x) a)
       : Π (a : A), A
  let g := (λ (a : A), λ (b : A), b) in (λ (a : A), λ (b : A), g)
       : Π (a : A), Π (b : A), Π (_3 : A), Π (_4 : A), A
  λ (a : A), λ (b : A), let g := (λ (a1 : A), λ (b1 : A), b1) in g
       : Π (a : A), Π (b : A), Π (_5 : A), Π (_6 : A), A
  λ (a : A), λ (b : A), let g := (λ (a1 : A), λ (b1 : A), f a1) in g
       : Π (a : A), Π (b : A), Π (x : A), Π (y : A), P x
