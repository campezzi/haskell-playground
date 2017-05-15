# haskell-playground

Random Haskell experiments, exercises and brain farts. Don't judge!

### Laws

- Associativity: `(x + y) + z = x + (y + z)`
- Identity: `id x = x`
- Composition: `(g . f) $ x = (g (f x))`
- Homomorphism: `pure f <*> x = pure (f x)`
- Interchange: `u <*> pure x = pure ($ x) <*> u`

### Categories & Laws
- Semigroup: associativity
- Monoid: associativity, identity
- Functor: identity, composition
- Applicative: identity, composition, homomorphism, interchange
