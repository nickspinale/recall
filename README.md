# recall
Haskell memoization library using `-XTypeFamilies` based off of [research](http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf) by Simon Peyton Jones, Oleg Kiselyov, and Chung-chieh Shan. In fact, the `Memo` typeclass and a few instances (`Bool`, `Etiher`, and `List`) come directly from thier examples.

Example usage:

```haskell
import Data.Recall

-- actual function to be memoized
f :: [Maybe Bool] -> Maybe Bool
f n = fmap and . sequenceA

-- function that lazily populates and accesses a lazy trie holding the results of past calls
f' :: [Maybe Bool] -> Maybe Bool
f' = memoize f
```
