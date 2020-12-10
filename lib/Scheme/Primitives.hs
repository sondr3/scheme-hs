module Scheme.Primitives
  ( numericPrimitives,
    stringPrimitives,
    listPrimitives,
    symbolPrimitives,
    equivalencePrimitives,
  )
where

import Scheme.Primitives.Equivalence (equivalencePrimitives)
import Scheme.Primitives.Lists (listPrimitives)
import Scheme.Primitives.Numbers (numericPrimitives)
import Scheme.Primitives.Strings (stringPrimitives)
import Scheme.Primitives.Symbols (symbolPrimitives)
