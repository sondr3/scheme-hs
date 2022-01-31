module Scheme.Primitives
  ( numericPrimitives,
    stringPrimitives,
    listPrimitives,
    symbolPrimitives,
    equivalencePrimitives,
    ioPrimitives,
    booleanPrimitives,
  )
where

import Scheme.Primitives.Boolean (booleanPrimitives)
import Scheme.Primitives.Equivalence (equivalencePrimitives)
import Scheme.Primitives.IO (ioPrimitives)
import Scheme.Primitives.Lists (listPrimitives)
import Scheme.Primitives.Numbers (numericPrimitives)
import Scheme.Primitives.Strings (stringPrimitives)
import Scheme.Primitives.Symbols (symbolPrimitives)
