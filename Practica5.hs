type Boolean = Bool

andB :: Boolean -> Boolean -> Boolean
andB True True = True
andB True False = False
andB False True = False
andB False False = False

orB :: Boolean -> Boolean -> Boolean
orB True True = True
orB True False = True
orB False True = True
orB False False = False

notB :: Boolean -> Boolean
notB True = False
notB False = True

nandB :: Boolean -> Boolean -> Boolean
nandB x y = notB (x `andB` y)

norB :: Boolean -> Boolean -> Boolean
norB x y = notB (x `orB` y)

xorB :: Boolean -> Boolean -> Boolean
xorB x y = ((notB x) `andB` y) `orB` (x `andB` (notB y))

xnorB :: Boolean -> Boolean -> Boolean
xnorB x y = (notB x `andB` notB y) `orB` (x `andB` y)