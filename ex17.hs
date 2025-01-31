-- 1. give three more examples of monoids
--      the carrier sets must be different from the examples and from each other
--      be sure to reason about monoid laws

-- carrier: The set of functions x->x
-- operator: Function composition
-- Identity Element: The identity function (id)
-- Associativity: (f ∘ g) ∘ h = f ∘ (g ∘ h)

-- carrier: The set of Set
-- operator: Union
-- Identity Element: Emtpty set
-- Associativity: (A ∪ B) ∪ C = A ∪ (B ∪ C)

-- Carrier: The set of orderings {LT,EQ,GT}
-- Operator: Combining two orderings where
--      LT⊕LT=LT (LT⊕LT = LT)
--      LT⊕EQ=LT
--      EQ⊕EQ=EQ (EQ⊕R = R⊕EQ = R)
--      EQ⊕LT=LT
--      EQ⊕GT=GT
--      GT⊕EQ=GT (GT⊕R = R⊕GT = GT)
--      GT⊕LT=GT
--      GT⊕GT=GT
-- Identity Element: EQ
-- Associativity: (R⊕S)⊕T=R⊕(S⊕T)

-- 2. using newtype, declare monoid instances for Bool, where operator: (&&) operator: (||)
newtype AndBool = AndBool {getAnd :: Bool} deriving (Show)

instance Semigroup AndBool where
    (AndBool a) <> (AndBool b) = AndBool (a && b)

instance Monoid AndBool where
    mempty = AndBool True

newtype OrBool = OrBool {getOr :: Bool} deriving (Show)

instance Semigroup OrBool where
    (OrBool a) <> (OrBool b) = OrBool (a || b)

instance Monoid OrBool where
    mempty = OrBool False

-- 3. define function maybeBind ::
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _  = Nothing
maybeBind (Just x) f = f x

-- 4. define function listBind ::
listBind :: [a] -> (a -> [b]) -> [b]
listBind [] _ = []
listBind (x:xs) f = f x ++ listBind xs f

-- 5. define function eitherBind ::
eitherBind :: Either r a -> (a -> Either r b) -> Either r b
eitherBind (Left y) _ = Left y
eitherBind (Right x) f = f x

-- 6. define function arrowBind ::
arrowBind :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
arrowBind f g r = g (f r) r

-- 7. define function pairBind ::
pairBind :: Monoid r => (r, a) -> (a -> (r, b)) -> (r, b)
pairBind (r1, x) f = case f x of
    (r2, y) -> (r1 <> r2, y)
-- what do we need to know about r?
--      r is a Monoid, which means:
--          r has an identity element (mempty)
--          r has an associative binary operation (<>)
--          It must satisfy the monoid laws
