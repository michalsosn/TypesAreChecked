{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, PolyKinds, DataKinds, KindSignatures, UndecidableInstances #-}

-- | What this module should offer when finished:
-- - Easily define a collection of roles.
-- - Add a list of allowed roles to any function.
-- - Check at runtime if the user has a particular role, the programmer should be able to write their own check function.
-- - Make the compilation fail if a function requiring a role is invoked before the check function has been called.
module Security where

import Data.Proxy
import GHC.TypeLits

-- | Type level booleans and handy type families
data SBool = SFalse | STrue

type family (a :: SBool) :|: (b :: SBool) :: SBool where
    STrue  :|: b = STrue
    SFalse :|: b = b

type family (a :: SBool) :&: (b :: SBool) :: SBool where
    SFalse :&: b = SFalse
    STrue  :&: b = b

type family IsIn (r :: k) (rs :: [k]) :: SBool where
    IsIn a '[] = SFalse
    IsIn a (a ': tl) = STrue
    IsIn a (b ': tl) = IsIn a tl

-- | Two type level lists of roles
data RolesAllowed a = RList [a] | RAll

data RolesOwned a = ROwned [a]

-- | Two types with phantom type variables - the lists from above
data Secured (r :: RolesAllowed k) a where
    Secured :: a -> Secured r a

-- | Value constructor should probably be hidden and methods that perform real authorization should be supplied instead.
data SecurityContext (r :: RolesOwned k) where
    SecurityContext :: SecurityContext r

-- | Type level function comparing the lists of owned and required roles
type family IsAllowed (ro :: RolesOwned k) (ra :: RolesAllowed k) :: SBool where
    IsAllowed os RAll = STrue
    IsAllowed (ROwned '[]) as = SFalse
    IsAllowed (ROwned (o ': os)) (RList as) = IsIn o as :|: IsAllowed (ROwned os) (RList as)

-- | The desired result of our work. A function taking the context with owned roles and a secured value
-- that stop the compilation when the value should not be accessible in the given context.
runSecured :: (IsAllowed ro ra ~ STrue) => SecurityContext ro -> Secured ra a -> a
runSecured _ctx (Secured a) = a

-- | Role definitions. Type level only, without constructors
data Role = Insert | Select | Update | Delete

login :: IO (SecurityContext (ROwned '[Select, Update]))
login = do
    putStrLn "Logging in!"
    return SecurityContext

securedOperation :: Secured (RList '[Insert, Update]) (IO ())
securedOperation = Secured $ do
    putStrLn "No ordinary user should be able to call me"
    print $ 3 + 4

demo :: IO ()
demo = do
    ctx <- login
    runSecured ctx securedOperation


