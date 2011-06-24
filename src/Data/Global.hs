{-# LANGUAGE TemplateHaskell #-}

module Data.Global
    ( UniqueDeclaration(..)
    , UN
    , un
    , UT
    , ut
    , Cnt
    , UV
    , monomorphic
    , unsafeUDeclInternal
    ) where

import Data.IORef
import Data.Tagged
import Debug.Trace.LocationTH
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

-- | Types that can be uniquely declared on the top level.
--
-- For each declaration, 'declare' must provide a definition and not a
-- definition for any other name, as well as not defining a type signature.
-- Additionally, .  Neither is enforced by the type system.
--
-- Like 'Monad', this type class itself is not magical.  Its instances, however, may be primitive, at least conceptually, much like IO's Monad instance.
--
-- Individual instances may require certain compiler constructs* to be
-- invoked.  Each individual instances should include in its documentation what these are.  These ops may affect surrounding code, perhaps in ways detrimental* to the program's performance or eficiency; users should thus consider isolating "global" declarations in their own ".Global" module; this is not necessarily necessary for every instance.  (Again**, ) See the documentation of the particular to see how the declarations should be used.
--
-- The type should be monomorphic, or concrete
-- enough, to be type safe, so that the references
-- cannot be treated as multiple concreet types
-- (writing @[Integer]@ to a reference that has the
-- type @IORef [a]@ and subsequently reading @[Char]@
-- can cause the program to core dump).  Ensuring
-- this safety is the responsibility of the
-- implementer of the instances of this type class;
-- other users of this library who do not extend this
-- class's functionality generally do not need to be
-- concerned whether the program will run correctly
-- at run-time, since the mistakes, which can violate
-- type safety, will be caught at compile-time and
-- the code will not build (this is, however, not
-- intrinsically guaranteed, much like the monad
-- laws: they are expected to be followed).  It is
-- worth reiterating that instances of this class
-- need to be sure to not allow code with such
-- erroneous types to compile.  For more information
-- about type safety, see the documentation of
-- 'unsafePerformIO'.
--
-- Example:
--
-- @
-- un "lives" =:: ([| 3 |], ut [t| Integer |] :: UT TVar)
-- @
--
-- @lives@ would then refer to the 'TVar' and would initially contain the value @3@.
class UniqueDeclaration u where
    -- | Declare uniquely.
    --
    -- A type signature should not be provided by this function.  This
    -- function is thus normally not used to directly declare a unique
    -- reference; see '=::'.
    (=::) ::
        UN u         -- ^ Name of reference
     -> (UV, UT u)   -- ^ Initial value, tagged with the unique constructor so that the correct instance can be unambiguously determined.
                     --
                     -- An initial value may not make sense in some
                     -- contexts; implementations of instances may choose
                     -- to ignore this value.  Implementations should
                     -- document how this value is used.
     -> Q [Dec]      -- ^ Top-level declarations for the unique declaration.
                     --
                     -- At least a definition for the name and a type signature should be provided.

-- | Declaring unique 'IORef's; for thread-safe handling of mutable data, see 'TVar'.
--
-- The initial value is used so that the reference refers initially to that value.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same preconditions apply to this instance, particularly those
-- regarding top-level declarations (referential transparency cannot be
-- violated here).  As of base-4.3.1.0, these conditions, that the user needs
-- to be aware of, are the following:
--  * Compile the declarations with the compiler flag -fno-cse.  This
--    prevents multiple references from being substituted to refer to the
--    same data.  This flag thus does not affect the semantics of the
--    program, but may potentially adversely affect its performance; thus,
--    isolating in a @.Global@ module may be advisable in some cases.  This
--    condition is not strictly necessary when only one declaration is made
--    in a module, since the compiler cannot substitute multiple references
--    to refer to same data.
--
--    If your code behaves differently when optimizations are enabled,
--    ensure that this flag is indeed being used when the declarations are being compiled.
--    Setting or passing this flag is NOT handled automatically by this
--    implementation; it is the responsibility of users of this
--    implementation to ensure that such appropriate behaviour is set when
--    necessary.
--
--    This can be accomplished by placing the line @{-# OPTIONS_GHC -fno-cse #-}@ in
--    the file in which the declarations are declared, before the "module"
--    line.
--  * 'unsafeUDeclInternal' needs to be in scope where unique declarations are declared.  It normally is when '=::' is in scope, as long as it is not hidden.
instance UniqueDeclaration IORef where
    (Tagged name) =:: (uvq, Tagged typq) = do
        uv  <- uvq
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''IORef) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'newIORef) uv) []
            ]

-- | Tagged name type.
type UN u = Tagged (Cnt u) Name
-- | Construct a name for a unique declaration from a string.
un :: (UniqueDeclaration u) => String -> UN u
un = Tagged . mkName

-- | Tagged unique declaration type.
type UT c = Tagged (Cnt c) TypeQ
-- | Tagged unique declaration type constructor.
ut :: (UniqueDeclaration c) => TypeQ -> UT c
ut = Tagged

-- | Transform a container with kind @* -> *@ into a concrete type @*@ by
-- applying the type '()' to the constructor.
--
-- Intended to be used for tagging types for unique declarations.
type Cnt c = c ()

-- | An expression for a value contained in a unique declaration.
type UV = ExpQ

-- | Determine whether a type is polymorphic.
monomorphic :: Type -> Bool
monomorphic (ConT   _) = True
monomorphic (TupleT _) = True
monomorphic (ArrowT)   = True
monomorphic (ListT)    = True
monomorphic (AppT _ _) = True
monomorphic (SigT _ _) = True
monomorphic t          = $failure $ printf "type is too polymorphic: `%s'" (pprint t)

-- | Type of 'unsafeUDeclInternal', used only by this library.
type UnsafeUDeclInternal u a = IO (u a) -> (u a)
-- | Internal means of constructing unique values, used only by this library.
--
-- 'unsafeUDeclInternal' should never be used directly, outside this
-- library.  It may, however, be required by some instances of
-- 'UniqueDeclaration' to be in scope where a unique declaration is
-- declared; when this is the case, the documentation of the instances should
-- indicate this.
unsafeUDeclInternal :: (UniqueDeclaration u) => UnsafeUDeclInternal u a
{-# NOINLINE unsafeUDeclInternal #-}
unsafeUDeclInternal = unsafePerformIO
