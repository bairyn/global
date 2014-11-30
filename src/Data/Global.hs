{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

-- TODO: The DRY principle is violated in the instance declarations of
-- 'UniqueDeclaration'.  Fix by writing an abstraction.
module Data.Global
    ( UniqueDeclaration(..)
    , UDEmpty(..)
    , UN
    , un
    , UT
    , ut
    , Cnt
    , UV
    , monomorphic
    , QSemQuantity
    , translateExtsToTH'
    , utl
    , ud
    , uninitialized
    ) where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.FairRWLock as FairRWLock
import Control.Concurrent.MSampleVar as MSampleVar
import Control.Concurrent.MSem as MSem
import Control.Concurrent.MSemN as MSemN
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Data.IORef
import Data.Tagged
import Debug.Trace.LocationTH
import qualified Language.Haskell.Exts.Syntax as Exts (Exp)
import Language.Haskell.Exts.QQ
import Fork.Bairyn.Language.Haskell.SyntaxTrees.ExtsToTH
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

-- | Types that can be uniquely declared on the top level.
--
-- Like 'Monad', this type class itself is not "magical".  Its instances,
-- however, may be primitive, at least conceptually, much like 'IO's 'Monad'
-- instance.
--
-- Individual instances may be accompanied with certain caveats.  Each
-- individual instance should include in its documentation what these are.
-- These caveats may affect surrounding code, perhaps in ways detrimental to
-- the program's performance or efficiency; users should thus consider
-- isolating "global" declarations in their own @.Global@ module; this is not
-- necessarily necessary for every instance.  See the documentation of the
-- particular instance to see how the declarations should be declared.
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
-- un \"lives\" =:: ([| 3 |], ut [t| Integer |] :: UT TVar)
-- @
--
-- @lives@ would then refer to the 'TVar' and would initially contain the value @3@.
class UniqueDeclaration u where
    -- | Declare uniquely.
    (=::) ::
        UN u         -- ^ Name of reference
     -> (UV, UT u)   -- ^ Initial value, accompanied with the internal type
                     -- and tagged with the unique constructor so that the
                     -- correct instance can be unambiguously determined.
                     --
                     -- An initial value may not make sense in some
                     -- contexts; implementations of instances may choose
                     -- to ignore this value, as well as the internal type
                     -- and unique constructor.  Implementations should
                     -- document how this parameter is used.
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
instance UniqueDeclaration IORef where
    (Tagged name) =:: (uvq, Tagged typq) = do
        uv  <- uvq
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''IORef) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'newIORef) uv) []
            ]

-- | Declaring unique 'MVar's; see also 'TMVar'; caveats are the same as those of 'IORef's.
--
-- The initial value is used so that the reference refers initially to that value.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration MVar where
    (Tagged name) =:: (uvq, Tagged typq) = do
        uv  <- uvq
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''MVar) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'newMVar) uv) []
            ]

-- | Declaring unique 'MVar's that are initially empty; see also 'TMVar'.
--
-- The initial value is ignored.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration (UDEmpty MVar) where
    (Tagged name) =:: (_, Tagged typq) = do
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''MVar) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ VarE 'newEmptyMVar) []
            ]

-- | Declaring unique 'Chan's that are initially empty; for thread-safe atomic accessing of channels, see 'TChan'; caveats are the same as those of 'MVar's that are initially empty.
--
-- The initial value is ignored.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration (UDEmpty Chan) where
    (Tagged name) =:: (_, Tagged typq) = do
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''Chan) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ VarE 'newChan) []
            ]

-- | Declaring unique 'QSem's.
--
-- The initial value; which is, in this case, determines the initial quantity
-- of the semaphore; is passed to 'newQSem'; the types thus must match.  The
-- internal type is ignored.
--
-- NB: When multiple units of a resource are needed simultaneously, consider using 'QSemN's to avoid deadlocks.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration (Const QSem) where
    (Tagged name) =:: (uvq, _) = do
        uv  <- uvq
        return $
            [ SigD name $ ConT ''QSem
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'newQSem) uv) []
            ]

-- | Declaring unique 'QSemN's.
--
-- The initial value; which is, in this case, determines the initial quantity
-- of the semaphore; is passed to 'newQSemN'; the types thus must match.  The
-- internal type is ignored.
--
-- NB: When multiple units of a resource are needed simultaneously, consider using 'QSemN's to avoid deadlocks.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration (Const QSemN) where
    (Tagged name) =:: (uvq, _) = do
        uv  <- uvq
        return $
            [ SigD name $ ConT ''QSemN
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'newQSemN) uv) []
            ]

-- | Declaring unique 'RWLock's.
--
-- The initial value and the internal type are ignored.
--
-- NB: When multiple units of a resource are needed simultaneously, consider using 'QSemN's to avoid deadlocks.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration (Const RWLock) where
    (Tagged name) =:: (_, _) = do
        return $
            [ SigD name $ ConT ''RWLock
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ VarE 'FairRWLock.new) []
            ]

-- | Declaring unique 'MSampleVar's; caveats are the same as those of 'IORef's.
--
-- The initial value is used so that the reference refers initially to that value.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration MSampleVar where
    (Tagged name) =:: (uvq, Tagged typq) = do
        uv  <- uvq
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''MSampleVar) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'MSampleVar.newSV) uv) []
            ]

-- | Declaring unique 'MSampleVars's that are initially empty; caveats are the same as those of 'MVar's that are initially empty.
--
-- The initial value is ignored.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration (UDEmpty MSampleVar) where
    (Tagged name) =:: (_, Tagged typq) = do
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''MSampleVar) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ VarE 'MSampleVar.newEmptySV) []
            ]

-- | Declaring unique 'MSem's.
--
-- The initial value; which is, in this case, determines the initial quantity
-- of the semaphore; is passed to 'newMSem'; the types thus must match.  The
-- internal type is given to the 'MSem' constructor to construct a semaphore
-- based on an integral type.
--
-- NB: When multiple units of a resource are needed simultaneously, consider using 'MSemN's to avoid deadlocks.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration MSem where
    (Tagged name) =:: (uvq, Tagged typq) = do
        uv  <- uvq
        typ <- typq
        return $
            [ SigD name $ AppT (ConT ''MSem) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'MSem.new) uv) []
            ]

-- | Declaring unique 'MSemN's.
--
-- The initial value; which is, in this case, determines the initial quantity
-- of the semaphore; is passed to 'newMSemN'; the types thus must match.  The
-- internal type is given to the 'MSemN' constructor to construct a semaphore
-- based on an integral type.
--
-- NB: When multiple units of a resource are needed simultaneously, consider using 'MSemN's to avoid deadlocks.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration MSemN where
    (Tagged name) =:: (uvq, Tagged typq) = do
        uv  <- uvq
        typ <- typq
        return $
            [ SigD name $ AppT (ConT ''MSemN) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'MSemN.new) uv) []
            ]

-- | Declaring unique 'TVar's; caveats are the same as those of 'IORef's.
--
-- The initial value is used so that the reference refers initially to that value.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration TVar where
    (Tagged name) =:: (uvq, Tagged typq) = do
        uv  <- uvq
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''TVar) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'newTVarIO) uv) []
            ]

-- | Declaring unique 'TMVar's; caveats are the same as those of 'IORef's.
--
-- The initial value is used so that the reference refers initially to that value.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration TMVar where
    (Tagged name) =:: (uvq, Tagged typq) = do
        uv  <- uvq
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''TMVar) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ AppE (VarE 'newTMVarIO) uv) []
            ]

-- | Declaring unique 'TMVar's that are initially empty; caveats are the same as those of 'MVar's that are initially empty.
--
-- The initial value is ignored.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration (UDEmpty TMVar) where
    (Tagged name) =:: (_, Tagged typq) = do
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''TMVar) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ VarE 'newEmptyTMVarIO) []
            ]

-- | Declaring unique 'TChan's that are initially empty; caveats are the same as those of 'MVar's that are initially empty.
--
-- The initial value is ignored.
--
-- These preconditions apply to GHC 7.0.4 and base-0.4.3.1 and likely similar versions and implementations as well.
--
-- In its low-level implementation, this instance uses 'unsafePerformIO';
-- thus, the same caveats apply to this instance, particularly those
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
instance UniqueDeclaration (UDEmpty TChan) where
    (Tagged name) =:: (_, Tagged typq) = do
        typ <- typq
        $(assert [| monomorphic typ |]) . return $
            [ SigD name $ AppT (ConT ''TChan) typ
            , PragmaD (InlineP name (InlineSpec False False Nothing))
            , ValD (VarP name) (NormalB $ AppE (VarE 'unsafeUDeclInternal) $ VarE 'newTChanIO) []
            ]

-- | Identity type wrapper that indicates that the unique declaration should be "empty" by default.
newtype UDEmpty u a = UDEmpty (u a)

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
type UnsafeUDeclInternal a = IO a -> a

-- | The type of values that supply an initial quantity for quantity semaphores.
type QSemQuantity = Int

-- | Internal means of constructing unique values, used only by this library.
--
-- 'unsafeUDeclInternal' should never be used directly, outside this
-- library.
unsafeUDeclInternal :: UnsafeUDeclInternal a
{-# NOINLINE unsafeUDeclInternal #-}
unsafeUDeclInternal = unsafePerformIO

-- | Translate an "Exts" AST to a Template Haskell AST, failing when the translation result is not a Template Haskell AST.
--
-- This is defined in terms of 'Language.Haskell.Exts.QQ.translateExtsToTH'
translateExtsToTH' :: Exts.Exp -> Exp
translateExtsToTH' = either (const $ $failure $ printf "translating Exts AST to Template Haskell AST resulted in Exts AST") id . translateExtsToTH

-- | Apply translateExtsToTH' and lift the result into the 'Q' monad.
--
-- This is often used with 'ud' to refer to variables whose names are not required to be in scope when the quotation is expanded, in a very roundabout way.
--
-- "utl" can be thought of as a mnemonic for "unique", "translate" and "lift"; and will be updated appropriately to reflect changes to 'UV'.
--
-- For example, to enable self-referential recursion by referring to
-- variables whose names are not yet in scope, an expression quotation
-- @[| … |]@ can usually be written as @utl [ud| … |]@.
utl :: Exts.Exp -> UV
utl = return . translateExtsToTH'

-- | Alias to the 'QuasiQuoter' 'hs', which does not require names to be in scope when the quotation is expanded, which enables self-referential recursion.
ud :: QuasiQuoter
ud = hs

-- | An alternative to providing an initial value.
--
-- Warning: attempting to read uninitialized references can cause the program to crash.
uninitialized :: Q Exp
uninitialized = [| $failure "uninitialized" |]
