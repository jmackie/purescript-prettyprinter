module Test.Code (test) where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (intercalate)
import Data.NonEmpty ((:|))
import Data.String as String
import Effect (Effect)
import Test.Common (runTest, green)
import Text.Pretty ((<+>))
import Text.Pretty as Pretty


test :: Effect Unit
test = do
    let sig = Sig "addShow" (Abs (Abs IntType IntType) StringType)
    testSig 31 sig
    testSig 25 sig
    testSig 12 sig


testSig :: Int -> Sig -> Effect Unit
testSig width sig = runTest width (ansiFancy <$> prettySig sig)


ansiFancy :: Fancy -> String
ansiFancy (Plain s) = s
ansiFancy (Green s) = green s


-- | This shows how you can use types other than `String` with `Doc`.
data Fancy = Green String | Plain String


instance semigroupFancy :: Semigroup Fancy where
    append (Green s) (Green s') = Green (s <> s')
    append (Green s) (Plain s') = Green (s <> s')
    append (Plain s) (Green s') = Green (s <> s')
    append (Plain s) (Plain s') = Plain (s <> s')


instance monoidFancy :: Monoid Fancy where
    mempty = Plain ""


instance renderableFancy :: Pretty.Renderable Fancy where
    space   = Plain " "
    newline = Plain "\n"
    width   = unFancy >>> String.length


unFancy :: Fancy -> String
unFancy (Green s) = s
unFancy (Plain s) = s


data Sig = Sig String Type -- foo :: Int


prettySig :: Sig -> Pretty.Doc Fancy
prettySig (Sig name t) =
    Pretty.text (Green name) <>
        (Pretty.group $ map Plain (Pretty.nest 2 (Pretty.line <> Pretty.text "::" <+> prettyType t)))


data Type
    = Abs Type Type
    | IntType
    | StringType


prettyType :: Type -> Pretty.Doc String
prettyType IntType    = Pretty.text "Int"
prettyType StringType = Pretty.text "String"
prettyType abs@(Abs _ _)  =
    case NonEmptyArray.toNonEmpty (unAbs abs) of
         t :| ts ->
            Pretty.group $
                intercalate Pretty.line $
                    Array.cons
                        (prettyType t)
                        (map (prettyType >>> (Pretty.text "->" <+> _)) ts)


unAbs :: Type -> NonEmptyArray Type
unAbs (Abs x y) = unAbs x <> unAbs y
unAbs t         = NonEmptyArray.singleton t
