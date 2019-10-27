{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Generators where

import Data.Map.Strict
import Test.QuickCheck

import AST.V0_16
import qualified AST.Declaration
import AST.Expression (AnnotatedExpression(AE))
import qualified AST.Expression
import qualified AST.Module
import qualified AST.Pattern
import qualified AST.Variable
import Data.Fix
import qualified Reporting.Annotation
import qualified Reporting.Region


capitalLetter :: Gen Char
capitalLetter =
    elements "ABCDEFGHIJKLMNOPQRSTUVWXYZÀΩЉԱႠḀℇ𐐅𝐴𝞨"
    -- Should also be capital, but Data.Char.isUpper does not agree: Ꭳ


capitalAscii :: Gen Char
capitalAscii =
    elements "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


lowerLetter :: Gen Char
lowerLetter =
    elements "abcdefghijklmnopqrstuvwxyz"


number :: Gen Char
number =
    elements "0123456789"


capIdentifier :: Gen UppercaseIdentifier
capIdentifier =
    do
        first <- capitalLetter
        rest <- listOf $ oneof [ capitalLetter, lowerLetter, number ]
        return $ UppercaseIdentifier $ first:rest


lowerIdentifier :: Gen LowercaseIdentifier
lowerIdentifier =
    do
        first <- lowerLetter
        rest <- listOf $ oneof [ capitalLetter, lowerLetter, number ]
        return $ LowercaseIdentifier $ first:rest


nowhere :: Reporting.Region.Position
nowhere =
    Reporting.Region.Position 0 0


located :: a -> Reporting.Annotation.Located a
located =
    Reporting.Annotation.at nowhere nowhere


instance Arbitrary Reporting.Region.Region where
    arbitrary =
        return $ Reporting.Region.Region nowhere nowhere


instance (Arbitrary a) => Arbitrary (Reporting.Annotation.Located a) where
    arbitrary =
        do
            ann <- arbitrary
            a <- arbitrary
            return $ Reporting.Annotation.A ann a


commented :: Gen a -> Gen (C2 before after a)
commented inner =
    C ([], []) <$> inner


instance Arbitrary AST.Variable.Value where
    arbitrary =
        do
            name <- capIdentifier
            return $ AST.Variable.Union (C [] name) AST.Variable.ClosedListing


listing :: Gen (AST.Variable.Listing a)
listing =
    return $ AST.Variable.OpenListing (C ([], []) ())


instance Arbitrary AST.Module.Module where
    arbitrary =
        do
            name <- listOf1 $ capIdentifier
            listing <- listing
            moduleType <- fmap (\x -> if x then AST.Module.Port [] else AST.Module.Normal) arbitrary
            return $ AST.Module.Module
                []
                (Just $ AST.Module.Header
                  moduleType
                  (C ([], []) name)
                  Nothing
                  (Just $ C ([], []) listing)
                )
                (located Nothing)
                (C [] empty)
                [ AST.Declaration.Entry $ located $ AST.Declaration.Definition (located $ AST.Pattern.Anything) [] [] (Fix $ AE $ located $ AST.Expression.TupleFunction 2)]
