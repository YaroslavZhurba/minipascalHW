module Main where

import qualified Examples
import Language.MiniPascal
import MockIO
import Test.Hspec

main = hspec do
  describe "parser and prettyprinter" do
    it "works" do
      parse_ Examples.unformattedSourceFact \app ->
        prettyPrint app `shouldBe` Examples.formattedSourceFact

    it "is independent" do
      let src1 = Examples.unformattedSourceFact
      parse_ src1 \app -> do
        let src2 = prettyPrint app
        parse_ src2 \app -> do
          let src3 = prettyPrint app
          src2 `shouldBe` src3

  describe "typechecker" do
    it "catch invalid type" do
      parse_ Examples.invalidSourceFact \app ->
        check app
          `shouldBeTypeCheckError` "Binary operator (>) operands should have the same type"

    it "catch function without return" do
      parse_ Examples.emptyFunction \app ->
        check app
          `shouldBeTypeCheckError` "Function f can not be empty"
      parse_ Examples.functionWithoutReturn \app ->
        check app
          `shouldBeTypeCheckError` "Function f should be ended with result"

    it "catch duplication" do
      parse_ Examples.duplicatedVariable \app ->
        check app
          `shouldBeTypeCheckError` "Duplicated variable a"
      parse_ Examples.duplicatedFunction \app ->
        check app
          `shouldBeTypeCheckError` "Duplicated function f"

    it "without typecheck errors" do
      parse_ Examples.unformattedSourceFact \app ->
        case check app of
          Left _ -> expectationFailure "should be valid"
          Right app -> app `shouldBe` Examples.edslFact

  describe "interpreter" do
    it "works on empty example" do
      output <- withMockedIO "" $ eval Examples.empty
      output `shouldBe` ""

    it "works on greet" do
      output <- withMockedIO "username" $ eval Examples.greeting
      output `shouldBe` "Hello, username!\n"

    it "works on factorial repl" do
      let input = unlines ["2", "5", "10", "0"]
      let expectedOutput =
            unlines
              [ "Enter number, 0 to exit:",
                "2! = 2",
                "Enter number, 0 to exit:",
                "5! = 120",
                "Enter number, 0 to exit:",
                "10! = 3628800",
                "Enter number, 0 to exit:",
                "0! = 1",
                "Leaving."
              ]
      output <- withMockedIO input $ eval Examples.edslFact
      output `shouldBe` expectedOutput
  where
    Left e `shouldBeTypeCheckError` e' = e `shouldBe` e'
    Right _ `shouldBeTypeCheckError` _ = expectationFailure "should not typecheck"

    parse_ src cont = do
      case parse src of
        Left error -> expectationFailure error
        Right app -> cont app
