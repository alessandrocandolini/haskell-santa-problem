module ArgsSpec where

import Args
    ( parseArgs,
      Args(..),
      Command(Simulate, Doctor),
      GlobalFlags(GlobalFlags),
      SimulationArgs(SimulationArgs),
      Verbosity(NotVerbose, Verbose) )
import Options.Applicative (
  ParserResult (Failure, Success),
 )
import Options.Applicative.Extra (renderFailure)
import Test.Hspec ( describe, it, shouldBe, shouldSatisfy, Spec, Expectation, expectationFailure )
import Test.Hspec.QuickCheck ()
import Test.QuickCheck.Property ()
import Data.List (isPrefixOf)

parseArgsEither :: String -> Either String Args
parseArgsEither = transform . parseArgs . words
 where
  transform (Success a) = Right a
  transform (Failure failure) =
    let (msg, _) = renderFailure failure "program"
     in Left msg
  transform _ = Left "Completion was invoked, cannot continue parsing"

leftShouldStartWith :: (Show a) => Either String a -> String -> Expectation
leftShouldStartWith (Left e) message =
    e `shouldSatisfy` isPrefixOf message
leftShouldStartWith (Right value) _ =
    expectationFailure $ "Expected Left value, but got Right: " ++ show value

spec :: Spec
spec = describe "Args parser" $ do
  it "is able to parse doctor subcommand (verbose flag on)" $
    parseArgsEither "-v doctor" `shouldBe` Right (Args (GlobalFlags Verbose) Doctor)

  it "is able to parse doctor subcommand (verbose flag off)" $
    parseArgsEither "doctor" `shouldBe` Right (Args (GlobalFlags NotVerbose) Doctor)

  it "is able to parse simulate subcommand (short, verbose flag off, default room size)" $
    parseArgsEither "simulate -n 4 -e 18 -r 9" `shouldBe` Right (Args (GlobalFlags NotVerbose) (Simulate (SimulationArgs 4 18 9 3)))

  it "is able to parse simulate subcommand (short, out of order, verbose flag off, non default room size)" $
    parseArgsEither "simulate -n 4 -e 18 -g 4 -r 9" `shouldBe` Right (Args (GlobalFlags NotVerbose) (Simulate (SimulationArgs 4 18 9 4)))

  it "is able to parse simulate subcommand (short, out of order, verbose flag on, non default room size)" $
    parseArgsEither "-v simulate -n 4 -e 18 -g 4 -r 9" `shouldBe` Right (Args (GlobalFlags Verbose) (Simulate (SimulationArgs 4 18 9 4)))

  it "is able to parse simulate subcommand (long, verbose flag off, default room size)" $
    parseArgsEither "simulate --threads 4 --elfs 18 --reinders 9" `shouldBe` Right (Args (GlobalFlags NotVerbose) (Simulate (SimulationArgs 4 18 9 3)))

  it "should fail to parse simulate subcommand with out of range values: greater than (verbose flag on, non default room size)" $
    parseArgsEither "-v simulate -n 100000 -e 18 -g 4 -r 9" `leftShouldStartWith` "option -n: value 100000 out of range! Must be 1 <= 100000 < 100"

  it "should fail to parse simulate subcommand with out of range values: lower than (verbose flag on, non default room size)" $
    parseArgsEither "-v simulate -n 0 -e 18 -g 4 -r 9" `leftShouldStartWith` "option -n: value 0 out of range! Must be 1 <= 0 < 100"
