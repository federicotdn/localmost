module LocalmostSpec where

import Config (Config (..), ConfigRule (..), defaultConfig)
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Localmost
import Test.Hspec
import TestUtils (right, sh)
import Types (Policy (..))

-- Test Rule
tr :: ConfigRule
tr = ConfigRule {rRule = "x", rPipe = Nothing, rRedirect = Nothing, rUnless = Nothing}

testRt :: [(Text, Policy)] -> Runtime
testRt rules =
  right $
    parseConfig
      Config
        { cAllow = Just [tr {rRule = r} | (r, Allow) <- rules],
          cDeny = Just [tr {rRule = r} | (r, Deny) <- rules],
          cPath = Nothing
        }

spec :: Spec
spec = do
  describe "parseConfig" $ do
    it "correctly parses the default/empty config" $ do
      isRight (parseConfig defaultConfig) `shouldBe` True

    it "rejects rules with multiple commands" $ do
      let rules = ["foo | bar", "foo && bar", "foo; bar"]
      let parse r = parseConfig defaultConfig {cAllow = Just [tr {rRule = r}]}
      mapM_ (\r -> parse r `shouldSatisfy` isLeft) rules

    it "rejects rules with redirects" $ do
      let rules = ["foo > bar", "foo 2>&1", "foo < bar"]
      let parse r = parseConfig defaultConfig {cAllow = Just [tr {rRule = r}]}
      mapM_ (\r -> parse r `shouldSatisfy` isLeft) rules

    it "rejects rules with no commands" $ do
      let cfg = defaultConfig {cAllow = Just [tr {rRule = ""}]}
      parseConfig cfg `shouldSatisfy` isLeft

    it "rejects except clauses with multiple commands" $ do
      let excepts = ["foo | bar", "foo && bar", "foo; bar"]
      let parse e = parseConfig defaultConfig {cAllow = Just [tr {rUnless = Just [e]}]}
      mapM_ (\e -> parse e `shouldSatisfy` isLeft) excepts

    it "rejects except clauses with redirects" $ do
      let excepts = ["foo > bar", "foo 2>&1", "foo < bar"]
      let parse e = parseConfig defaultConfig {cAllow = Just [tr {rUnless = Just [e]}]}
      mapM_ (\e -> parse e `shouldSatisfy` isLeft) excepts

    it "rejects except clauses with no commands" $ do
      let cfg = defaultConfig {cAllow = Just [tr {rUnless = Just [""]}]}
      parseConfig cfg `shouldSatisfy` isLeft

    it "rejects rules with quantifier @anything" $ do
      let rules = ["foo @anything+", "foo @anything?", "foo @anything*"]
      let parse r = parseConfig defaultConfig {cAllow = Just [tr {rRule = r}]}
      mapM_ (\r -> parse r `shouldSatisfy` isLeft) rules

  describe "buildCommand" $ do
    it "detects pipe positions correctly" $ do
      case sCommands (sh "foo") of
        [c1] -> do
          cmdPipeIn c1 `shouldBe` False
          cmdPipeOut c1 `shouldBe` False
        cs -> expectationFailure $ show cs
      case sCommands (sh "foo | bar && baz") of
        [c1, c2, _] -> do
          cmdPipeIn c1 `shouldBe` False
          cmdPipeOut c1 `shouldBe` True
          cmdPipeIn c2 `shouldBe` True
          cmdPipeOut c2 `shouldBe` False
        cs -> expectationFailure $ show cs
      case sCommands (sh "a > foo | b | c") of
        [c1, c2, c3] -> do
          cmdPipeIn c1 `shouldBe` False
          cmdPipeOut c1 `shouldBe` True
          cmdPipeIn c2 `shouldBe` True
          cmdPipeOut c2 `shouldBe` True
          cmdPipeIn c3 `shouldBe` True
          cmdPipeOut c3 `shouldBe` False
        cs -> expectationFailure $ show cs

  describe "computePolicy" $ do
    it "computes policies correctly (no rules)" $ do
      let rt = testRt []
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "source xyz") `shouldBe` Ask
      computePolicy rt (sh "set -e") `shouldBe` Ask
      computePolicy rt (sh "echo") `shouldNotBe` Allow
      computePolicy rt (sh "echo") `shouldNotBe` Deny

    it "computes policies correctly (command)" $ do
      let rt = testRt [("echo", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Allow
      computePolicy rt (sh "'echo'") `shouldBe` Allow
      computePolicy rt (sh "ECHO") `shouldBe` Ask
      computePolicy rt (sh "echo foo") `shouldBe` Ask
      computePolicy rt (sh "ls") `shouldBe` Ask

    it "computes policies correctly (command, deny)" $ do
      let rt = testRt [("echo", Allow), ("evil", Deny)]
      computePolicy rt (sh "echo") `shouldBe` Allow
      computePolicy rt (sh "evil") `shouldBe` Deny
      computePolicy rt (sh "echo | evil") `shouldBe` Deny
      computePolicy rt (sh "echo && evil") `shouldBe` Deny

    it "computes policies correctly (repeat match)" $ do
      let rt = testRt [("echo", Allow), ("echo", Deny)]
      computePolicy rt (sh "echo") `shouldBe` Deny
      let rt' = testRt [("echo", Deny), ("echo", Ask)]
      computePolicy rt' (sh "echo") `shouldBe` Deny

    it "computes policies correctly (and)" $ do
      let rt = testRt [("foo", Allow), ("bar", Allow)]
      computePolicy rt (sh "foo && bar") `shouldBe` Allow
      computePolicy rt (sh "foo && baz") `shouldBe` Ask

    it "computes policies correctly (or)" $ do
      let rt = testRt [("foo", Allow), ("bar", Allow)]
      computePolicy rt (sh "foo || bar") `shouldBe` Allow
      computePolicy rt (sh "foo || baz") `shouldBe` Ask

    it "computes policies correctly (arg metavar)" $ do
      let rt = testRt [("echo @arg", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo foo") `shouldBe` Allow
      computePolicy rt (sh "echo -foo") `shouldBe` Allow
      computePolicy rt (sh "echo @") `shouldBe` Allow
      computePolicy rt (sh "echo \"test\"") `shouldBe` Allow
      computePolicy rt (sh "echo \"*.txt\"") `shouldBe` Allow
      computePolicy rt (sh "echo $foo") `shouldBe` Ask
      computePolicy rt (sh "echo ${foo}") `shouldBe` Ask
      computePolicy rt (sh "echo $(foo)") `shouldBe` Ask
      computePolicy rt (sh "echo `foo`") `shouldBe` Ask
      computePolicy rt (sh "echo {a,b,c}") `shouldBe` Ask
      computePolicy rt (sh "echo *.a") `shouldBe` Ask
      computePolicy rt (sh "echo \"${arr[@]}\"") `shouldBe` Ask
      computePolicy rt (sh "echo !(*.a)") `shouldBe` Ask

    it "computes policies correctly (match arbitrary token)" $ do
      let rt = testRt [("echo $foo", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo $foo") `shouldBe` Allow
      computePolicy rt (sh "echo '$foo'") `shouldBe` Ask
      computePolicy rt (sh "echo \"$foo\"") `shouldBe` Ask
      let rt' = testRt [("echo ${foo:-bar}", Allow)]
      computePolicy rt' (sh "echo $foo") `shouldBe` Ask
      computePolicy rt' (sh "echo bar") `shouldBe` Ask
      computePolicy rt' (sh "echo ${foo}") `shouldBe` Ask
      computePolicy rt' (sh "echo ${foo:-b}") `shouldBe` Ask
      computePolicy rt' (sh "echo ${foo:-bar}") `shouldBe` Allow

    it "computes policies correctly (int metavar)" $ do
      let rt = testRt [("echo @int", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo foo") `shouldBe` Ask
      computePolicy rt (sh "echo 0") `shouldBe` Allow
      computePolicy rt (sh "echo 42") `shouldBe` Allow
      computePolicy rt (sh "echo -1") `shouldBe` Allow
      computePolicy rt (sh "echo 3.14") `shouldBe` Ask
      computePolicy rt (sh "echo 12abc") `shouldBe` Ask

    it "computes policies correctly (at metavar)" $ do
      let rt = testRt [("echo @@", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo foo") `shouldBe` Ask
      computePolicy rt (sh "echo @") `shouldBe` Allow

    it "computes policies correctly (arg metavar with quant +)" $ do
      let rt = testRt [("echo @arg+", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo foo") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar baz") `shouldBe` Allow

    it "computes policies correctly (arg metavar with quant *)" $ do
      let rt = testRt [("echo @arg*", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Allow
      computePolicy rt (sh "echo foo") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar") `shouldBe` Allow
      let rt' = testRt [("echo @arg* bar", Allow)]
      computePolicy rt' (sh "echo") `shouldBe` Ask
      computePolicy rt' (sh "echo foo") `shouldBe` Ask
      computePolicy rt' (sh "echo bar") `shouldBe` Allow
      computePolicy rt' (sh "echo foo bar") `shouldBe` Allow
      computePolicy rt' (sh "echo foo foo2 bar") `shouldBe` Allow

    it "computes policies correctly (arg metavar with quant ?)" $ do
      let rt = testRt [("echo @arg?", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Allow
      computePolicy rt (sh "echo foo") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar") `shouldBe` Ask

    it "computes policies correctly (quant with trailing literal)" $ do
      let rt = testRt [("echo @arg+ done", Allow)]
      computePolicy rt (sh "echo done") `shouldBe` Ask
      computePolicy rt (sh "echo foo done") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar done") `shouldBe` Allow
      computePolicy rt (sh "echo foo") `shouldBe` Ask

    it "computes policies correctly (anything metavar)" $ do
      let rt = testRt [("echo @anything", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Allow
      computePolicy rt (sh "echo foo") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar") `shouldBe` Allow
      computePolicy rt (sh "ls") `shouldBe` Ask

    it "computes policies correctly (choices)" $ do
      let rt = testRt [("echo @{foo,bar}", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo foo") `shouldBe` Allow
      computePolicy rt (sh "echo bar") `shouldBe` Allow
      computePolicy rt (sh "echo abc") `shouldBe` Ask
      computePolicy rt (sh "echo foo bar") `shouldBe` Ask

    it "computes policies correctly (command choices)" $ do
      let rt = testRt [("@{foo,bar} x", Allow)]
      computePolicy rt (sh "echo x") `shouldBe` Ask
      computePolicy rt (sh "foo x") `shouldBe` Allow
      computePolicy rt (sh "bar x") `shouldBe` Allow

    it "computes policies correctly (choices with quant)" $ do
      let rt = testRt [("echo @{foo,bar}*", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Allow
      computePolicy rt (sh "echo foo") `shouldBe` Allow
      computePolicy rt (sh "echo bar") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar foo") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar foo baz") `shouldBe` Ask

    it "computes policies correctly (metavar choices)" $ do
      let rt = testRt [("echo @{@@,@int}", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo @") `shouldBe` Allow
      computePolicy rt (sh "echo 123") `shouldBe` Allow
      computePolicy rt (sh "echo foo bar") `shouldBe` Ask

    it "computes policies correctly (metavar quant choices)" $ do
      let rt = testRt [("echo @{@@,@int+}", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo @") `shouldBe` Allow
      computePolicy rt (sh "echo 123") `shouldBe` Allow
      computePolicy rt (sh "echo 123 123") `shouldBe` Allow
      computePolicy rt (sh "echo abc") `shouldBe` Ask

    it "computes policies correctly (metavar choices with quant)" $ do
      let rt = testRt [("echo @{@@,@int}+", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo @") `shouldBe` Allow
      computePolicy rt (sh "echo 1") `shouldBe` Allow
      computePolicy rt (sh "echo 1 2 @") `shouldBe` Allow
      computePolicy rt (sh "echo 1 2 -x foo") `shouldBe` Ask

    it "computes policies correctly (quoted metavar)" $ do
      let rt = testRt [("echo '@arg'", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Ask
      computePolicy rt (sh "echo @arg") `shouldBe` Allow
      computePolicy rt (sh "echo -x") `shouldBe` Ask
      let rt' = testRt [("echo \"@arg\"", Allow)]
      computePolicy rt' (sh "echo") `shouldBe` Ask
      computePolicy rt' (sh "echo @arg") `shouldBe` Allow
      computePolicy rt' (sh "echo -x") `shouldBe` Ask

    it "computes policies correctly (dollar expansion)" $ do
      let rt = testRt [("echo @anything", Allow), ("foo", Allow)]
      computePolicy rt (sh "echo $(echo)") `shouldBe` Allow
      computePolicy rt (sh "echo $(foo)") `shouldBe` Allow
      computePolicy rt (sh "echo | echo $(foo)") `shouldBe` Allow

    it "computes policies correctly (dollar braced)" $ do
      let rt = testRt [("echo @anything", Allow), ("foo @arg", Allow), ("baz @arg", Allow)]
      computePolicy rt (sh "echo $test") `shouldBe` Allow
      computePolicy rt (sh "echo '$test'") `shouldBe` Allow
      computePolicy rt (sh "echo \"$test\"") `shouldBe` Allow
      computePolicy rt (sh "foo $test") `shouldBe` Ask
      computePolicy rt (sh "foo $test $test") `shouldBe` Ask
      computePolicy rt (sh "baz $test") `shouldBe` Ask
      computePolicy rt (sh "baz $test $test") `shouldBe` Ask

    it "computes policies correctly (source command)" $ do
      let rt = testRt [("source @arg", Allow)]
      computePolicy rt (sh "source x") `shouldBe` Allow
      computePolicy rt (sh "source x y") `shouldBe` Ask

    it "computes policies correctly (source command #2)" $ do
      let rt = testRt [(". @arg", Allow)]
      computePolicy rt (sh ". x") `shouldBe` Allow
      computePolicy rt (sh ". x y") `shouldBe` Ask

    it "computes policies correctly (if)" $ do
      let rt = testRt [("echo @arg", Allow), ("true", Allow)]
      computePolicy rt (sh "if true; then echo hi; fi") `shouldBe` Allow
      computePolicy rt (sh "if true; then rm -rf /; fi") `shouldBe` Ask

    it "computes policies correctly (while)" $ do
      let rt = testRt [("echo @arg", Allow), ("true", Allow)]
      computePolicy rt (sh "while true; do echo hi; done") `shouldBe` Allow
      computePolicy rt (sh "while true; do rm -rf /; done") `shouldBe` Ask

    it "computes policies correctly (for)" $ do
      let rt = testRt [("echo @arg", Allow)]
      computePolicy rt (sh "for x in a b c; do echo hi; done") `shouldBe` Allow
      computePolicy rt (sh "for x in a b c; do rm hi; done") `shouldBe` Ask

    it "computes policies correctly (case)" $ do
      let rt = testRt [("echo @arg", Allow)]
      computePolicy rt (sh "case x in a) echo hi;; esac") `shouldBe` Allow
      computePolicy rt (sh "case x in a) rm -rf /;; esac") `shouldBe` Ask

    it "computes policies correctly (subshell)" $ do
      let rt = testRt [("echo @arg", Allow)]
      computePolicy rt (sh "(echo hi)") `shouldBe` Allow
      computePolicy rt (sh "(rm -rf /)") `shouldBe` Ask

    it "computes policies correctly (group)" $ do
      let rt = testRt [("echo @(-C @arg)", Allow)]
      computePolicy rt (sh "echo -C foo") `shouldBe` Allow
      computePolicy rt (sh "echo -C") `shouldBe` Ask
      computePolicy rt (sh "echo foo") `shouldBe` Ask
      computePolicy rt (sh "echo -C foo bar") `shouldBe` Ask
      let rt' = testRt [("echo @(-C @int) @arg", Allow)]
      computePolicy rt' (sh "echo -C 3 foo") `shouldBe` Allow
      computePolicy rt' (sh "echo -C foo bar") `shouldBe` Ask
      computePolicy rt' (sh "echo foo") `shouldBe` Ask

    it "computes policies correctly (group with only literal)" $ do
      let rt = testRt [("echo @(foo)?", Allow)]
      computePolicy rt (sh "echo foo") `shouldBe` Allow
      computePolicy rt (sh "echo") `shouldBe` Allow
      computePolicy rt (sh "echo bar") `shouldBe` Ask

    it "computes policies correctly (group with quant)" $ do
      let rt = testRt [("echo @(-C @arg)*", Allow)]
      computePolicy rt (sh "echo") `shouldBe` Allow
      computePolicy rt (sh "echo -C foo") `shouldBe` Allow
      computePolicy rt (sh "echo -C foo -C bar") `shouldBe` Allow
      computePolicy rt (sh "echo -C") `shouldBe` Ask
      computePolicy rt (sh "echo foo") `shouldBe` Ask
      let rt' = testRt [("echo @(-C @arg)+", Allow)]
      computePolicy rt' (sh "echo") `shouldBe` Ask
      computePolicy rt' (sh "echo -C foo") `shouldBe` Allow
      computePolicy rt' (sh "echo -C foo -C bar") `shouldBe` Allow
      let rt'' = testRt [("echo @(-C @arg)?", Allow)]
      computePolicy rt'' (sh "echo") `shouldBe` Allow
      computePolicy rt'' (sh "echo -C foo") `shouldBe` Allow
      computePolicy rt'' (sh "echo -C foo -C bar") `shouldBe` Ask
