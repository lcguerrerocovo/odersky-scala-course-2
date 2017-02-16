package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("colorForRemainingCharsCount with a signal") {
    val a = Var(52)
    val resultGreen = TweetLength.colorForRemainingCharsCount(a)
    assert(resultGreen() == "green")

    val remaining = a()
    a() = remaining - 50

    assert(resultGreen() == "orange")

    val remaining2 = a()
    a() = remaining2 - 5

    assert(resultGreen() == "red")
  }

  test("delta x^2 + 4x + 4") {
    (Polynomial.computeDelta(Signal(1d),Signal(4d),Signal(4d))()) should equal (0d)
  }

  test("roots x^2 + 4x + 4") {
    val (a,b,c) = (Signal(1d),Signal(4d),Signal(4d))

    val delta = Polynomial.computeDelta(a,b,c)

    println(Polynomial.computeSolutions(a,b,c,delta)())
    (Polynomial.computeSolutions(a,b,c,delta)()) should equal (Set(-2d))
  }

  test("roots x^2 - 4") {
    val (a,b,c) = (Signal(1d),Signal(0d),Signal(-4d))

    val delta = Polynomial.computeDelta(a,b,c)

    println(Polynomial.computeSolutions(a,b,c,delta)())
    (Polynomial.computeSolutions(a,b,c,delta)()) should equal (Set(-2d,2d))
  }

  test("coefficients change") {
    val (a,b,c) = (Var(1d),Var(4d),Var(4d))

    val delta = Polynomial.computeDelta(a,b,c)

    println(Polynomial.computeSolutions(a,b,c,delta)())
    (Polynomial.computeSolutions(a,b,c,delta)()) should equal (Set(-2d))

    b() = 0d
    c() = -4d
    (Polynomial.computeSolutions(a,b,c,delta)()) should equal (Set(-2d,2d))
  }

  test("a = 2, a + 3 = 5 with eval") {
    val references: Map[String,Signal[Expr]] = Map("a" -> Signal(Literal(2d)))

    Calculator.eval(Plus(Ref("a"),Literal(3d)),references) should equal (5d)
  }

  test("a = 2, b = 2 + a with computeValues") {
    val references: Map[String,Signal[Expr]]
      = Map("a" -> Signal(Literal(2d)), "b" -> Signal(Plus(Ref("a"),Literal(3d))))

    Calculator.computeValues(references)("b")() should equal (5d)
  }

  test("computeValues updating expression") {
    val references: Map[String,Signal[Expr]]
    = Map("a" -> Signal(Literal(2d)), "b" -> Signal(Plus(Ref("a"),Literal(3d))))

    Calculator.computeValues(references + ("a" -> Signal(Literal(3d))))("b")() should equal (6d)
  }

}
