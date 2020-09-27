package recfun

object RecFun {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */

 def pascal(c: Int, r: Int): Int = c match {
   case 0 => 1
   case `c` if c >= r => 1
   case _ => pascal(c-1, r-1)+pascal(c,r-1)
 }
println(pascal(1,9))

 def balance(chars: List[Char]): Unit = {
   def test (c: Char, l: List[Char], i: Int): Int={
     if( l.isEmpty ) {
       if(c=='(') i+1
       else if(c==')') i-1
       else i
     }
     else if (c=='(')
       test(l.head, l.tail,i+1)
     else if (c==')') { if (i<1) i+1
     else
       test(l.head, l.tail, i-1)
     } else
       test(l.head,l.tail, i)
   }

   if(test(chars.head, chars.tail,0) ==0 )
     println("true"):Unit
   else
     println("false"):Unit
 }
  balance("())(".toList) //passes when it should fail
  balance(":-)".toList)
  balance("(if (zero? x) max (/ 1 x))".toList)
  balance("I told him (that it not (yet) done).\n(But he wasn't listening)".toList)

  def countChange(money: Int, coins: List[Int]): Int = {
    def test(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else test(money - coins.head, coins) + test(money, coins.tail)

    test(money, coins)
  }

  println ("результат: " + countChange(4,List(1,2)))
}