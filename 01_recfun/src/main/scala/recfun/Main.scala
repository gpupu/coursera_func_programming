package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c==0 || c == r){
      return 1
    } else{
      return pascal(c-1,r-1) + pascal(c,r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

		  return balance(chars,0)
  }

  def balance(chars: List[Char], num: Int): Boolean = {
		  if(chars.isEmpty){
			  return (num == 0)
		  }
		  if(num < 0){
			  return false
		  }
		  if(chars.head == '('){
			  return balance(chars.tail, num +1)
		  }
		  if(chars.head == ')'){
			  return balance(chars.tail, num -1)
		  }
		  // in other case
		  return balance(chars.tail,num)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty && (money!= 0)){
      return 0
    }
    if(coins.isEmpty && (money == 0)){
      return 1
    }
    val div:Int = money/coins.head
    var x:Int = 0
    var result:Int = 0
    for(x <- 0 to div){
      result = result + countChange(money - (coins.head*x),coins.tail)
    }

    return result;
  }
}
