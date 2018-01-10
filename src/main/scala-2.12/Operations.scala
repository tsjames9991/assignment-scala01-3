class Operations {

  private def generateFibonacci(number: Int): Int = {
    def tailRecursion(a: Int, b: Int, fib: Int): Int = {
      fib match {
        case 0 => a
        case 1 => b
        case _ => tailRecursion(b ,a+b , fib-1)
      }
    }
    tailRecursion(0, 1, number)
  }

  def printFibonacci(number: Int): List[Int] = {
    val temp = 1 to number
    val fibonacciList = temp.toList
    fibonacciList.map(x => generateFibonacci(x))
  }

  def doubleTheList(number: List[Int]): List[Int] = {
    number.map(x => x*2)
  }

  private def area(shape: String, first: Int, second: Int, f: (Int, Int) => Int): String = {
    f(first,second)
    s"The Area of ${shape} is ${f(first,second)}"
  }

  def selectOption(option: String, first: Int, second: Int): String = {
    option match {
      case "rectangle" => area(option.toLowerCase, first, second, (first, second) => first * second)
      case "rhombus" => area(option.toLowerCase(), first, second, (first, second) => first * second / 2)
      case "parallelogram" => area(option.toLowerCase(), first, second, (first, second) => first * second)
      case "square" => "Not defined yet for square."
      case _ => "Illegal Quadrilateral"
    }
  }

  def lengthOfList(number: List[Int]): Int  = {
    def countLength(list: List[Int], length: Int): Int = {
      list match {
        case Nil => length
        case _ :: tail => countLength(tail, length+1)
      }
    }
    countLength(number,0)
  }

  def findElementByPosition(number: List[Int], position: Int): Int = {
    def countPlaces(list: List[Int], length: Int): Int = {
      if(list.nonEmpty) {
        (length, list) match {
          case (0, head :: _) => head
          case (_, _ :: tail) => countPlaces(tail, length - 1)
        }
      }
      else {-1}
    }
    countPlaces(number,position)
  }

  def reverseList(list: List[Int]): List[Int] = {
    list match {
      case head :: tail => reverseList(tail) ::: List(head)
      case Nil => Nil
    }
  }

  def addLists(list1: List[Int], list2: List[Int]): List[Int] = {
    if(list1.length > list2.length)
      {
        list1.map {x => list2.lift(list1.indexOf(x)+ x )}
      }
    else {
      list2.map {x => list1.lift(list2.indexOf(x) + x)}
    }
  }
}

object Operations extends App {
  val obj = new Operations
  val temp = 1 to 10
  val testList1 = temp.toList
  val testList2 = temp.toList
  val testFibonacci = 5
  val choice = "rectangle"
  println(s"Write a program to find Fibonacci series till a given limit${obj.printFibonacci(testFibonacci)}")
  println(s"List of sums  ${obj.addLists(testList1,testList2)}")
  println(s"Higher order function ${obj.selectOption(choice, 4, 5)}")
  println(s"Double all the elements of a list using map ${obj.doubleTheList(testList2)}")
  println(s"Find the Kth element of a list ${obj.findElementByPosition(testList2,4)}")
  println(s"Find the number of elements of a list ${obj.lengthOfList(testList1)}")
  println(s"Reverse a list ${obj.reverseList(testList2)}")
}
