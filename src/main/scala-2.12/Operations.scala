class Operations {

  def printFibonacci(number: Int): List[Int] = {
    val temp = 1 to number
    val fibonacciList = temp.toList
    fibonacciList.map(x => generateFibonacci(x))
  }

  private def generateFibonacci(number: Int): Int = {
    def tailRecursion(a: Int, b: Int, fib: Int): Int = {
      fib match {
        case 0 => a
        case 1 => b
        case _ => tailRecursion(b, a + b, fib - 1)
      }
    }

    tailRecursion(0, 1, number)
  }

  def doubleTheList(number: List[Int]): List[Int] = {
    number.map(x => x * 2)
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

  private def area(shape: String, first: Int, second: Int, f: (Int, Int) => Int): String = {
    f(first, second)
    s"The Area of $shape is ${f(first, second)}"
  }

  def lengthOfList(number: List[Int]): Int = {
    def countLength(list: List[Int], length: Int): Int = {
      list match {
        case Nil => length
        case _ :: tail => countLength(tail, length + 1)
      }
    }

    countLength(number, 0)
  }

  def findElementByPosition(number: List[Int], position: Int): Int = {
    def countPlaces(list: List[Int], length: Int): Int = {
      if (list.nonEmpty) {
        (length, list) match {
          case (0, head :: _) => head
          case (_, _ :: tail) => countPlaces(tail, length - 1)
        }
      }
      else {
        -1
      }
    }

    countPlaces(number, position)
  }

  def reverseList(list: List[Int]): List[Int] = {
    list match {
      case head :: tail => reverseList(tail) ::: List(head)
      case Nil => Nil
    }
  }

  def addLists(list1: List[Int], list2: List[Int]): List[Int] = {
    if (list1.nonEmpty && list2.nonEmpty) {
      list1.zip(list2).map {
        (tuple) => {
          tuple._1 + tuple._2
        }
      }
    }
    else {
      List()
    }
  }
}

object Operations extends App {
  val obj = new Operations
  val temp = 1 to 10
  val test1: Int = 20
  val test2: Int = 5
  val testList1 = temp.toList
  val testList2 = temp.toList
  val testFibonacci = 5
  val choice = "rectangle"
  print(s"Write a program to find Fibonacci series till a given limit${obj.printFibonacci(testFibonacci)}")
  print(s"List of sums  ${obj.addLists(testList1, testList2)}")
  print(s"Higher order function ${obj.selectOption(choice, test1, test2)}")
  print(s"Double all the elements of a list using map ${obj.doubleTheList(testList2)}")
  print(s"Find the Kth element of a list ${obj.findElementByPosition(testList2, test2)}")
  print(s"Find the number of elements of a list ${obj.lengthOfList(testList1)}")
  print(s"Reverse a list ${obj.reverseList(testList2)}")
}
