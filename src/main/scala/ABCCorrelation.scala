import scala.io._

object ABCCorrelation extends App {

  // Get a string input and store it into a variable or something and compare
  // the number of occurrences of the letters "a", "b" and "c".
  //
  // All other characters MUST be ignored. If the "a"'s, "b"'s and "c"'s occur with exactly equal frequency,
  // return true; otherwise return false.

  def checkWord(word: String, n: Int): Boolean = {
    /**
     * Функция проверки строки на условие из задачи
     *
     * @param word строка для проверки
     * @param n    минимальное количество вхождений для каждого символа
     */

    val counter = word.toUpperCase.trim
      .filter(x => "ABC".contains(x))
      .groupBy(identity).mapValues(_.length)

    counter.size == 3 && counter.values.toSet.size == 1 && counter.getOrElse('A', 0) >= n
  }

  // тест кейсы с использованием файла words_alpha.txt
  val source = Source.fromFile("src/main/scala/words_alpha.txt")
  val finalList = source.getLines.filter(line => checkWord(line, 2)).toList // чтобы не заполонять вывод в консоль взяли дополнительный фитльтр - counter('A') >= 2
  source.close()

  val checkList = List(
    "abboccato",
    "bambocciade",
    "beccabunga",
    "blackback",
    "bombacaceous",
    "brachiocubital",
    "buccolabial",
    "cabbalistic",
    "subbrachycephaly",
    "subcarbonaceous"
  )

  println("true: ") // выводим все что true
  finalList.foreach(println)

  //тут проверим корректность
  checkList.indices.foreach(x => assert(finalList(x) == checkList(x)))

}
