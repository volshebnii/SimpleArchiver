package patmat

import Huffman._
import java.nio.file.{Files, Paths}
import java.io._

import scala.collection
import scala.io.Source

/**
  * Created by qwerty on 23.09.16.
  */

object Code {
  def main(args: Array[String]): Unit = {
    val files = List(
      ("/home/qwerty/Documents/Univer/OIT/Lab1/Pic14.bmp", "UTF-8"),
      ("/home/qwerty/Documents/Univer/OIT/Lab1/Pic14.JPG", "UTF-8"),
      ("/home/qwerty/Documents/Univer/OIT/Lab1/text14.doc", "ISO-8859-5"),
      ("/home/qwerty/Documents/Univer/OIT/Lab1/text14.docx", "ISO-8859-5"),
      ("/home/qwerty/Documents/Univer/OIT/Lab1/text14.txt", "Cp1251")
    )

    val txtFile = "/home/qwerty/Documents/help/commands"
    val pdfFile = "/home/qwerty/Documents/книги/haskell/ru-haskell-book.pdf"

    val byteArray = Source.fromFile(files(1)._1, files(2)._2).toList
    //val byteArray = Files.readAllBytes(Paths.get(files(1)._1))
    //rintln(byteArray.take(5000))
    val codeTree = createCodeTree(byteArray)


    val encodedT = quickEncode(codeTree)(byteArray)
    val decodedT = decode2(codeTree, encodedT)


    val dStr = decodedT.mkString
    println(dStr.take(2000))
    val writer = new PrintWriter(new File(files(1)._1 + ".Res"))

    writer.write(dStr)
    writer.close()

  }
}
