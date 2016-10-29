package patmat

import Huffman._
import java.nio.file.{Files, Paths}
import java.io._


object Code {
  def main(args: Array[String]): Unit = {
    val files = List(
      "/home/qwerty/Documents/Univer/OIT/Lab1/Pic14.bmp",
      "/home/qwerty/Documents/Univer/OIT/Lab1/Pic14.JPG",
      "/home/qwerty/Documents/Univer/OIT/Lab1/text14.doc",
      "/home/qwerty/Documents/Univer/OIT/Lab1/text14.docx",
      "/home/qwerty/Documents/Univer/OIT/Lab1/text14.txt"
    )

    val byteArray = Files.readAllBytes(Paths.get(files.head)).toList
    val codeTree = createCodeTree(byteArray)

    val encodedT = quickEncode(codeTree)(byteArray)
    val decodedT = decode2(codeTree, encodedT)

    val bos = new BufferedOutputStream(new FileOutputStream(files.head + "RES.bmp"))
    Stream.continually(bos.write(decodedT.toArray))
    bos.close()

  }
}
