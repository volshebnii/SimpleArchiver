package patmat


import common._

import scala.annotation.tailrec

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Byte], weight: Int) extends CodeTree
  case class Leaf(char: Byte, weight: Int) extends CodeTree
  

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match{
    case Fork(left: CodeTree, right: CodeTree, chars: List[Byte], weight: Int) => weight
    case Leaf(char: Byte, weight: Int) => weight
  } // tree match ...
  
  def chars(tree: CodeTree): List[Byte] = tree match{
    case Fork(left: CodeTree, right: CodeTree, chars: List[Byte], weight: Int) => chars
    case Leaf(char: Byte, weight: Int) => List(char)
  } // tree match ...
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Byte]): List[(Byte, Int)] = chars.groupBy(identity).mapValues(_.size).toList
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Byte, Int)]): List[Leaf] = freqs match{
    case (x, y) :: xs => Leaf(x, y) :: makeOrderedLeafList(xs) sortWith(_.weight < _.weight)
    case Nil => Nil
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match{
    case x :: Nil => true
    case _ => false
  }
  
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match{
    case x :: y :: xs => makeCodeTree(x, y) :: xs sortWith(weight(_) < weight(_))
  }
  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */

  @tailrec
  def until(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if(singleton(trees)) trees
    else until(singleton, combine)(combine(trees))
  }
  
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */

  def createCodeTree(chars: List[Byte]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))) head

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Byte] = {
    def traverse(t: CodeTree, bits: List[Bit]): List[Byte] = t match {
      case Leaf(char, _) => if(bits.isEmpty) List(char) else char :: traverse(tree, bits)
      case Fork(left, right, _, _) => if(bits.head == 0) traverse(left, bits.tail) else traverse(right, bits.tail)
    }
    traverse(tree, bits)
  }

  def decode2(tree: CodeTree, bits: List[Bit]): Stream[Byte] = {
    def traverse(t: CodeTree, bits: List[Bit]): Stream[Byte] = t match {
      case Leaf(char, _) => if(bits.isEmpty) Stream(char) else char #:: traverse(tree, bits)
      case Fork(left, right, _, _) => if(bits.head == 0) traverse(left, bits.tail) else traverse(right, bits.tail)
    }
    traverse(tree, bits)
  }


  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Byte]): List[Bit] = {
    def encodeSymbol(t: CodeTree)(ch: Byte): List[Bit] = t match {
      case Leaf(_, _) => Nil
      case Fork(left, right, chs, _) =>
        if(chars(left).contains(ch)) 0 :: encodeSymbol(left)(ch)
        else if(chars(right).contains(ch)) 1 :: encodeSymbol(right)(ch)
        else throw new IllegalArgumentException
    }

    if (text.isEmpty) Nil
    else text flatMap (encodeSymbol(tree)(_))
  }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Byte, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Byte): List[Bit] = table match {
    case (ch, bits) :: xs => if(ch == char) bits else codeBits(xs)(char)
    case Nil => throw new NoSuchElementException
  }
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(ch, _) => List((ch, List()))
    case Fork(l, r, _, _) => mergeCodeTables(convert(l), convert(r))
  }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    def prepend(bit: Bit)(codeTable: CodeTable): CodeTable = codeTable map (code => (code._1, bit :: code._2))
    prepend(0)(a) ::: prepend(1)(b)
  }
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Byte]): List[Bit] = text flatMap codeBits(convert(tree))
  }
