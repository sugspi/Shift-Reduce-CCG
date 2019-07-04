package edin.ccg

import java.io.File

import scala.io.StdIn.readLine
import edin.ccg.representation.DerivationsLoader
import edin.ccg.representation.tree._

object MainShiftReduce {

  def main(args:Array[String]) : Unit = {

    val fileName = "/Users/guru/MyResearch/Shift-Reduce/hw/toy.trees"//"path of your *.tree"

    val test = DerivationsLoader.fromFile(fileName).toList
    //println(test.size + " trees successfully loaded")
    //println(test)

    for (tree <- test){
      println((tree.words).mkString(" "))
      output_oracle(tree)
      println("")
    }
    //test(0).visualize() //visualization function

    def output_oracle(tree:TreeNode) {
      for(node <- tree.allNodesPostorder){
        node match {
          case TerminalNode(_, tag) =>
            println("shift")
            println("tag-"+tag)
          case BinaryNode(c, _, _) =>
            println("binary_reduce-"+c)
          case UnaryNode(c, _) =>
            println("unary_reduce"+c)
        }
      }
    }


  }

}
