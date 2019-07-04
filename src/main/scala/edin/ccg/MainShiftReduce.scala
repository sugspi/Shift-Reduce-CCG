package edin.ccg

import java.io.File

import scala.io.StdIn.readLine
import edin.ccg.representation.DerivationsLoader
import edin.ccg.representation.tree._

object MainShiftReduce {

  //case class CMDargs(
                    //   ccg_dir : String    = null,
                    //   out_dir : String    = null
                    // )

  def main(args:Array[String]) : Unit = {

    // val parser = new scopt.OptionParser[CMDargs](PROGRAM_NAME) {
    //   head(PROGRAM_NAME, PROGRAM_VERSION.toString)
    //   opt[String]("ccg_dir").action((x, c) => c.copy(ccg_dir = x)).required()
    //   opt[String]("out_dir").action((x, c) => c.copy(out_dir = x)).required()
    // }
    println("succsess main sftred")

    val fileName = "/Users/guru/MyResearch/Rotating-CCG/hw/shftredu_test.trees"

    val test = DerivationsLoader.fromFile(fileName).toList
    println(test.size + " trees successfully loaded")
    //test(0).allNodes
    println("words:\n" + test(0).words + "\n")
    println("allNodesPostorder:\n" + test(0).allNodesPostorder + "\n")
    for(node <- test(0).allNodesPostorder){
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
    test(0).visualize()
    println("leafs:\n" + test(0).leafs + "\n")
    println("children:\n" + test(0).children + "\n")
    println("getCombinator:\n" + test(0).getCombinator + "\n")
  }

}
