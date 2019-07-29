package edin.ccg
import java.io.{File, PrintWriter}

import edin.ccg.representation.DerivationsLoader
import edin.ccg.representation.category.Category
import edin.ccg.representation.tree.{BinaryNode, TerminalNode, TreeNode, UnaryNode}

object MainCreateRule {

  def main(args: Array[String]): Unit = {

    val file_train = "/Users/guru/MyResearch/Shift-Reduce/hw/train.trees"
    val file_dev = "/Users/guru/MyResearch/Shift-Reduce/hw/dev.trees"
    val file_test = "/Users/guru/MyResearch/Shift-Reduce/hw/test.trees"

    val trees_train = DerivationsLoader.fromFile(file_train).toList
    val trees_dev = DerivationsLoader.fromFile(file_dev).toList
    val trees_test = DerivationsLoader.fromFile(file_test).toList
    val trees = trees_dev ++ trees_test ++ trees_train

    val table = createNonTerminalTable(trees)
    val rules = findAllTheRules(trees, table)

    val pw_table = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/hw/ccg_rule_table")
    val pw_ccg_trees_train = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/hw/ccg_train.tree")
    val pw_ccg_trees_dev = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/hw/ccg_dev.tree")
    val pw_ccg_trees_test = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/hw/ccg_test.tree")

    pw_table.println(rules.mkString("\n"))
    //println(rules.mkString("\n"))

    for(tree <- trees_train){
      pw_ccg_trees_train.println(tree2string(tree, table))
    }

    for(tree <- trees_dev){
      pw_ccg_trees_dev.println(tree2string(tree, table))

    }

    for(tree <- trees_test){
      pw_ccg_trees_test.println(tree2string(tree, table))
    }

    pw_table.close()
    pw_ccg_trees_train.close()
    pw_ccg_trees_dev.close()
    pw_ccg_trees_test.close()


  }

  def findAllTheRules(trees: List[TreeNode], table:Map[Category, String]) : Set[String] = {
    val allRules = scala.collection.mutable.Set[String]()
    for(tree <- trees){
      tree.allNodes.foreach{
        case node@UnaryNode(_, child) =>
          allRules += s"${table(node.category)},${table(child.category)}"
          //allRules += s"${table(node.category)} -> ${table(child.category)}"
        case node@BinaryNode(_, left, right) =>
          allRules += s"${table(node.category)},${table(left.category)},${table(right.category)}"
          //allRules += s"${table(node.category)} -> ${table(left.category)} ${table(right.category)}"
        case _ =>
      }
    }
    allRules.toSet
  }


  def createNonTerminalTable(trees:Seq[TreeNode]) : Map[Category, String] = {

    val allNonTerminals = scala.collection.mutable.Set[Category]()

    for(tree <- trees){
      for(node <- tree.allNodes){
        allNonTerminals += (node.category)
      }
    }

    allNonTerminals.toList.sortBy(_.toString).zipWithIndex.map{case (category, index) =>
      (category -> s"NT$index")
    }.toMap
  }

  def tree2string(node:TreeNode, table:Map[Category, String]) : String = node match {
    case UnaryNode(_, child) => "("+table(node.category)+ " " +tree2string(child, table)+ ")"
    case BinaryNode(_, left, right) => "(" +table(node.category)+ " " +tree2string(left, table) + " "+tree2string(right, table) + ")"
    case TerminalNode(word, category) => s"(${table(category)} $word)"
  }

  def output_oracle(tree:TreeNode) {
    for(node <- tree.allNodesPostorder){
      node match {
        case TerminalNode(_, tag) =>
          println("SHIFT")
          println(tag)
        case BinaryNode(c, _, _) =>
          println("REDUCE_B_"+c)
        case UnaryNode(c, _) =>
          println("REDUCE_U_"+c)
      }
    }
  }

}
