package edin.ccg
import java.io.{File, PrintWriter}

import edin.ccg.representation.DerivationsLoader
import edin.ccg.representation.category.Category
import edin.ccg.representation.tree.{BinaryNode, TerminalNode, TreeNode, UnaryNode}

object MainCreateRule {

  def main(args: Array[String]): Unit = {

    val file_train = "/Users/guru/MyResearch/Shift-Reduce/org_data/wsj_02-21.depccg.auto" //train.trees"
    val file_dev = "/Users/guru/MyResearch/Shift-Reduce/org_data/dev.trees"
    val file_test = "/Users/guru/MyResearch/Shift-Reduce/org_data/test.trees"

    val trees_train = DerivationsLoader.fromFile(file_train).toList
    val trees_dev = DerivationsLoader.fromFile(file_dev).toList
    val trees_test = DerivationsLoader.fromFile(file_test).toList
    val trees = trees_dev ++ trees_test ++ trees_train

    //val table = createNonTerminalTable(trees)// not need anymore
    val rules = findAllTheRules(trees)//, table)

    val pw_table = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/data/ccg_rule_table")
    val pw_ccg_trees_train = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/data/train.tree")
    val pw_ccg_trees_dev = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/data/dev.tree")
    val pw_ccg_trees_test = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/data/test.tree")
    val train_oracle = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/data/depccg_train_preorder_new2.oracle")
    val dev_oracle = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/data/dev_preorder.oracle")
    val test_oracle = new PrintWriter("/Users/guru/MyResearch/Shift-Reduce/data/test_preorder.oracle")

    pw_table.println(rules.mkString("\n"))
    //println(rules.mkString("\n"))
    pw_table.close()

    for(tree <- trees_train){
      pw_ccg_trees_train.println(tree2string(tree))
    }
    pw_ccg_trees_train.close()

    for(tree <- trees_dev){
      pw_ccg_trees_dev.println(tree2string(tree))
    }
    pw_ccg_trees_dev.close()

    for(tree <- trees_test){
      pw_ccg_trees_test.println(tree2string(tree))
    }
    pw_ccg_trees_test.close()

    var word_dict_from_tree = get_word_count(trees_train)
    var word_dict = get_dict(word_dict_from_tree)
    //output_oracle(trees_test(1),word_dict)

    for(tree <- trees_train){
     output_oracle(tree,word_dict,train_oracle)
    }
    train_oracle.close()

    for(tree <- trees_dev){
     output_oracle(tree,word_dict,dev_oracle)
    }
    dev_oracle.close()

    for(tree <- trees_test){
     output_oracle(tree,word_dict,test_oracle)
    }
    test_oracle.close()

  }

  def findAllTheRules(trees: List[TreeNode]) : Set[String] = {
    val allRules = scala.collection.mutable.Set[String]()
    for(tree <- trees){
      tree.allNodes.foreach{
        case node@UnaryNode(_, child) =>
          allRules += s"${node.category},${child.category}"
          //allRules += s"${table(node.category)} -> ${table(child.category)}"
        case node@BinaryNode(_, left, right) =>
          allRules += s"${node.category},${left.category},${right.category}"
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

  def tree2string(node:TreeNode) : String = node match {
    case UnaryNode(_, child) => "("+node.category+ " " +tree2string(child)+ ")"
    case BinaryNode(_, left, right) => "(" +node.category+ " " +tree2string(left) + " "+tree2string(right) + ")"
    case TerminalNode(word, category) => s"(${category} $word)"
  }

  def get_word_count(trees:List[TreeNode]):Map[String,Int] = {
    trees.foldLeft(Map.empty[String, Int]){ (counter, t) =>
      t.words.foldLeft(counter) {(counter,word)=>
        counter + (word -> (counter.getOrElse(word, 0) + 1))
      }
    }
  }

  def get_dict(dict:Map[String,Int]) = {
    dict.filter(k => { k._2 > 1 })//.keys.toList
  }


  def isAllDigits(x: String) = x forall Character.isDigit
  def checkDash(x: String) = x.contains('-')
  def isAllAlpha(x: String) = x forall (c => c.isLetter && c <= 'z')
  def checkLower(x:String) = x exists (c=>c.isLower)
  def checkUpper(x:String) = x exists (c=>c.isUpper)

  def add_unk_inf(token:String,word_dict:Map[String,Int]):String = {
    var result = ""
    if(token.isEmpty){
      result = "UNK"
    } else if(word_dict.isDefinedAt(token)){ //dictになかったら
      result = token
    }else{
      var numCaps = 0
      var hasDigit = false
      var hasDash = false
      var hasLower = false

      if(isAllDigits(token)){hasDigit = true}
      else if(checkDash(token)){hasDash = true}
      else if(isAllAlpha(token)){
        if(checkLower(token)){hasLower = true}
        if(checkUpper(token)){
          token.foreach(c =>
            if(c.isUpper){numCaps = numCaps+1}
          )
        }
      }
      result = "UNK"
      var lower = token.toLowerCase
      var ch0 = token.head

      if(ch0.isUpper){
        if(numCaps == 1){
          result = result + "-INITC"
          if(word_dict.isDefinedAt(lower)){
            result = result + "-KNOWNLC"
          }
        }else{
          result = result + "-CAPS"
        }
      }else if(!(ch0.isLetter && ch0 <= 'z') && numCaps > 0){
        result = result + "-CAPS"
      }else if(hasLower){
        result = result + "-LC"
      }
      if(hasDigit){result = result + "-NUM"}
      if(hasDash){result = result + "-DASH"}
      if(lower.last == 's' && lower.length >= 3){
        var ch2 = lower.takeRight(2)
        if(!(ch2 == 's') && !(ch2 == 'i') && !(ch2 == 'u')){
          result = result + "-s"
        }
      }else if(lower.length >=5 && !(hasDash) && !(hasDigit && (numCaps > 0))){
        if (lower.substring(lower.length-2, lower.length) == "ed")
          result = result + "-ed"
        else if (lower.substring(lower.length-3, lower.length) == "ing")
          result = result + "-ing"
        else if (lower.substring(lower.length-3, lower.length) == "ion")
          result = result + "-ion"
        else if (lower.substring(lower.length-2, lower.length) == "er")
          result = result + "-er"
        else if (lower.substring(lower.length-3, lower.length) == "est")
          result = result + "-est"
        else if (lower.substring(lower.length-2, lower.length) == "ly")
          result = result + "-ly"
        else if (lower.substring(lower.length-3, lower.length) == "ity")
          result = result + "-ity"
        else if (lower.substring(lower.length-1, lower.length) == "y")
          result = result + "-y"
        else if (lower.substring(lower.length-2, lower.length) == "al")
          result = result + "-al"
      }
    }
    result
  }

  //token is not lowercase
  def unkify(tokens:List[String], words_dict:Map[String,Int]):List[String] ={
    tokens.foldLeft(List.empty[String]){(strings, token) =>
      strings ++ List(add_unk_inf(token, words_dict))
    }
  }

  def output_oracle(tree:TreeNode, word_dict:Map[String,Int],oracle:PrintWriter) {

    oracle.println("# "+tree2string(tree))

    val category_tag = tree.category_leafs
    for (c <- category_tag) { oracle.print(c + " ") }
    oracle.println("")

    val sentence = tree.words
    for (s <- sentence) { oracle.print(s + " ") }
    oracle.println("")

    for (s <- sentence) { oracle.print(s.toLowerCase + " ") }
    oracle.println("")

    unkify(sentence, word_dict).foreach(c => oracle.print(c + " "))
    oracle.println("")

    tree.allNodesPreorder2.foreach(oracle.println)
    oracle.println("")

  }

}
