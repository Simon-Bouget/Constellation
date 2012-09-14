package example

import scala.collection.mutable.{ HashSet, LinkedList, LinkedHashSet }
import scala.util.Random
import core._
import scala.io.Source._
import scala.util.parsing.json._
import MyNodeOverlay._
import scala.collection.mutable.HashMap

object Simulator {

  var nodes = new LinkedHashSet[MyNode]()
  val random = new Random(0)

  def main(args: Array[String]) {

    /**
     * Data processing before the actual simulation
     */
    val data: String =
      fromFile("D:/Work In Progress/2012_Summer_Internship/dataset/onlyBayLocsUsers-1000.json")
        .mkString

    val profileList = JSON parseRaw data match {
      case Some(x: JSONArray) => x
      case Some(x: JSONObject) => throw new IllegalArgumentException("Object instead of Array")
      case None => throw new IllegalArgumentException("None")
    }

    /**
     * Node creation and initialization
     */
    
   
     
    var subsSet = new HashMap[Any, Int]()
    println("enter profile loading")
    for (profile <- profileList.list) {
      var node = new MyNode()
      node.name = profile.asInstanceOf[JSONObject].obj.get("name").get.asInstanceOf[String]
      //var locations = new LinkedHashSet ++
      //  profile.asInstanceOf[JSONObject].obj.get("locs").get.asInstanceOf[JSONArray].list
      //node.qualitativeContent.put("Locations", locations)
      var thisSubs = profile.asInstanceOf[JSONObject].obj.get("subs").get.asInstanceOf[JSONArray].list
      var subscriptions = new LinkedHashSet ++ thisSubs
      node.qualitativeContent.put("Subscriptions", subscriptions)
      for (sub <- thisSubs) {
        if (subsSet.contains(sub)) subsSet(sub) += 1
        else subsSet(sub) = 1
      }
      nodes += node
    }
    
     /*
    println("enter test profile loading")
    var node1 = new MyNode()
    node1.name = "first"
    node1.qualitativeContent.put("Subscriptions",new LinkedHashSet() ++= 42::1::Nil)
    node1.qualitativeContent.put("SubscriptionsHidden",new LinkedHashSet() + 42)
    var node2 = new MyNode()
    node2.name = "second"
    var subs = new LinkedHashSet[Any]()
    subs += 42
    subs += 43
    subs += 1
    subs += 3
    node2.qualitativeContent.put("Subscriptions", subs)
    node2.qualitativeContent.put("SubscriptionsHidden",new LinkedHashSet() + 2)
    var node3 = new MyNode()
    node3.name = "third"
    node3.qualitativeContent.put("Subscriptions",new LinkedHashSet() + 43)
    node3.qualitativeContent.put("SubscriptionsHidden",new LinkedHashSet() + 4)
    var node4 = new MyNode()
    node4.name = "fourth"
    node4.qualitativeContent.put("Subscriptions",new LinkedHashSet() ++= 42::3::Nil)
    node4.qualitativeContent.put("SubscriptionsHidden", (new LinkedHashSet() ++= 42::43::Nil))

    nodes += node1 += node2 += node3 += node4
    */
     
    
    println("enter subs filtering")
    for ((sub, count) <- subsSet if count < 20; node <- nodes) {
      var nodeSubs = node.qualitativeContent.get("Subscriptions").get
      nodeSubs.remove(sub)
      if (nodeSubs.size < 2) nodes.remove(node)
    }
    println("nodes size = " + nodes.size.toString())
    println("entering separation")
    var percentage = 0.2
    for (node <- nodes) {
      var subs = node.qualitativeContent.get("Subscriptions").get
      var nbHidden = (subs.size * percentage).floor.toInt
      if (nbHidden == 0) nbHidden = 1
      var hidden = subs take nbHidden
      var visible = subs drop nbHidden
      node.qualitativeContent.put("Subscriptions", visible)
      node.qualitativeContent.put("SubscriptionsHidden", hidden)

    }

    /**
     * Simulation loop
     */
    var randomWitness = nodes.toList(random.nextInt(nodes size))

    for (i <- 1 to 50) {
      println("Start of turn: " + i)
      val now = System.nanoTime()
      //var totalPredicted = 0
      //var totalToPredict = 0
      var predictedOv: List[Int] = Nil
      var predictedJa: List[Int] = Nil
      var predictedBi: List[Int] = Nil
      var toPredict: List[Int] = Nil

      for (node <- nodes) {
        node.clusters.get("RPS").get.run
        node.clusters.get("Overlap").get.run
        node.clusters.get("Jaccard").get.run
        node.clusters.get("Big"    ).get.run

        var predictionOv = (for (
          neighbor <- node.clusters.get("Overlap").get.neighbors;
          sub <- neighbor.qualitativeContent.get("Subscriptions").get
        ) yield sub)
       var predictionJa = (for (
         neighbor <- node.clusters.get("Jaccard").get.neighbors;
         sub <- neighbor.qualitativeContent.get("Subscriptions").get
       ) yield sub)
       var predictionBi = (for (
         neighbor <- node.clusters.get("Big").get.neighbors;
         sub <- neighbor.qualitativeContent.get("Subscriptions").get
       ) yield sub)
        var subsHidden = node.qualitativeContent.get("SubscriptionsHidden").get
        //totalPredicted += (prediction & subsHidden).size
        //totalToPredict += subsHidden.size
        predictedOv = (predictionOv & subsHidden).size :: predictedOv
        predictedJa = (predictionJa & subsHidden).size :: predictedJa
        predictedBi = (predictionBi & subsHidden).size :: predictedBi
        toPredict = subsHidden.size :: toPredict
      }

      val end = System.nanoTime() - now
      println("durée du round " + i + " : " + end.toDouble / 1000000000)
      //println("Global recall: " + totalPredicted.toDouble / totalToPredict)
      println("Global recallOv: " + (predictedOv reduce (_ + _)).toDouble / (toPredict reduce (_ + _)))
      println("Global recallJa: " + (predictedJa reduce (_ + _)).toDouble / (toPredict reduce (_ + _)))
      println("Global recallBi: " + (predictedBi reduce (_ + _)).toDouble / (toPredict reduce (_ + _)))
    }

    /**
     * Final display
     */
    println("Final display")
   // println("Overlap :" + randomWitness.clusters.get("Overlap").get.neighbors)
    //println("Jaccard :" + randomWitness.clusters.get("Jaccard").get.neighbors)

    /**
     *     var thand = for (node <- nodes if node.name == "17thandirving") yield node
    thand map { x: MyNode =>
      {
        println("name :" + x.name)
        var prediction = (for (
          neighbor <- x.clusters.get("Overlap").get.neighbors;
          sub <- neighbor.qualitativeContent.get("Subscriptions").get
        ) yield sub).asInstanceOf[LinkedHashSet[Int]]
        println("prediction :" + prediction)
        for (hidSubs <- x.qualitativeContent.get("SubscriptionsHidden").get) {
          println("hidSubs: " + hidSubs)
          if (prediction.contains(hidSubs.asInstanceOf[Double].round.toInt)) println("Found!") else println("fail...")
        }
        println("name :" + x.name)
      }
    }

     */
  } // end of main

  /** Utility function used to emulate RPS */
  def randomNodes(n: Int)() = {
    //import Simulator._
    val res = new LinkedHashSet[MyNode]()
    for (i <- 1 to n) {
      res += nodes.toList(random.nextInt(nodes size))
    }
    res
  }

}