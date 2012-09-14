package example

import core._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Set
import scala.collection.mutable.LinkedHashSet

object MyNodeOverlay extends NodeOverlay {
  //type N = MyNode

  class MyNode extends Node[MyNode] with QualitativeData {
    //self: N =>
    var name: String = ""
    override def toString() = name
    qualitativeContent.put("Locations"          , new LinkedHashSet())
    qualitativeContent.put("LocationsHidden"    , new LinkedHashSet())
    //qualitativeContent.put("Subscriptions"      , new LinkedHashSet())
    //qualitativeContent.put("SubscriptionsHidden", new LinkedHashSet())

    clusters.put("RPS", new RPS(this, 10))
    clusters.put("Overlap", new Overlap(this))
    clusters.put("Jaccard", new Jaccard(this))
    clusters.put("Big"    , new Big    (this))
  }

  class RPS(hostingNode: MyNode, numberOfPeers: Int) extends Overlay(hostingNode) {
    /** Utility function used to emulate RPS */
    def randomNodes(n: Int)() = {
      import Simulator._
      val res = new LinkedHashSet[MyNode]()
      for (i <- 1 to n) {
        res += nodes.toList(random.nextInt(nodes size))
      }
      res
    }

    def nextCandidates = randomNodes(numberOfPeers) 

    def measure(nodes: LinkedHashSet[MyNode]) = nodes
  }

  class Overlap(hostingNode: MyNode)
    extends Overlay(hostingNode)
    with simpleMeasure[MyNode] {

    /**
     * Source function for Overlap layer
     */
    def nextCandidates = {
      (for {
        neighbor <- host.clusters.get("Overlap").get.neighbors
        neighborSNeighbor <- neighbor.clusters.get("Overlap").get.neighbors
      } yield neighborSNeighbor)           ++
        host.clusters.get("Overlap").get.neighbors ++
        host.clusters.get("RPS"    ).get.neighbors -
        hostingNode
    }

    /**
     * Measure function for Overlap layer
     */
    def grade(n: MyNode): Double = {
      val hostContent = host.qualitativeContent.getOrElse("Subscriptions", new LinkedHashSet)
      val nContent    =    n.qualitativeContent.getOrElse("Subscriptions", new LinkedHashSet)
      (hostContent & nContent) size
    }
  }

  class Jaccard(hostingNode: MyNode)
    extends Overlay(hostingNode) 
    with simpleMeasure[MyNode] {
    
    /**
     * Source function for Jaccard layer
     */
    def nextCandidates = {
      (for {
        neighbor <- host.clusters.get("Jaccard").get.neighbors
        neighborSNeighbor <- neighbor.clusters.get("Jaccard").get.neighbors
          /* {
        neighbor <- host.clusters.getOrElse("Jaccard", DefaultOverlay).neighbors
        neighborSNeighbor <- neighbor.clusters.getOrElse("Jaccard", DefaultOverlay).neighbors
      }*/
      } yield neighborSNeighbor) ++
        host.clusters.get("Jaccard").get.neighbors ++
        host.clusters.get("RPS").get.neighbors - 
        hostingNode
    }

    /**
     * Measure function for Jaccard layer
     */
    def grade(n: MyNode): Double = {
      val hostContent = host.qualitativeContent.getOrElse("Subscriptions", new LinkedHashSet)
      val nContent    =    n.qualitativeContent.getOrElse("Subscriptions", new LinkedHashSet)
      if ((hostContent size) == 0 || (nContent size) == 0) return 0
      else {
        val interSize = (hostContent & nContent).size
        interSize / (hostContent.size + nContent.size - interSize) // necessary because of a bug on union
      }
    }
  }
  
  class Big(hostingNode: MyNode)
    extends Overlay(hostingNode)
    with simpleMeasure[MyNode] {

    /**
     * Source function for Overlap layer
     */
    def nextCandidates = {
      (for {
        neighbor <- host.clusters.get("Big").get.neighbors
        neighborSNeighbor <- neighbor.clusters.get("Big").get.neighbors
      } yield neighborSNeighbor)           ++
        host.clusters.get("Big").get.neighbors ++
        host.clusters.get("RPS"    ).get.neighbors -
        hostingNode
    }

    /**
     * Measure function for Overlap layer
     */
    def grade(n: MyNode): Double = {
      n.qualitativeContent.getOrElse("Subscriptions", new LinkedHashSet).size
    }
  }

}
