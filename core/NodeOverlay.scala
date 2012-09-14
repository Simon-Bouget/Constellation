package core

import scala.collection.mutable.{Set,HashSet,HashMap,Map}
import scala.collection.mutable.LinkedHashSet

abstract class NodeOverlay {
  //type N <: Node

  trait Node[N] {
    var clusters: Map[String, Overlay[N]] = HashMap() // key: common name (e.g. RPS, gossip, etc.); value: ref. to the corresponding Overlay
    var scope: Scope = Container()

  }
  
  trait VNode[N,P] extends Node[N] {
    var container: Node[P]
  }

  trait Container[N, P <: Node[Q], Q] extends Node[N] {
    var content: Set[P]
  }
  
  
  abstract class Overlay[N](val host: N) {
    /**
     * here is how it's supposed to be used :
     * 1/ measure is given a set of candidate and return a set of new neighbors
     * 2/ nextCandidates is a function taking Unit and returning candidate neighbors for the next round
     * 3/ neighbors is the set of current neighbors
     */

    var neighbors: LinkedHashSet[N] = new LinkedHashSet[N]()

    def measure(nodes: LinkedHashSet[N]): LinkedHashSet[N]
    def nextCandidates: LinkedHashSet[N]

    def run {
      neighbors = this measure nextCandidates
    }

  }
  
  trait simpleMeasure[N] extends Overlay[N] {
    def grade(n: N): Double

    override def measure(candidates: LinkedHashSet[N]) = {
      LinkedHashSet() ++ (
        (for (candidate <- candidates.toList) yield (grade(candidate), candidate))
        sortWith { (a, b) => a._1 > b._1 }
        take 5
        map {_._2})
    }
  }

}

