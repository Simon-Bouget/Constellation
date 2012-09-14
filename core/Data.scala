package core

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.LinkedHashSet

class Data

trait QualitativeData {
  val qualitativeContent = new HashMap[String, LinkedHashSet[Any]]()
}

trait QuantitativeData {
  val qualitativeContent = new HashMap[String, HashMap[Any,Int]]()
}