package practice.test

import scala.annotation.tailrec
import graph.config.WeightedDirectedGraphConf.WeightedGraph
import graph.config.WeightedDirectedGraphConf.Vertex

object EvaluateGraph {
  val visitedPaths: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer()
  var pathMaps: scala.collection.mutable.Map[String, List[DistVector]] = scala.collection.mutable.Map.empty
  case class DistVector(to: Vertex, distance: Int, visited: Boolean)
  case class DistVectorPlain(to: String, distance: Int)
  def solveMap(graph: WeightedGraph) = {
    graph.graph.getOrElse("sample", List()).map(f => {
      if(f.to.isDefined) pathMaps(f.from.label) = pathMaps.getOrElse(f.from.label, List()) ++ List(DistVector(Vertex(f.to.get.label), f.distance, false))
      else pathMaps(f.from.label) = List()
    })
    println(getDistance("A-B-C"))
    println(getDistance("A-D"))
    println(getDistance("A-D-C"))
    println(getDistance("A-E-B-C-D"))
    println(findDistance("A-E-D", graph, pathMaps))
    println(findBelowCount(graph, "C", "C", pathMaps, "max-loops", 3))
    println(findBelowCount(graph, "A", "C", pathMaps, "exact-loops", 4))
    println(shortestPath(graph, "A","C", pathMaps, 0, false))
    println(findBelowCount(graph, "B","B", pathMaps, "", -1))
//    println(possiblePaths(graph, "C","C", pathMaps, "max-distance", 30, 0))
  }
  
  def findDistance(path: String, g:WeightedGraph, pathMaps: scala.collection.mutable.Map[String, List[DistVector]]) = {
    val sp = path.split("-")
    var cost = 0;
    for(i<-1 until sp.length) {
      cost = cost + shortestPath(g, sp(i-1), sp(i), pathMaps, 0, false)
    }
    cost
  }
  def getDistance(path: String) = {
    var total_distance = 0
    val split = if(path.contains("-")) path.split("-").toList else List() 
    if(split.length > 1) {
      val x : scala.collection.mutable.Map[String, Boolean] =  scala.collection.mutable.Map.empty
      split.foreach(f => x(f) = false)
      getRecDist(path.split("-"), x)
    }
  }
  
  @tailrec
  def getRecDist(nodes: Array[String], marker: scala.collection.mutable.Map[String, Boolean], sum: Int = 0): Int = {
    var nodesRe = nodes
    if(nodes.isEmpty) sum
    else {
      val x = pathMaps.getOrElse(nodes.head, List()).find(_.to.label == nodes.tail.headOption.getOrElse(""))
      if(! marker.getOrElse(nodes.head, false)) {  //never visited, then      
    	  if(x.isDefined) { marker(nodes.head) = true; }
//    	  else { nodesRe = nodes.tail ++ pathMaps.getOrElse(nodes.head, List()).map(_.to.label)}
      }
      getRecDist(nodesRe.tail, marker, sum + x.map(_.distance).getOrElse(0))
    }
  }
  
  def shortestPath(g: WeightedGraph, from: String, to: String, pathMaps: scala.collection.mutable.Map[String, List[DistVector]], initCount: Int = 0, isMax: Boolean): Int = {
		var cost = 0;
    if(initCount > 10 & cost == 0) return -1
    val visited: scala.collection.mutable.Map[String, Boolean] = scala.collection.mutable.Map.empty
    val parent: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map.empty
    val adj: Vector[Int] = Vector(pathMaps.size*2)
    pathMaps.keys.foreach(i =>{ parent(i) = "";} )
    import java.util.Queue
    val que: java.util.Queue[String] = new java.util.LinkedList()
    visited(from) = true;
    que.add(from)
    var path = from
    while(!que.isEmpty) {
      val x = que.poll
      if( x == to ) { path = path + x; return cost + pathMaps.getOrElse(from.take(1), List()).find(f => f.to.label == to).map(_.distance).getOrElse(0); }
      val y = pathMaps.getOrElse(from.take(1), List())
      y.foreach(f => {
        if(!visited.getOrElse(f.to.label, false)) {
          path = path + f.to.label
          visited(f.to.label) = true;
          que.add(f.to.label)
          parent(f.to.label) = x
        }
      })
    }
    if((initCount > 0 && isMax && path.length <= initCount) || (initCount > 0 && !isMax && path.length == initCount)) pathMaps.getOrElse(from.take(1), List()).map(_.distance).toList.min + pathMaps.getOrElse(from.take(1), List()).map(f => shortestPath(g, f.to.label, to, pathMaps, initCount+1, isMax)).filterNot(_ == 0).size
    else if(cost == 0) pathMaps.getOrElse(from.take(1), List()).map(_.distance).toList.min + pathMaps.getOrElse(from.take(1), List()).map(f => shortestPath(g, f.to.label, to, pathMaps, initCount+1, isMax)).filterNot(_ == 0).min
    else cost
  }
  
  def findBelowCount(g: WeightedGraph, from: String, to: String, pathMaps: scala.collection.mutable.Map[String, List[DistVector]], op: String, maxLoops: Int): Int = {
    val count = 1;
    op match {
      case "max-loops" => {
    	  if(from == to) shortestPath(g, from+count, to, pathMaps, maxLoops, true)
    	  else shortestPath(g, from, to, pathMaps, maxLoops, false)
      } case "exact-loops" => {
        if(from == to) shortestPath(g, from+count, to, pathMaps, maxLoops, false)
        else shortestPath(g, from, to, pathMaps, maxLoops, false)
      }
      case _ => {
    	  if(from == to) shortestPath(g, from+count, to, pathMaps, 0, false)
    	  else shortestPath(g, from, to, pathMaps, 0, false)
      }
    }
    
  }
  
  def possiblePaths(g: WeightedGraph, frm: String, to: String, pathMaps: scala.collection.mutable.Map[String, List[DistVector]], op: String, maxDistance: Int = 30, distance:Int = 0): Int = {
    if(frm == to) findAllPaths(g, frm+distance, to, pathMaps)
    else findAllPaths(g, frm, to, pathMaps)
  }
  
  def findAllPaths(g: WeightedGraph, from: String, to: String, pathMaps: scala.collection.mutable.Map[String, List[DistVector]]): Int = {
    var count: Int = 0;
		val paths: List[String] = List[String]()
//    while(count<1) {
      val (dist,path)= shortestGrowingPath(g, from, to, pathMaps, 0)
		  if(paths.contains(path)) count = count+1; else paths +: path
//    }
    count
  }
  
  def getDist(from: String, to: String, pathMaps: scala.collection.mutable.Map[String, List[DistVector]]) = {
    pathMaps.getOrElse(from, List()).find(_.to.label == to).headOption.map(_.distance).getOrElse(0)
  }
  def getPrevFrom(to: String, adjVertices: List[DistVector]) = {
    adjVertices.sortBy(_.distance).headOption.map(_.to.label).getOrElse("")
  }
  def getNextTraversable(to: String, adjVertices: List[DistVector]) = {
    adjVertices.sortBy(_.distance).headOption.map(_.to.label).getOrElse("")
  }
  
  def shortestGrowingPath(g: WeightedGraph, from: String, to: String, pathMaps: scala.collection.mutable.Map[String, List[DistVector]], initCount: Int = 0): (Int, String)= {
		var cost = 0;
    if(initCount > 10 & cost == 0) return (-1,"")
    val visited: scala.collection.mutable.Map[String, Boolean] = scala.collection.mutable.Map.empty
    val parent: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map.empty
    val adj: Vector[Int] = Vector(pathMaps.size*2)
    pathMaps.keys.foreach(i =>{ parent(i) = "";} )
    import java.util.Queue
    val que: java.util.Queue[String] = new java.util.LinkedList()
    visited(from) = true;
    var path = from
    que.add(from)
    while(!que.isEmpty) {
      val x = que.poll
      if( x == to ) { path = path + to; return (cost + pathMaps.getOrElse(from.take(1), List()).find(f => f.to.label == to).map(_.distance).getOrElse(0), path) }
      val y = pathMaps.getOrElse(from.take(1), List())
      y.foreach(f => {
        if(!visited.getOrElse(f.to.label, false)) {
          path = path + f.to.label
          visited(f.to.label) = true;
          que.add(f.to.label)
          parent(f.to.label) = x
        }
      })
    }
    val markedPath: scala.collection.mutable.Map[Int, Boolean] = scala.collection.mutable.Map.empty
    val graphPaths =  pathMaps.getOrElse(from.take(1), List()).map(f => shortestGrowingPath(g, f.to.label, to, pathMaps, initCount+1)._1).filterNot(_ == 0)
    val distance = pickShortNode(graphPaths, markedPath)
    markedPath(distance) = true
    if(cost == 0) (pathMaps.getOrElse(from.take(1), List()).map(_.distance).toList.min + distance, path)
    else (cost, path)
  }
  
  def pickShortNode(list: List[Int], paths: scala.collection.mutable.Map[Int, Boolean]) = {
      paths.filter(!_._2).headOption.map(_._1).getOrElse(0)
  }
  
}
