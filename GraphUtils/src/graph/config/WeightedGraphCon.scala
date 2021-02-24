package graph.config

import graph.config.WeightedDirectedGraphConf.Vertex
import graph.config.WeightedDirectedGraphConf.WeightedGraph
import graph.config.WeightedDirectedGraphConf.WGraph
import graph.config.WeightedDirectedGraphConf.Edge
import practice.test.EvaluateGraph

//package practice.test

//taking input first: so having the below object
object WeightedDirectedGraphConf {
  case class Vertex(label: String)
  //to-vertex is optional here, maintaining some dead-ends as well, even though that is not mentioned in use case for safe side
  case class Edge(from: Vertex, to: Option[Vertex], distance: Int)
  case class WeightedGraph(graph: Map[String, List[Edge]])
  
  implicit class WGraph(g: WeightedGraph) {
    def graphOps(operation: String, nodes: List[Edge]): Option[WeightedGraph] = {
      operation match {
        case "-->" => this --> nodes
        case "debug" => this.print
        //definitely other operations are not in-place, will modify if use cases increased
        case _ => {println("Operation not supported"); None;}
      }
    }
    
    def -->(node: List[Edge]) = {
      var nodes = g.graph("sample")
      Some(WeightedGraph(Map("sample" -> (nodes ++ node))))
    } 
    
    def print() = {
      println(g.graph)
      None
    }
    
    def getGraph = g
  }
}

object WeightedDirectedGraphClient extends App{
  val vertices: List[Vertex] = List.empty
  val initGraph = Map("sample" -> List())
  val edgesConfig = "AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7".split(", ").toList
  def toEdge(vls: String) = {val x = vls.toCharArray.map(_.toString); Edge(Vertex(x(0)), Some(Vertex(x(1))), x(2).toInt)}
  val graph = WGraph(WeightedGraph(initGraph)).graphOps("-->", edgesConfig.map(e => toEdge(e)))//.flatten.map(_.graph("sample")).foreach(f => println(f.map(_.distance)))
  EvaluateGraph.solveMap(graph.get)
}
