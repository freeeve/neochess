package com.skeweredrook.neochess

import scala.math.abs
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import java.util.Date
import ictk.boardgame.chess._
import ictk.boardgame.chess.io._
import ictk.boardgame.chess.ui.cli.TxChessBoardDisplay
import scala.collection.mutable.Map
import org.anormcypher._

package object Analyze {
val san = new SAN
val fen = new FEN
val stockfish = new Stockfish(1024, 100)

var bestFen: String = null
var lastBestFen: String = null
var maxDepth = 5; //stockfish maxDepth
var dbDepth = 2;
val maxThreshhold = 999.0
val startingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

Neo4jREST.setServer("neo4j1.skeweredrook.com", 7474, "/db/data/","","")

def getNodeByFEN(FEN:String) =
  Cypher(
    """
      start n=node:node_auto_index(fen="{fen}")
      return n.fen
    """
  ).on("fen" -> FEN)().map(row =>
    row[String]("n.fen")
  ).toList

def createPosition(FEN:String, score:Double) = 
  Cypher(
    """
      start pos=node:node_auto_index(pos={pos})
      with count(pos) as existspos
      where existspos = 0
      create pos={pos:{pos}}
      with count(1) as dummy
      start pos=node:node_auto_index(pos={pos})
      create unique (pos)<-[:position]-(fen {fen:{fen}})
      set fen.score={score}
    """
  ).on("pos"->getPositionFEN(FEN), "fen" -> FEN, "score"->score).execute()

def createMove(startFEN:String, move:String, endFEN:String, score:Double, isDrawn:Option[Boolean]) = 
  Cypher(
    """
      start s=node:node_auto_index(fen={startFEN}), end=node:node_auto_index(fen={endFEN})
      match end-[:position]->endpos
      where not(s<-[*]-()-[:position]->endpos)
      create unique s-[m:move]->end
      set m.score = {score}, end.score = {score}
    """
  ).on("startFEN"->startFEN, "move"->move, "endFEN"->endFEN, "score"->score).execute()

def getNextPosition(baseFEN:String) =
  Cypher(
    """
      start n=node:node_auto_index(fen={fen})
      match n-[:move*0..2]->m
      where not(m-[:move]->())
      and abs(m.score) < 1000
      return m.fen
      limit 100
    """
  ).on("fen"->baseFEN)().map(row =>
    row[String]("m.fen")
  ).toList

def getChildrenMoves(baseFEN:String) =
  Cypher(
    """
      start n=node:node_auto_index(fen={fen})
      match n-[:move]->m
      return m.fen
    """
  ).on("fen"->baseFEN, "depth"->dbDepth)().map(row =>
    row[String]("m.fen")
  ).toList

def main(args: Array[String]): Unit = {
  // create base node if none exists
  val base = getNodeByFEN(startingFEN)
  if(base.size == 0) {
    createPosition(startingFEN, 0.0);
  }
  var baseFEN = startingFEN;
  var depth = 0
  val stack = collection.mutable.Stack((baseFEN,depth))

  stockfish.start()
  while (true) {
    // get next position to analyze
    // need to make sure next position is not a repeat
    var startTime = System.currentTimeMillis
    var unanalyzed = getNextPosition(baseFEN)
    var time = System.currentTimeMillis - startTime
    println("gotNextPosition in " + time + "ms")
    if(unanalyzed.size == 0) {
      if(stack.size == 0) {
        dbDepth += 1
        println("new dbDepth: "+dbDepth)
        baseFEN = startingFEN
        depth = 0
        stack.push((baseFEN, depth))
      } else {
        stack.pop match {
          case (f:String, d:Int) => {
            baseFEN = f
            depth = d
          }
        }
        println("new baseFEN: " + baseFEN + "; depth: " + depth)
      
        if(depth < dbDepth - 2) {
          for(m <- getChildrenMoves(baseFEN)) stack.push((m,depth+1))
        } 
      }
    }
    println("analyzing: " + unanalyzed)
    for (FEN <- unanalyzed) {
      startTime = System.currentTimeMillis
      println("analyzing to depth " + maxDepth + " cur best: " + 42 + ": " + FEN);
      val nextMoves = analyzeNextMoves(FEN)
      time = System.currentTimeMillis - startTime
      if(nextMoves.size > 0) println("analyzed in " + time + "ms; best score: " + nextMoves.head._2 + ".");
      startTime = System.currentTimeMillis
      if(nextMoves.size > 0) {
        for((k,(p,s)) <- nextMoves) {
          //println("creating child: " + p + "; score: " + s);
          createPosition(p,s)
          createMove(FEN, k, p, s, isDrawn(p))
        }
      }
      time = System.currentTimeMillis - startTime
      println("created children in " + time + "ms; "+nextMoves.size)
    }
  }
  stockfish.stop();
}

def isDrawn(FEN:String):Option[Boolean] = {
  val board = fen.stringToBoard(FEN).asInstanceOf[ChessBoard]
  if (board.getUnCapturedPieces(false).size + board.getUnCapturedPieces(true).size == 3) {
    for(piece <- board.getUnCapturedPieces(board.getUnCapturedPieces(true).size == 2)) {
      if(piece.isBishop() || piece.isKnight()) { 
        println("only bishop or knight left. drawn");
        return Some(true)
      }
    }
  }
  if (board.getUnCapturedPieces(false).size + board.getUnCapturedPieces(true).size > 2 && !board.isStalemate() && !board.is50MoveRuleApplicible) {
      None
  } else {
      Some(true)
  }
}

def analyzeNextMoves(curFEN: String):Map[String,(String,Double)] = {
  var lastDepth = 0;
  var nextMoves = Map[String,(String,Double)]()
  stockfish.setFEN(curFEN)
  stockfish.goToDepth(maxDepth)
  while (true) {
    val line = stockfish.readLine
    if (line.startsWith("info") && line.contains("seldepth") && !line.contains("bound")) {
      val split = line.split(" ");
      val depth = split(2).toInt;
      if (depth > lastDepth) {
        lastDepth = depth; print("D" + depth + " ");
      }
      if (depth >= lastDepth) {
        val score =
          if (split(6) == "mate") {
            10000.0 * split(7).toInt / abs(split(7).toInt) - split(7).toInt;
          } else {
            split(7).toInt / 100.0;
          }
        val board: ChessBoard = fen.stringToBoard(curFEN).asInstanceOf[ChessBoard]
        val chessMove = algToCM(board, split(17));
        val moveStr = san.moveToString(chessMove);
        board.playMove(chessMove);
        nextMoves += moveStr -> Tuple2(fen.boardToString(board), score)
      }
    } else if (line.startsWith("bestmove")) {
      return nextMoves
    }
  }
  return null;
}

/* gets an array of SAN move strings from a list of algebraic coord strings */
def getMoveList(move: String, moves: Array[String], fenStr: String) = {
  val b = fen.stringToBoard(fenStr).asInstanceOf[ChessBoard]
  b.playMove(algToCM(b, move));
  for (m <- moves) yield {
    val cm = algToCM(b, m);
    b.playMove(cm)
    san.moveToString(cm);
  }
}

/* convert an algebraic coordinate move to a ChessMove */
def algToCM(b: ChessBoard, moveStr: String): ChessMove = {
  if (moveStr.length == 4) {
    new ChessMove(b,
      san.fileToNum(moveStr(0)),
      san.rankToNum(moveStr(1)),
      san.fileToNum(moveStr(2)),
      san.rankToNum(moveStr(3)));
  } else {
    // handle promotions
    val pieceIdx = moveStr.substring(4, 5) match {
      case "q" => Queen.INDEX;
      case "n" => Knight.INDEX;
      case "b" => Bishop.INDEX;
      case "r" => Rook.INDEX;
    }
    new ChessMove(b,
      san.fileToNum(moveStr(0)),
      san.rankToNum(moveStr(1)),
      san.fileToNum(moveStr(2)),
      san.rankToNum(moveStr(3)),
      pieceIdx);
  }
}

def getPositionFEN(fen: String) = {
  val fenSplit = fen.split(" ");
  var res = "";
  for (i <- 0 to 3) {
    res += fenSplit(i) + " ";
  }
  res.substring(0, res.length - 1);
}

}
