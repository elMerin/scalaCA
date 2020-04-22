

object core {
  
  val mapWidth = 200
  val mapHeight = 200

  
  def mapNeighbours(map: Array[Array[mapCell]],c:mapCell) : Array[mapCell] = {
      val x = c.x
      val y = c.y
      Array(map(x-1)(y-1),map(x-1)(y),map(x)(y-1),map(x+1)(y-1),map(x-1)(y+1),map(x+1)(y),map(x)(y+1),map(x+1)(y+1))
  }
  

  def neighbours(c:((Int,Int),Cell)) = c match {
      case ((x,y),cell) => 
      for( n <- Map( ((x-1,y-1),cell),((x-1,y),cell),((x,y-1),cell),((x+1,y-1),cell),((x-1,y+1),cell),((x+1,y),cell),((x,y+1),cell),((x+1,y+1),cell))) yield n 
  }
  
  
  
  def evolve(board: Map[(Int,Int),Cell], map: Array[Array[mapCell]]): Map[(Int,Int),Cell] = {
    
    
      val cells = board flatMap neighbours
      
      val r = scala.util.Random
            
      def boardFilter(c:((Int,Int),Cell)):Boolean ={
        if(c._1._1 > mapWidth-2 || c._1._2-2 > mapHeight || c._1._1 < 1 || c._1._2 < 1) return false    
        val nb = neighbours(c) collect {case cell:((Int,Int),Cell) if(board.contains(cell._1)) => ((cell._1),board(cell._1))}
        val waterNb = mapNeighbours(map,map(c._1._1)(c._1._2)).filter(_.t == "w" ) 
        val count = nb.size
        val waterCount = waterNb.size
        val aquatic = r.nextInt(2)
        val vEffect = r.nextInt(1000)       

        if(board.contains(c._1))
          if (c._2.vi)
            if (vEffect > 900) return false
        
        
        if(waterCount > 0 && map(c._1._1)(c._1._2).t == "l"){
          if(count<5 && (count > 2 || (count == 2 && board.contains(c._1) ))){
            true
          }
          else false
        }
        else if(map(c._1._1)(c._1._2).t == "w"){
          if(aquatic == 1){
            if(count == 2 || (count==1 && board.contains(c._1) )){
              true
            }
            else
              false
          }
          else
            false
        }
        else{
          if(count == 3 || (count==2 && board.contains(c._1) )){
            if(map(c._1._1)(c._1._2).t == "l")return true
            else false
          }
          else false
        }
            
      }
     
     

      val newBoard = cells filter boardFilter 
      
      def vMap(c:((Int,Int),Cell)):((Int,Int),Cell) ={
         val nb = neighbours(c) collect {case cell:((Int,Int),Cell) if(newBoard.contains(cell._1)) => ((cell._1),newBoard(cell._1))}
         val vNb = nb filter {(cell:((Int,Int),Cell))=>cell._2.vi}
         val vCount = vNb.size
         val effect = r.nextInt(1000)    
         
          if (vCount > 0 || newBoard(c._1).vi)
            if (effect < 50) (c._1,Cell(false))
            else (c._1,Cell(true))
          else
            (c._1,Cell(false))     
      }
      newBoard map vMap
      
  }
  
  
  val printBoard = (board:Map[(Int,Int),Cell]) => {
    println(board)
    board
  }
  
  
  val population = (board:Map[(Int,Int),Cell]) => {
    println(board.size)
  }
  
  
  val printInfo = population compose printBoard
  
  
  

case class Cell(vi:Boolean)


case class mapCell(t:String, x:Int, y:Int) extends Serializable

} 