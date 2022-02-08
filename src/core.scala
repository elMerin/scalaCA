

object core {
  
  //used to configure size of map- do not change
  val mapWidth = 200
  val mapHeight = 200

  //get neighbours of a cell in the map array
  def mapNeighbours(map: Array[Array[mapCell]],c:mapCell) : Array[mapCell] = {
      val x = c.x
      val y = c.y
      Array(map(x-1)(y-1),map(x-1)(y),map(x)(y-1),map(x+1)(y-1),map(x-1)(y+1),map(x+1)(y),map(x)(y+1),map(x+1)(y+1))
  }
  
  //get neighbours of a cell in the board
  def neighbours(c:((Int,Int),Cell)) = c match {
      case ((x,y),cell) => 
      for( n <- Map( ((x-1,y-1),cell),((x-1,y),cell),((x,y-1),cell),((x+1,y-1),cell),((x-1,y+1),cell),((x+1,y),cell),((x,y+1),cell),((x+1,y+1),cell))) yield n 
  }
  
  
  //returns the next generation of the CA
  def evolve(board: Map[(Int,Int),Cell], map: Array[Array[mapCell]]): Map[(Int,Int),Cell] = {
    
      //area of potential cells to consider
      val cells = board flatMap neighbours
      
      val r = scala.util.Random
      
      //filters the cells and decides which ones live/die
      def boardFilter(c:((Int,Int),Cell)):Boolean ={
        if(c._1._1 > mapWidth-2 || c._1._2-2 > mapHeight || c._1._1 < 1 || c._1._2 < 1) return false //avoid cell outside bounds  
        //lazy val nb = neighbours(c) filter {(cell:((Int,Int),Cell))=>board.contains(cell._1)} map {(cell:((Int,Int),Cell)) => board(cell._1)}
        lazy val nb = neighbours(c) collect {case cell:((Int,Int),Cell) if(board.contains(cell._1)) => ((cell._1),board(cell._1))}
        lazy val waterNb = mapNeighbours(map,map(c._1._1)(c._1._2)).filter(_.t == "w" ) //get number of neighbouring water cells
        lazy val count = nb.size
        lazy val waterCount = waterNb.size
        lazy val aquatic = r.nextInt(2) //decides whether cell can survive on water
        lazy val vEffect = r.nextInt(1000) //random val to determine if cell dies from virus       

        if(board.contains(c._1))
          if (c._2.vi)
            if (vEffect > 900) return false    //if cell has virus it can die
        
        if(waterCount > 0 && map(c._1._1)(c._1._2).t == "l"){  //if the cell is on shore
          if(count<5 && (count > 2 || (count == 2 && board.contains(c._1) ))){
            true
          }
          else false
        }
        else if(map(c._1._1)(c._1._2).t == "w"){  //if cell is on water
          if(aquatic == 1){
            if(count == 2 || (count==1 && board.contains(c._1) )){
              true
            }  else false
          }  else false
        }
        else{  //cell is landlocked
          if(count == 3 || (count==2 && board.contains(c._1) )){
            if(map(c._1._1)(c._1._2).t == "l")return true
            else false
          }
          else false
        }
            
      }
     
     
      //returns cells that live/die
      val newBoard = cells filter boardFilter 
      
      //function to decide if the cells contract virus
      def vMap(c:((Int,Int),Cell)):((Int,Int),Cell) ={
         val nb = neighbours(c) collect {case cell:((Int,Int),Cell) if(newBoard.contains(cell._1)) => ((cell._1),newBoard(cell._1))}
         val vNb = nb filter {(cell:((Int,Int),Cell))=>cell._2.vi}
         val vCount = vNb.size
         val effect = r.nextInt(1000)    
         
          if (vCount > 0 || newBoard(c._1).vi)
            if (effect < 10) (c._1,Cell(false))
            else (c._1,Cell(true))
          else
            (c._1,Cell(false))     
      }
      //applies vMap and returns result
      newBoard map vMap
      
  }

case class Cell(vi:Boolean)  //cell that's part of the CA
case class mapCell(t:String, x:Int, y:Int) extends Serializable  //cell of the map. t denotes type, x and y denote coordinates

} 