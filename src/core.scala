import scala.collection.immutable.HashMap

object core {
  
  val mapWidth = 200
  val mapHeight = 200

  
  def mapNeighbours(map: Array[Array[mapCell]],c:mapCell) : Array[mapCell] = {
      val x = c.coords._1
      val y = c.coords._2
      Array(map(x-1)(y-1),map(x-1)(y),map(x)(y-1),map(x+1)(y-1),map(x-1)(y+1),map(x+1)(y),map(x)(y+1),map(x+1)(y+1))
  }
  

  def neighbours(c:((Int,Int),Cell)) = c match {
      case ((x,y),v) => 
      for( n <- HashMap( ((x-1,y-1),v),((x-1,y),v),((x,y-1),v),((x+1,y-1),v),((x-1,y+1),v),((x+1,y),v),((x,y+1),v),((x+1,y+1),v))) yield n 

  }
  
  
  
  def evolve(board: HashMap[(Int,Int),Cell], map: Array[Array[mapCell]]): HashMap[(Int,Int),Cell] = {
    
    
      val cells = board flatMap neighbours
      
      val r = scala.util.Random
            
      def boardFilter(c:((Int,Int),Cell)):Boolean ={
        if(c._1._1 > mapWidth-2 || c._1._2-2 > mapHeight || c._1._1 < 1 || c._1._2 < 1) return false    
        val nbP = neighbours(c) filter {(cell:((Int,Int),Cell))=>board.contains(cell._1)}
        val nb = nbP map ((c:((Int,Int),Cell))=> if(board(c._1).vi==c._2.vi) c else ((c._1),Cell(board(c._1).vi)))
        val waterNb = mapNeighbours(map,map(c._1._1)(c._1._2)).filter(_.tp == "w" ) 
        val vNb = nb filter {(cell:((Int,Int),Cell))=>cell._2.vi}
        val count = nb.size
        val waterCount = waterNb.size
        val vCount = vNb.size
        val aquatic = r.nextInt(2)
        val effect = r.nextInt(1000)
        
        if (vCount > 0)
          if(effect>950) 
            return false 
          else if(effect<1) 
            c._2.vi=false 
          else 
            c._2.vi=true  
        
                          
        if(waterCount > 0 && map(c._1._1)(c._1._2).tp == "l"){
          if(count<5 && (count > 2 || (count == 2 && board.contains(c._1) ))){
            true
          }
          else false
        }
        else if(map(c._1._1)(c._1._2).tp == "w"){
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
            if(map(c._1._1)(c._1._2).tp == "l")return true
            else false
          }
          else false
        }
            
      }
     

      return cells filter boardFilter 
  }
  
  
  val printBoard = (board:HashMap[(Int,Int),Cell]) => {
    println(board)
    board
  }
  
  
  val population = (board:HashMap[(Int,Int),Cell]) => {
    println(board.size)
  }
  
  
  val printInfo = population compose printBoard
  
  
  

case class Cell(var vi:Boolean)


case class mapCell(t:String, x:Int, y:Int) extends Serializable{

  val tp = t
  val coords = (x,y) 

}

} 