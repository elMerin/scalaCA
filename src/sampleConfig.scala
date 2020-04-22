

object sampleConfig {
  
  /*
  var blinker = buildBoard(List((10,8),(10,9),(10,10)))
  var square = buildBoard(List((5,5),(6,5),(6,6),(5,6)))
  var gosperGlider = buildBoard(List((5,5),(6,5),(6,6),(5,6)))
  var glider = buildBoard(List((5,5),(6,6),(7,6),(6,7),(7,5)))*/
  val rpentomino = Map[(Int,Int),core.Cell]((91,50) -> core.Cell(false),(90,51) -> core.Cell(false),(91,51) -> core.Cell(false),(91,52) -> core.Cell(false),(92,52) -> core.Cell(false))   

}