

import scala.util.Random
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.layout.StackPane
import javafx.stage.Stage
import javafx.scene.layout.Pane
import javafx.scene.shape.Rectangle
import javafx.scene.Group 
import javafx.scene.layout.GridPane
import javafx.geometry.Insets
import javafx.scene.control.Label
import javafx.application.Platform
import javafx.animation.Timeline
import javafx.animation.Animation
import javafx.animation.KeyFrame
import javafx.util.Duration
import javafx.scene.paint.Color
import javafx.geometry.Pos 
import javafx.scene.layout.HBox
import scala.annotation.tailrec
import javafx.scene.input.MouseEvent
import javafx.scene.input.MouseButton
import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import javafx.scene.control.Slider
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue
import javafx.scene.control.Label
import java.io._



class MainWindow extends Application{
    
    //sets board to r-pentomino configuration
    var board = Map[(Int,Int),core.Cell]((91,50) -> core.Cell(false),(90,51) -> core.Cell(false),(91,51) -> core.Cell(false),(91,52) -> core.Cell(false),(92,52) -> core.Cell(false))   
    
    //loads map
    val ois = new ObjectInputStream(new FileInputStream("maps/config"))
    val mapConfig = ois.readObject.asInstanceOf[Array[Array[core.mapCell]]]
    ois.close
    val map = mapConfig
    
    //defines initial timeline for CA to run on
    var runLoop = new Timeline();
    runLoop.setCycleCount(Animation.INDEFINITE)
    var running = false
    
    //JavaFX stuff
    val root = new StackPane
    val scene = new Scene(root,1400,1000) 
    scene.setRoot(root)
    
    //one grid is for map, one is for cells
    val grid = new Pane
    val grid2 = new Pane
    
    //called from main method  
    override def start(stage: Stage) : Unit = {

      stage.setScene(scene)
     
      
      root.getChildren.add(grid2) 
      root.getChildren.add(grid)
      
      
      //menu stuff
      val menu = new HBox()
            
      val playBtn = new Button("Start")
      menu.getChildren.add(playBtn)
      playBtn.setMinSize(200, 50)
      playBtn.setStyle("-fx-font-size: 20pt")
      HBox.setMargin(playBtn, new Insets(20, 20, 0, 20))
      
      val clearBtn = new Button("Clear") 
      menu.getChildren.add(clearBtn)
      clearBtn.setMinSize(200, 50)
      clearBtn.setStyle("-fx-font-size: 20pt")
      HBox.setMargin(clearBtn, new Insets(20, 20, 0, 20))
      
      val nextBtn = new Button("Next")
      menu.getChildren.add(nextBtn)
      nextBtn.setMinSize(200, 50)
      nextBtn.setStyle("-fx-font-size: 20pt")
      HBox.setMargin(nextBtn, new Insets(20, 20, 0, 20))
      
      val speed = new Label("Speed:");
      menu.getChildren.add(speed)
      speed.setStyle("-fx-font-size: 20pt")
      HBox.setMargin(speed, new Insets(20, 0, 0, 20))
      
      val slider = new Slider(0.9,0.99,0.95)
      menu.getChildren.add(slider)
      slider.setMinSize(200, 50)
      slider.setStyle("-fx-font-size: 20pt")
      HBox.setMargin(slider, new Insets(20, 20, 0, 0))
      //when slider is changed reconfigure timeline
      slider.valueProperty().addListener(new ChangeListener[Number]() {
        override def changed(observableValue: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit = {
          setTime((newValue.doubleValue().doubleValue()-1).abs)
        }
      })
      
      
      root.getChildren.add(menu)
     
      //called to draw map. only needed once because it is static
      def drawMap():Unit={
        grid2.getChildren.clear()
        for {
           i <- 0 to core.mapWidth - 1
           j <- 0 to core.mapHeight - 1
        }{ val rect = new Rectangle(7,7)
          if(map(i)(j).t=="l")
              rect.setFill(Color.GREEN)
          else if(map(i)(j).t=="w")
              rect.setFill(Color.AQUA)
          rect.relocate(i*7, j*7)
          grid2.getChildren.add(rect) 
        }
      }
     
     
      
      drawMap()
      drawGrid()
      
      //actions for UI defined above
      playBtn.setOnAction((e: ActionEvent) => {
          if(running){
            running = false
            playBtn.setText("Start")
          }
          else{
            running = true
            playBtn.setText("Stop")
          }          
      })
      clearBtn.setOnAction((e: ActionEvent) => {
        board = board.empty
        drawGrid()
      })
      nextBtn.setOnAction((e: ActionEvent) => {       
          board = core.evolve(board,map)
          drawGrid()
      })

      stage.show()
      
      //handler for mouse input, which creates and removes cells
      val eventHandler: EventHandler[MouseEvent] =
      new EventHandler[MouseEvent]() {
        override def handle(e: MouseEvent): Unit = {
          if(e.getButton() == MouseButton.PRIMARY)
            //create cell without virus
            board = board + ((((e.getX/7).intValue(),(e.getY/7).intValue()),core.Cell(false)))
          else if(e.getButton() == MouseButton.SECONDARY)
            //create cell with virus
            board = board + ((((e.getX/7).intValue(),(e.getY/7).intValue()),core.Cell(true)))        
          else if(e.getButton() == MouseButton.MIDDLE)
            //remove cell
            board = board - (((e.getX/7).intValue(),(e.getY/7).intValue()))
          drawGrid()
        }
      }
      
    
    scene.addEventHandler(MouseEvent.MOUSE_CLICKED, eventHandler)
    scene.addEventHandler(MouseEvent.MOUSE_DRAGGED, eventHandler)

    //set timeline to initial speed 
    setTime((0.95.doubleValue().doubleValue()-1).abs)           
    }
    
    //set how often board updates
    def setTime(seconds:Double):Unit={
       if (runLoop != null) {
         runLoop.stop();
       }
       runLoop = new Timeline(new KeyFrame(Duration.seconds(seconds), new EventHandler[ActionEvent]() {
            @Override
            def handle(event:ActionEvent):Unit={
              //if running call evolve with future so it runs concurrently
              if (running){
                val f = Future {
                  core.evolve(board, map)
                }
                f.onComplete{
                  case Success(value) => board = value; Platform.runLater(() => drawGrid())
                  case Failure(e) => e.printStackTrace         
                }
              }
            }
        }));
        runLoop.setCycleCount(Animation.INDEFINITE);
        runLoop.play();
    }
        
        
      //called to draw grid. used after every evolution of the CA  
      def drawGrid():Unit={
        grid.getChildren.clear()
        board.map(x=>{              
              val rect = new Rectangle(7,7)
              if(x._2.vi)rect.setFill(Color.RED)else rect.setFill(Color.BLACK)
              rect.relocate(x._1._1*7, x._1._2*7)
              grid.getChildren.add(rect)
              }
        )
      }
    
}





object Main {

  def main(args: Array[String]): Unit = {
    //Launches JavaFX application
    Application.launch(classOf[MainWindow], args: _*)
    
  } 
  
}