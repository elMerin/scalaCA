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
import scala.collection.immutable.HashSet
import javafx.geometry.Pos 
import javafx.scene.layout.HBox
import scala.annotation.tailrec
import javafx.scene.input.MouseEvent
import javafx.scene.input.MouseButton
import java.io._



class MainWindow extends Application{
  
  
    override def start(stage: Stage) : Unit = {
      var board = sampleConfig.rpentomino
      
      val ois = new ObjectInputStream(new FileInputStream("maps/config"))
      val mapConfig = ois.readObject.asInstanceOf[Array[Array[core.mapCell]]]
      ois.close
      val map = mapConfig
      
      /*
      for {
           i <- 0 to 199
           j <- 0 to 199
       }{
         map(i)(j) = core.mapCell("w")
       }
       */
      
      val root = new StackPane
      val scene = new Scene(root,1920,1080) 
      scene.setRoot(root)
      stage.setScene(scene)
      //stage.show()
     
      
      val grid = new Pane
      val grid2 = new Pane

      
      root.getChildren.add(grid2) 
      root.getChildren.add(grid)
      
      
      val menu = new HBox()
            
      val playBtn = new Button("Start"); 
      menu.getChildren.add(playBtn);
      playBtn.setMinSize(200, 50)
      playBtn.setStyle("-fx-font-size: 20pt")
      HBox.setMargin(playBtn, new Insets(20, 20, 0, 20))
      
      val clearBtn = new Button("Clear"); 
      menu.getChildren.add(clearBtn);
      clearBtn.setMinSize(200, 50)
      clearBtn.setStyle("-fx-font-size: 20pt")
      HBox.setMargin(clearBtn, new Insets(20, 20, 0, 20))
      
      val nextBtn = new Button("Next"); 
      menu.getChildren.add(nextBtn);
      nextBtn.setMinSize(200, 50)
      nextBtn.setStyle("-fx-font-size: 20pt")
      HBox.setMargin(nextBtn, new Insets(20, 20, 0, 20))
     
      
      
      root.getChildren.add(menu)
      
      val runLoop = new Timeline();
      runLoop.setCycleCount(Animation.INDEFINITE)
      
      val startTime = System.currentTimeMillis();

      var running = false
     
     
      
      
      def drawMap(){
        grid2.getChildren.clear()
        for {
           i <- 0 to core.mapWidth - 1
           j <- 0 to core.mapHeight - 1
        }{ val rect = new Rectangle(7,7)
          if(map(i)(j).t=="l")
              rect.setFill(Color.GREEN)
          else if(map(i)(j).t=="w")
              rect.setFill(Color.BLUE)
          rect.relocate(i*7, j*7)
          grid2.getChildren.add(rect) 
        }
      }
      
      def drawGrid(){
        grid.getChildren.clear()
        board.map(x=>{              
              val rect = new Rectangle(7,7)
              if(x._2.vi)rect.setFill(Color.RED)else rect.setFill(Color.BLACK)
              rect.relocate(x._1._1*7, x._1._2*7)
              grid.getChildren.add(rect)
              }
        )
      }
      
      
      val kf: KeyFrame = new KeyFrame(Duration.seconds(0.05),
      //val kf: KeyFrame = new KeyFrame(Duration.seconds(0.1 ),
        new EventHandler[ActionEvent]() {
          def handle(ae: ActionEvent): Unit = {
            board = core.evolve(board,map)
            drawGrid()
          }
        }
      )
      
      drawMap()
      drawGrid()
      playBtn.setOnAction((e: ActionEvent) => {
          if(running){
            running = false
            playBtn.setText("Start")
            runLoop.stop()
          }
          else{
            running = true
            playBtn.setText("Stop")
            runLoop.play()
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
      runLoop.getKeyFrames.add(kf)
      stage.show()
      
    
      val eventHandler: EventHandler[MouseEvent] =
      new EventHandler[MouseEvent]() {
        override def handle(e: MouseEvent): Unit = {
          if(e.getButton() == MouseButton.PRIMARY)
            board = board + ((((e.getX/7).intValue(),(e.getY/7).intValue()),core.Cell(false)))
          else if(e.getButton() == MouseButton.SECONDARY)
            board = board + ((((e.getX/7).intValue(),(e.getY/7).intValue()),core.Cell(true)))        
          else if(e.getButton() == MouseButton.MIDDLE)
            board = board - (((e.getX/7).intValue(),(e.getY/7).intValue()))
          drawGrid()
        }
      }
    scene.addEventFilter(MouseEvent.MOUSE_CLICKED, eventHandler)
    scene.addEventFilter(MouseEvent.MOUSE_DRAGGED, eventHandler)

           
    }
    
    
}



object Main {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[MainWindow], args: _*)

    
  }
  
  
  
  
}