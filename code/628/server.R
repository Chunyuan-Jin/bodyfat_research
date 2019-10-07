library(shiny)
library(shinybulma)
library(grid)
library(png)
library(bs4Dash)
library(rsconnect)
library(ggplot2)
library(shinyalert)
img <- readPNG("IMG_0266.png")
g <- rasterGrob(img, interpolate=TRUE)
  server = function(input, output,session) {
    
    output$bigPlot <-renderPlot({
      img <- readPNG("IMG_0266.png")
      g <- rasterGrob(img, interpolate=TRUE)
      ggplot(NULL,aes(0, 0))+geom_point(stat="identity",color="white")+
      annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+scale_y_continuous(limits=c(0, 10), breaks=NULL)+
      scale_x_continuous(limits=c(0, 10), breaks=NULL)+
      annotate("text",x=7,y=2.6,label=paste(input$abdomen,"cm"),color="steelblue",size=7)+
      annotate("text",x=2,y=4.5,label=paste(input$weight,"lbs"),color="steelblue",size=7)+
      annotate("text",x=5,y=0,label=paste("Your body fat percentage is",-40.77528+0.91797*input$abdomen-0.14060*input$weight,"%."),color="black",
               size=6)+theme_minimal()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                                             axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
    })
    observeEvent(input$abdomen, {
        feedbackDanger(
        inputId = "abdomen",
        condition = (input$abdomen <= 50)|(input$abdomen >= 170),
        text = "Please enter a normal abdomen"
      )
    })
    observeEvent(input$weight, {
      feedbackDanger(
        inputId = "weight",
        condition = (input$weight <= 80)|(input$weight >= 400),
        text = "Please enter a normal weight."
      )
    })
    observeEvent({
      input$weight
      input$abdomen}, {
        if((-40.77528+0.91797*input$abdomen-0.14060*input$weight)<0 |(-40.77528+0.91797*input$abdomen-0.14060*input$weight)>100){
        showSnackbar("dangerSnackbar",autoHideDuration=3000)
        }
    })
    observeEvent(input$detail, {
      # Show a modal when the button is pressed
      if((-40.77528+0.91797*input$abdomen-0.14060*input$weight)>0){
      if((-40.77528+0.91797*input$abdomen-0.14060*input$weight)<5){
      shinyalert(paste0("Body fat: ",-40.77528+0.91797*input$abdomen-0.14060*input$weight," %"),
                 "Your body fat ratio is very low. Fat is an important part of cells and nervous system, excerssive reduction of body fat can cause endocrine disorders.", type = "warning")
        }else if((-40.77528+0.91797*input$abdomen-0.14060*input$weight)<10){
          shinyalert(paste0("Body fat: ",-40.77528+0.91797*input$abdomen-0.14060*input$weight," %"),
                     "You are reaching the body fat level same as athletic. Fat is an important part of cells and nervous system, excerssive reduction of body fat can cause endocrine disorders.", type = "warning")
                   f
      } else if((-40.77528+0.91797*input$abdomen-0.14060*input$weight)<14){
        shinyalert(paste0("Body fat: ",-40.77528+0.91797*input$abdomen-0.14060*input$weight," %"),
                   "Your body fat ratio is good. Please keep your good behavior.", type = "success")         
                 }
      else if((-40.77528+0.91797*input$abdomen-0.14060*input$weight)<20){
        shinyalert(paste0("Body fat: ",-40.77528+0.91797*input$abdomen-0.14060*input$weight," %"),
                   "Your body fat ratio is acceptable. Please change the unhealthy diet and do more excercise", type = "success")
      }else if((-40.77528+0.91797*input$abdomen-0.14060*input$weight)<24){
        shinyalert(paste0("Body fat: ",-40.77528+0.91797*input$abdomen-0.14060*input$weight," %"),
                   "You are overweight. Please change the unhealthy diet immediiately and do more excercise now!!!", type = "warning")
      }else if((-40.77528+0.91797*input$abdomen-0.14060*input$weight)<100){
        shinyalert(paste0("Body fat: ",-40.77528+0.91797*input$abdomen-0.14060*input$weight," %"),
                   "Your body fat ratio is high. Please change the unhealthy diet immediiately and do more excercise now!!!", type = "warning")
      }else{
        shinyalert(paste0("Body fat: ",-40.77528+0.91797*input$abdomen-0.14060*input$weight," %"),
                   "Please re-enter the numbers and check again.", type = "error")
      }
        }else{
          shinyalert(paste0("Body fat: ",-40.77528+0.91797*input$abdomen-0.14060*input$weight," %"),
                     "Please re-enter the numbers and check again.", type = "error")
      }
    })
    
  }