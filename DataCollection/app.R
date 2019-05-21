library(shiny)
library(shinyjs)
library(DT)
library(imager)
library(spatstat)
library(sp)
library(plotKML)
library(maptools)
library(raster)
library(shinythemes)
library(magick)
library(shinyFiles)
library(qrcode)
library(reticulate)
library(openxlsx)
use_python("C:\\Program Files\\Python36", required = T)


#UI Structure
ui <- fluidPage(theme = shinytheme("yeti"),
    tabsetPanel(
    
      #Begin Survey 1 Tab
      tabPanel("Survey 1", 
               
               titlePanel("CSAFE Handwriting Data Collection (Survey 1)"),
               
               fluidRow(
                 column(3, textInput("surveyInitials", h3("1. Initials"), value = NULL)),
                 column(3, textInput("surveyLocation", h3("2. Current Location"), value = NULL)),
                 column(3, dateInput("surveyDate",h3("3 . Date"),value = "2014-01-01")),
                 column(3, selectInput("surveyTime", h3("4. Time"), choices = list("a. Early Morning" = "a. Early Morning", "b. Late Morning" = "b. Late Morning","c. Early Afternoon" = "c. Early Afternoon", "d. Late Afternoon" = "d. Late Afternoon", "e. Early Evening" = "e. Early Evening" ,"f. Late Evening" = "f. Late Evening"), selected = 1))
               ),
               
               fluidRow(
                 column(3, textInput("surveyThirdGrade", h3("5. 3rd Grade Edu."), value = NULL)),
                 column(3, selectInput("surveyAge", h3("6. Age"), choices = list("a. 18-24" = "a. 18-24", "b. 25-40" = "b. 25-40","c. 41-60" = "c. 41-60", "d. 61+" = "d. 61+"), selected = 1)),
                 column(3, selectInput("surveyLanguage", h3("7. Language"), choices = list("a. Yes" = "a. Yes", "b. No" = "b. No"), selected = 1)),
                 column(3, selectInput("surveyGender", h3("8. Gender"), choices = list("a. Female" = "a. Female", "b. Male" = "b. Male", "c. Other" = "c. Other"), selected = 1)),
                 column(3, textInput("surveyOtherGender", h3("8(c.) Other Gender"), value = NULL))
               ),
               
               fluidRow(
                 column(3, selectInput("surveyEthnicity", h3("9. Ethnicity "), choices = list("a. African American" = "a. African American", "b. Asian" = "b. Asian","c. Caucasian" = "c. Caucasian", "d. Hispanic" = "d. Hispanic", "e. Native American" = "e. Native American" ,"f. South Pacific" = "f. South Pacific", "g. Other" = "g. Other"), selected = 1)),
                 column(3, selectInput("surveyEducation", h3("10. Formal Edu."), choices = list("a. High school or less" = "a. High school or less", "b. More than high school" = "b. More than high school"), selected = 1)),
                 column(3, selectInput("surveyHand", h3("11. Dominant Hand"), choices = list("a. Left" = "a. Left", "b. Right" = "b. Right", "c. Ambidextrous" = "c. Ambidextrous"), selected = 1))
                 
               ),
               
               fluidRow(column(3, textInput("surveyFile", h3("File Save Name"), value = NULL))),
               
               fluidRow(column(3, actionButton("action", label = "SAVE"))),
               
               mainPanel( img(src = "csafe_logo.jpg", height = 200, width = 300) )),
               #End Survey 1 Tab
      
      #Begin Survey 2 Tab
      tabPanel("Survey 2", 
               
              titlePanel("CSAFE Handwriting Data Collection (Survey 2)"),       
               
              fluidRow(
                  column(3, textInput("survey2Initials", h3("1. Initials"), value = NULL)),
                  column(3, textInput("survey2Location", h3("2. Current Location"), value = NULL)),
                  column(3, dateInput("survey2Date",h3("3 . Date"),value = "2014-01-01")),
                  
                  column(3, selectInput("survey2Time", h3("4. Time"), choices = list("a. Early Morning" = "a. Early Morning", "b. Late Morning" = "b. Late Morning","c. Early Afternoon" = "c. Early Afternoon", "d. Late Afternoon" = "d. Late Afternoon", "e. Early Evening" = "e. Early Evening" ,"f. Late Evening" = "f. Late Evening"), selected = 1))
              ),
               
               fluidRow(column(3, textInput("survey2File", h3("File Save Name"), value = NULL))),
               
               fluidRow(column(3, actionButton("action2", label = "SAVE"))),
               
               mainPanel( img(src = "csafe_logo.jpg", height = 200, width = 300))
              ),
              #End Survey 2 Tab
      
      #Begin Survey 3 Tab
      tabPanel("Survey 3",
               
               titlePanel("CSAFE Handwriting Data Collection (Survey 3)"),       
               
               fluidRow(
                 column(3, textInput("survey3Initials", h3("1. Initials"), value = NULL)),
                 column(3, textInput("survey3Location", h3("2. Current Location"), value = NULL)),
                 column(3, dateInput("survey3Date",h3("3 . Date"),value = "2014-01-01")),
                 column(3, selectInput("survey3Time", h3("4. Time"), choices = list("a. Early Morning" = "a. Early Morning", "b. Late Morning" = "b. Late Morning","c. Early Afternoon" = "c. Early Afternoon", "d. Late Afternoon" = "d. Late Afternoon", "e. Early Evening" = "e. Early Evening" ,"f. Late Evening" = "f. Late Evening"), selected = 1))
               ),
               
               fluidRow(column(3, textInput("survey3File", h3("File Save Name"), value = NULL))),
               
               fluidRow(column(3, actionButton("action3", label = "SAVE"))),
               
               mainPanel( img(src = "csafe_logo.jpg", height = 200, width = 300))
               
               ),
               #End Survey 3
      
      #Begin Image Viewer/Cropper tab
      tabPanel("Image Viewer/Cropper",
               
               fluidPage(
                 titlePanel("View/Crop Image"),
                 img(src = "csafe_logo.jpg", height = 200, width = 300),
                 useShinyjs(),
                 sidebarLayout(
                   sidebarPanel(
                     fileInput("file1", "Choose Image",accept = c(".png",".jpg")),
                     textInput("surveyFilePath", h3("Survey File Path"), value = NULL),
                     actionButton("surveyFileVal", label = "Survey"),
                     actionButton("cropSaveSample", label = "Prompt"),
                     width = 4),
                   
                   mainPanel(
                     plotOutput("plot1", click="plot1_click",
                                dblclick = "plot1_dblclick",
                                brush = brushOpts(
                                  id = "plot1_brush",
                                  resetOnNew = TRUE
                                ))
                            )
                           )
                        )
      )
    )
    #End tabset panel
)


#Writes survey data to file specified by File Name text input
server <- function(input, output, session) {
  
  #Height and Width variables for dyanmically manipulating image size in Image Viewer/Cropper
  h <- 845
  w <- 745
  
  #Saving Survey 1 data to a .csv file
  observeEvent(input$action, {
    
    d <- data.frame( WID = substr(input$surveyFile, 7, 10),
                    Intitials = input$surveyInitials, 
                    Location = input$surveyLocation,
                    Date = input$surveyDate,
                    Time = input$surveyTime,
                    ThirdGradeLoc = input$surveyThirdGrade,
                    Age = input$surveyAge,
                    Language = input$surveyLanguage,
                    Gender = input$surveyGender,
                    Other = input$surveyOtherGender,
                    Ethnicity = input$surveyEthnicity,
                    Edu = input$surveyEducation,
                    Hand = input$surveyHand)
    
    currDir <- getwd()
    
    if (dir.exists(substr(input$surveyFile, 0, 5))){
      setwd(file.path(getwd(), substr(input$surveyFile, 0, 5)))
    }else{
      dir.create(file.path(substr(input$surveyFile, 0, 5)), recursive = TRUE)
      setwd(file.path(getwd(), substr(input$surveyFile, 0, 5)))
    }

    v <- substr(input$surveyFile, 7, 10)
    o <- as.numeric(v)+1
    wb <- loadWorkbook("S1.xlsx")
    writeData(wb, sheet = "Sheet1", startRow = o, d, colNames = F)
    saveWorkbook(wb,"S1.xlsx",overwrite = T)
    setwd(file.path(currDir))
    
    if (dir.exists(substr(input$surveyFile, 0, nchar(as.character(input$surveyFile)) - 8))){
      setwd(file.path(getwd(), substr(input$surveyFile, 0, nchar(as.character(input$surveyFile)) - 8)))
    } else {
      dir.create(file.path(getwd(), substr(input$surveyFile, 0, nchar(as.character(input$surveyFile)) - 8)), recursive = TRUE)
      setwd(file.path(getwd(), substr(input$surveyFile, 0, nchar(as.character(input$surveyFile)) - 8)))
      
    }

    write.csv(d, file = paste("w", substr(input$surveyFile, 7, 10), "_", "s", substr(input$surveyFile, 12, 13), "_", "S1", ".csv",  sep = ""),row.names=FALSE,col.names=FALSE)
    showNotification("Data Saved!", action = a(href = "javascript:location.reload();", "Reload page"))
    setwd(file.path(currDir))
    
  })
  
  #Saving Survey 2 data to a .csv file
  observeEvent(input$action2, {
    
    d2 <- data.frame(Initials = input$survey2Initials, 
                    Current_Location = input$survey2Location,
                    Date = input$survey2Date,
                    Time = input$survey2Time)
    
    currDir <- getwd()
    
    
    if (dir.exists(substr(input$survey2File, 0, 5))){
      setwd(file.path(getwd(), substr(input$survey2File, 0, 5)))
    }else{
      dir.create(file.path(substr(input$survey2File, 0, 5)), recursive = TRUE)
      setwd(file.path(getwd(), substr(input$survey2File, 0, 5)))
    }
    
    v <- substr(input$survey2File, 7, 10)
    o <- as.numeric(v)+1
    wb <- loadWorkbook("S2.xlsx")
    writeData(wb, sheet = "Sheet1", startRow = o, d2, colNames = F)
    saveWorkbook(wb,"S2.xlsx",overwrite = T)
    setwd(file.path(currDir))
    
    if (dir.exists(substr(input$survey2File, 0, nchar(as.character(input$survey2File)) - 8))){
      setwd(file.path(getwd(), substr(input$survey2File, 0, nchar(as.character(input$survey2File)) - 8)))
    } else {
      dir.create(file.path(getwd(), substr(input$survey2File, 0, nchar(as.character(input$survey2File)) - 8)), recursive = TRUE)
      setwd(file.path(getwd(), substr(input$survey2File, 0, nchar(as.character(input$survey2File)) - 8)))
      
    }
    
    write.csv(d2, file = paste("w", substr(input$survey2File, 7, 10), "_", "s", substr(input$survey2File, 12, 13), "_", "S2", ".csv",  sep = ""))
    
    showNotification("Data Saved!", action = a(href = "javascript:location.reload();", "Reload page"))
    
    setwd(file.path(currDir))
    
  })
  
  #Saving Survey 3 data to a .csv file
  observeEvent(input$action3, {
    
    d3 <- data.frame(Initials = input$survey3Initials, 
                     Current_Location = input$survey3Location,
                     Date = input$survey3Date,
                     Time = input$survey3Time)
    
    currDir <- getwd()
    
    
    if (dir.exists(substr(input$survey3File, 0, 5))){
      setwd(file.path(getwd(), substr(input$survey3File, 0, 5)))
    }else{
      dir.create(file.path(substr(input$survey3File, 0, 5)), recursive = TRUE)
      setwd(file.path(getwd(), substr(input$survey3File, 0, 5)))
    }
    
    v <- substr(input$survey3File, 7, 10)
    o <- as.numeric(v)+1
    wb <- loadWorkbook("S3.xlsx")
    writeData(wb, sheet = "Sheet1", startRow = o, d3, colNames = F)
    saveWorkbook(wb,"S3.xlsx",overwrite = T)
    setwd(file.path(currDir))
    
    if (dir.exists(substr(input$survey3File, 0, nchar(as.character(input$survey3File)) - 8))){
      setwd(file.path(getwd(), substr(input$survey3File, 0, nchar(as.character(input$survey3File)) - 8)))
    } else {
      dir.create(file.path(getwd(), substr(input$survey3File, 0, nchar(as.character(input$survey3File)) - 8)), recursive = TRUE)
      setwd(file.path(getwd(), substr(input$survey3File, 0, nchar(as.character(input$survey3File)) - 8)))
      
    }
    
    write.csv(d3, file = paste("w", substr(input$survey3File, 7, 10), "_", "s", substr(input$survey3File, 12, 13), "_", "S3", ".csv",  sep = ""))
    
    showNotification("Data Saved!", action = a(href = "javascript:location.reload();", "Reload page"))
    
    setwd(file.path(currDir))
    
  })
  
  #Crops and saves sample image
  observeEvent(input$cropSaveSample, {
    
    sample <- image_read(input$file1$datapath)
    savedSample <- image_chop(sample, "0x340")
    tempSample <-image_chop(sample, "1300x1700+0+290")
    image_write(tempSample, path = "test.png", format = "png")
    
    source_python('decoder.py')
    fileName <- decode_file("test.png")
    fn <- "test.png"
    if (file.exists(fn)) 
      file.remove(fn)
    
    currDir <- getwd()
    
    if (dir.exists(substr(fileName, 0, nchar(as.character(fileName)) - 3))){
      setwd(file.path(getwd(), substr(fileName, 0, nchar(as.character(fileName)) - 3)))
    } else {
      dir.create(file.path(getwd(), substr(fileName, 0, nchar(as.character(fileName)) - 3)), recursive = TRUE)
      setwd(file.path(getwd(), substr(fileName, 0, nchar(as.character(fileName)) - 3)))
      
    }
    
    imageName<- paste("w", substr(fileName, 7, 10), "_", "s", substr(fileName, 12, 13), "_", "p", substr(fileName, 23, 23), "_", "r",substr(fileName, 25, 25)  ,  sep = "")
    image_write(savedSample, path = paste(imageName, ".png"), format = "png")
    showNotification("Sample Cropped and Saved!", action = a(href = "javascript:location.reload();", "Reload page"))
    
    setwd(file.path(currDir))
  })
  
  observeEvent(input$surveyFileVal, {
    
    sample <- image_read(input$file1$datapath)
    tempSample <-image_chop(sample, "500x700+0+93")
    finalSample <- image_scale(tempSample, "300x300")
    image_write(finalSample, path = "test.png", format = "png")
    
    source_python('decoder.py')
    fileName <- decode_file("test.png")
    fn <- "test.png"
    if (file.exists(fn)) 
      file.remove(fn)
    updateTextInput(session, "surveyFilePath", value = as.character(fileName))
  })
  
  #Function to read in images
  read.image <- function(image.file){
    im <- load.image(image.file)
    if(dim(im)[4] > 3){
      im <- imappend(channels(im, 1:3), 'c')
    }
    im
  }

  #Function to select points
  select.points <- function(im, x, y){
    if(is.null(x) | is.null(y)){
      mask <- matrix(1L, nrow=nrow(im), ncol=ncol(im))
    }else{
      xy<-cbind(x,y)
      xy<-as.data.frame(xy)
      coordinates(xy)=c("x","y")
      pnts<-vect2rast(xy)
      poly <- owin(poly=list(x=x, y=y), check=F)
      SpP<- as(poly,  "SpatialPolygons")
      attr  =  data.frame(a=1,  b=1)
      SrDf  =  SpatialPolygonsDataFrame(SpP,  attr)
      rast <- vect2rast(SrDf,cell.size=1)
      r <- raster(rast)
      crop <- coordinates(r)[!is.na(values(r)),]
      crop <- as.data.frame(crop)
      mask <- matrix(0L, nrow=nrow(im), ncol=ncol(im))
      for(x.coord in unique(crop$x)){
        t <- crop[crop$x==x.coord,]
        mask[t$x, t$y] <- 1
      }
    }
    mask
  }


  #Makes all points in image that have a 0 in the mask white
  removePoints <- function(im, mask){
    im[mask==0] <- 1
    im
  }

  #Plotting the image
  app.plot <- function(im, clicks.x = NULL, clicks.y = NULL, lineslist = NULL){
    if(is.null(im)){
      return(NULL)
    }
    if(is.null(ranges$x) | is.null(ranges$y)){
      #plot(paw, xaxt='n', yaxt='n', ann=FALSE)
      plot(im, xaxt='n', yaxt='n', ann=FALSE)
    }else{
      plot(im, xaxt='n', yaxt='n', ann=FALSE, xlim=ranges$x,  ylim=c(ranges$y[2], ranges$y[1]))
    }
    if(length(clicks.x) > 1){
      lines(c(clicks.x, clicks.x[1]), c(clicks.y, clicks.y[1]), col='red')
    }
    if(!is.null(lineslist)){
      for(i in 1:length(lineslist)){
        x <- lineslist[[i]][[1]]
        y <- lineslist[[i]][[2]]
        lines(c(x, x[1]), c(y, y[1]), col='red')
      }
    }
  }

  #Set ranges for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)

  #Zoom in on brushed area when double clicking for plot 1
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  v <- reactiveValues(
    originalImage = NULL,
    croppedImage = NULL,
    imgMask = NULL,
    imgclick.x = NULL,
    imgclick.y = NULL,
    crop.img = FALSE,
    imageName = NULL
  )

  #Reads image
  observeEvent(input$file1, {
    v$originalImage <- read.image(input$file1$datapath)
    v$croppedImage = NULL
    v$imgMask = NULL
    v$imgclick.x = NULL
    v$imgclick.y = NULL
    v$crop.img = FALSE
    updateTextInput(session, inputId = "imgName", label = NULL, value = v$imageName)
    output$plot1 <- renderPlot({
      app.plot(v$originalImage,v$imgclick.x, v$imgclick.y)
    }, width = w, height = h)
  })

  observeEvent(input$imgName, {v$imageName <- input$imgName})

  #Handle clicks on the plot for tracing foreground
  observeEvent(input$selectForeground, {
    v$crop.img <- TRUE
    disable("selectForeground")
    enable("pauseCropping")
    enable("resetCropping")
    enable("cropBackground")
  })

  #Pause cropping
  observeEvent(input$pauseCropping, {
    v$crop.img <- FALSE
    disable("pauseCropping")
    enable("selectForeground")
    enable("resetTracePaw")
    enable("cropBackground")
  })


  #Reset cropping
  observeEvent(input$resetCropping, {
    v$croppedImage <- NULL
    v$imgMask <- NULL
    v$crop.img <- FALSE
    v$imgclick.x  <- NULL
    v$imgclick.y <- NULL
    enable("pauseCropping")
    enable("selectForeground")
    disable("resetTracePaw")
    enable("cropBackground")
    output$plot1 <- renderPlot({app.plot(v$originalImage, v$imgclick.x, v$imgclick.y)}, width = w, height = h)
    ranges$x <- NULL
    ranges$y <- NULL
  })


  observeEvent(input$cropBackground,{
    if(is.null(v$imgclick.x) | is.null(v$imgclick.y)){
      v$croppedImage <- v$originalImage
      v$imgMask <- select.points(v$originalImage, v$imgclick.x, v$imgclick.y)
    }else{
      v$imgMask <- select.points(v$originalImage, v$imgclick.x, v$imgclick.y)
      v$croppedImage <- removePoints(v$originalImage, v$imgMask)
      v$crop.img <- FALSE
      v$imgclick.x  <- NULL
      v$imgclick.y <- NULL
      enable("pauseCropping")
      enable("selectForeground")

      enable("resetTracePaw")
      disable("cropBackground")
    }
    output$plot1 <- renderPlot({
      app.plot(v$croppedImage)
    })
  })

  observeEvent(input$downloadImage, {imager::save.image(v$croppedImage, paste(input$imgName, ".png"))})

  #Keep track of click locations
  observeEvent(input$plot1_click, {
    # Keep track of number of clicks for line drawing
    if(v$crop.img){
      v$imgclick.x <- c(v$imgclick.x, round(input$plot1_click$x))
      v$imgclick.y <- c(v$imgclick.y, round(input$plot1_click$y))
    }
  })


  ### Original Image
  output$plot1 <- renderPlot({app.plot(v$originalImage, v$imgclick.x, v$imgclick.y)})

}

# Run application 
shinyApp(ui = ui, server = server)