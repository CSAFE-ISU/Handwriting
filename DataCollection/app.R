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
use_python("/usr/local/bin/python3.7", required = T)
py_config()
#use_python("C:\\Program Files\\Python36", required = T)


#UI Structure
ui <- fluidPage(theme = shinytheme("yeti"),
                tabsetPanel(
                  
                  #Begin Survey 1 Tab
                  tabPanel("Survey 1", 
                           
                           titlePanel("CSAFE Handwriting Data Collection (Survey 1)"),
                           
                           fluidRow(
                             column(3, textInput("surveyInitials", h3("1. Initials"), value = NULL)),
                             column(3, selectInput("surveyLanguage", h3("7. Language"), choices = list("a. Yes" = "a. Yes", "b. No" = "b. No"), selected = 1))
                           ),
                           
                           fluidRow(
                             column(3, textInput("surveyLocation", h3("2. Current Location"), value = NULL)),
                             column(3, selectInput("surveyGender", h3("8. Gender"), choices = list("a. Female" = "a. Female", "b. Male" = "b. Male", "c. Other" = "c. Other"), selected = 1)),
                             column(3, textInput("surveyOtherGender", h3("8c. Other"), value = NULL))
                           ),
                           
                           fluidRow(
                             column(3, dateInput("surveyDate",h3("3 . Date"),value = "2014-01-01")),
                             column(3, selectInput("surveyEthnicity", h3("9. Ethnicity "), choices = list("a. African American" = "a. African American", "b. Asian" = "b. Asian","c. Caucasian" = "c. Caucasian", "d. Hispanic" = "d. Hispanic", "e. Native American" = "e. Native American" ,"f. South Pacific" = "f. South Pacific", "g. Other" = "g. Other"), selected = 1))
                           ),
                           
                           fluidRow(   
                             column(3, selectInput("surveyTime", h3("4. Time"), choices = list("a. Early Morning" = "a. Early Morning", "b. Late Morning" = "b. Late Morning","c. Early Afternoon" = "c. Early Afternoon", "d. Late Afternoon" = "d. Late Afternoon", "e. Early Evening" = "e. Early Evening" ,"f. Late Evening" = "f. Late Evening"), selected = 1)),
                             column(3, selectInput("surveyEducation", h3("10. Formal Edu."), choices = list("a. High school or less" = "a. High school or less", "b. More than high school" = "b. More than high school"), selected = 1))
                            ),
                           
                           fluidRow(
                             column(3, textInput("surveyThirdGrade", h3("5. 3rd Grade Edu."), value = NULL)),
                             column(3, selectInput("surveyHand", h3("11. Dominant Hand"), choices = list("a. Left" = "a. Left", "b. Right" = "b. Right", "c. Ambidextrous" = "c. Ambidextrous"), selected = 1))
                           ),
                           
                           fluidRow(
                             column(3, selectInput("surveyAge", h3("6. Age"), choices = list("a. 18-24" = "a. 18-24", "b. 25-40" = "b. 25-40","c. 41-60" = "c. 41-60", "d. 61+" = "d. 61+"), selected = 1))
                           ),
                           
                           fluidRow(column(3, textInput("surveyQR", h3("File Path"), value = NULL))),
                           
                           fluidRow(column(3, actionButton("action", label = "SAVE"))),
                           
                           mainPanel( img(src = "csafe_logo.jpg", height = 200, width = 300) )),
                  #End Survey 1 Tab
                  
                  #Begin Survey 2 Tab
                  tabPanel("Survey 2", 
                           
                           titlePanel("CSAFE Handwriting Data Collection (Surveys 2)"),       
                           
                           fluidRow(
                             column(3, textInput("survey2Initials", h3("1. Initials"), value = NULL)),
                             column(3, textInput("survey2Location", h3("2. Current Location"), value = NULL)),
                             column(3, dateInput("survey2Date",h3("3 . Date"),value = "2014-01-01")),
                             
                             column(3, selectInput("survey2Time", h3("4. Time"), choices = list("a. Early Morning" = "a. Early Morning", "b. Late Morning" = "b. Late Morning","c. Early Afternoon" = "c. Early Afternoon", "d. Late Afternoon" = "d. Late Afternoon", "e. Early Evening" = "e. Early Evening" ,"f. Late Evening" = "f. Late Evening"), selected = 1))
                           ),
                           
                           fluidRow(column(3, textInput("surveyQR2", h3("File Path"), value = NULL))),
                           
                           fluidRow(column(3, actionButton("action2", label = "SAVE"))),
                           
                           mainPanel( img(src = "csafe_logo.jpg", height = 200, width = 300))
                  ),
                  #End Survey 2 Tab
                  
                  #Begin Survey 3 Tab
                  tabPanel("Survey 3", 
                           
                           titlePanel("CSAFE Handwriting Data Collection (Surveys 3)"),       
                           
                           fluidRow(
                             column(3, textInput("survey3Initials", h3("1. Initials"), value = NULL)),
                             column(3, textInput("survey3Location", h3("2. Current Location"), value = NULL)),
                             column(3, dateInput("survey3Date",h3("3 . Date"),value = "2014-01-01")),
                             
                             column(3, selectInput("survey3Time", h3("4. Time"), choices = list("a. Early Morning" = "a. Early Morning", "b. Late Morning" = "b. Late Morning","c. Early Afternoon" = "c. Early Afternoon", "d. Late Afternoon" = "d. Late Afternoon", "e. Early Evening" = "e. Early Evening" ,"f. Late Evening" = "f. Late Evening"), selected = 1))
                           ),
                           
                           fluidRow(column(3, textInput("surveyQR3", h3("File Path"), value = NULL))),
                           
                           fluidRow(column(3, actionButton("action3", label = "SAVE"))),
                           
                           mainPanel( img(src = "csafe_logo.jpg", height = 200, width = 300))
                  ),
                  #End Survey 3 Tab
                  
                  #Begin Image Viewer/Cropper tab
                  tabPanel("Image View/Crop",
                           
                           fluidPage(
                             titlePanel("View/Crop Image"),
                             img(src = "csafe_logo.jpg", height = 130, width = 200),
                             useShinyjs(),
                             sidebarLayout(
                               sidebarPanel(
                                 fileInput("file1", "Choose Image", accept = c(".png",".jpg")),
                                 textInput("popFilePath", h3("File Path"), value = NULL),
                                 actionButton("surveyAction", label = "Survey"),
                                 actionButton("sampleAction", label = "Prompt"),
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

# testfile = "writing/w0000/s01/pWOZ_r1"

#Writes survey data to file specified by File Name text input
server <- function(input, output, session) {
  
  rootpath = "~/Documents/CSAFE/DataCollection/Scanning/Z/"
  stagepath = "~/Documents/CSAFE/DataCollection/Scanning/"
  pypath = "~/Documents/CSAFE/DataCollection/Scanning/scan_processing/DataCollection/"
  
  #Height and Width variables for dyanmically manipulating image size in Image Viewer/Cropper
  h <- 845
  w <- 745
  
  #Saving Survey 1 data to a .csv file
  observeEvent(input$action, {
    
    d <- data.frame( WID = gsub("(^.+/w)(\\d+)(/.+$)", "\\2", input$surveyQR), #WID
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
    
    if (!dir.exists(gsub("(.+?)(\\/.*)", "\\1", input$surveyQR))){   #everything before first fwd slash - "surveys"
      dir.create(file.path(paste0(rootpath, gsub("(.+?)(\\/.*)", "\\1", input$surveyQR), "/")), recursive = TRUE)
    }
    fp = file.path(paste0(rootpath, gsub("(.+?)(\\/.*)", "\\1", input$surveyQR), "/"))
    
    WID <- gsub("(^.+/w)(\\d+)(/.+$)", "\\2", input$surveyQR) #WID
    line_entry <- as.numeric(WID)+1
    wb <- loadWorkbook(paste0(fp,"S1.xlsx"))
    writeData(wb = wb, sheet = "Sheet1", x = d, startRow = line_entry, colNames = F)
    saveWorkbook(wb,paste0(fp,"S1.xlsx"), overwrite = T)
    
    if (!dir.exists(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR), "/")))){  #everything before last fwd slash
      dir.create(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR), "/")), recursive = TRUE)
    }
    fp = file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR), "/"))
    
    write.csv(d, file = paste0(fp,gsub("/", "_", gsub("(.+?\\/)(.*)", "\\2", input$surveyQR)), ".csv")) #everything after first fwd slash - turn / to _
    showNotification("Data Saved!", action = a(href = "javascript:location.reload();", "Reload page"))
  })
  
  #Saving Survey 2 data to a .csv file
  observeEvent(input$action2, {
    
    d2 <- data.frame(Initials = input$survey2Initials, 
                     Current_Location = input$survey2Location,
                     Date = input$survey2Date,
                     Time = input$survey2Time)
    
    if (!dir.exists(gsub("(.+?)(\\/.*)", "\\1", input$surveyQR2))){   #everything before first fwd slash - "surveys"
      dir.create(file.path(paste0(rootpath, gsub("(.+?)(\\/.*)", "\\1", input$surveyQR2), "/")), recursive = TRUE)
    }
    fp = file.path(paste0(rootpath, gsub("(.+?)(\\/.*)", "\\1", input$surveyQR2), "/"))
    
    WID <- gsub("(^.+/w)(\\d+)(/.+$)", "\\2", input$surveyQR2) #WID
    line_entry <- as.numeric(WID)+1
    wb <- loadWorkbook(paste0(fp,"S2.xlsx"))
    writeData(wb = wb, sheet = "Sheet1", x = d, startRow = line_entry, colNames = F)
    saveWorkbook(wb,paste0(fp,"S2.xlsx"), overwrite = T)
    
    if (!dir.exists(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR2), "/")))){  #everything before last fwd slash
      dir.create(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR2), "/")), recursive = TRUE)
    }
    fp = file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR2), "/"))
    
    write.csv(d, file = paste0(fp,gsub("/", "_", gsub("(.+?\\/)(.*)", "\\2", input$surveyQR2)), ".csv")) #everything after first fwd slash - turn / to _
    showNotification("Data Saved!", action = a(href = "javascript:location.reload();", "Reload page"))
  })
  
  #Saving Survey 3 data to a .csv file
  observeEvent(input$action3, {
    d3 <- data.frame(Initials = input$survey3Initials, 
                     Current_Location = input$survey3Location,
                     Date = input$survey3Date,
                     Time = input$survey3Time)
    
    if (!dir.exists(gsub("(.+?)(\\/.*)", "\\1", input$surveyQR3))){   #everything before first fwd slash - "surveys"
      dir.create(file.path(paste0(rootpath, gsub("(.+?)(\\/.*)", "\\1", input$surveyQR3), "/")), recursive = TRUE)
    }
    fp = file.path(paste0(rootpath, gsub("(.+?)(\\/.*)", "\\1", input$surveyQR3), "/"))
    
    WID <- gsub("(^.+/w)(\\d+)(/.+$)", "\\2", input$surveyQR3) #WID
    line_entry <- as.numeric(WID)+1
    wb <- loadWorkbook(paste0(fp,"S3.xlsx"))
    writeData(wb = wb, sheet = "Sheet1", x = d, startRow = line_entry, colNames = F)
    saveWorkbook(wb,paste0(fp,"S3.xlsx"), overwrite = T)
    
    if (!dir.exists(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR3), "/")))){  #everything before last fwd slash
      dir.create(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR3), "/")), recursive = TRUE)
    }
    fp = file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", input$surveyQR3), "/"))
    
    write.csv(d, file = paste0(fp,gsub("/", "_", gsub("(.+?\\/)(.*)", "\\2", input$surveyQR3)), ".csv")) #everything after first fwd slash - turn / to _
    showNotification("Data Saved!", action = a(href = "javascript:location.reload();", paste0("Reload page", input$file1$name)))
  })
  
  
  
  #Crops and saves writing sample
  observeEvent(input$sampleAction, {
    vert_frac = .15                               ## fraction of the vertical pixels that marks just below the line.
    sample = image_read(input$file1$datapath)
    dims = dim(as.raster(sample))                 ## [1] = rows (vert, y), [2] = cols (hor, x)
    top_y = vert_frac*dims[1]
    bottom_y = (1-vert_frac)*dims[1]
    
    # this saves the bottom portion of the image. Keeps from 0th col, top_yth row and underneath
    savedSample <- image_chop(sample, paste0("0x",top_y))     #;plot(savedSample)
    
    # this saves the upper right corner. (XbyY is the amount to cut off, X+Y is the offset)
    tempSample <- image_chop(sample, paste0(dims[2]*.75,"x", bottom_y,"+0+",top_y-40))
    image_write(tempSample, path = "test.png", format = "png")
    
    source_python(paste0(pypath, 'decoder.py'))
    fileName <- decode_file("test.png")
    fn <- "test.png"
    if (file.exists(fn)) 
      file.remove(fn)
    updateTextInput(session, "popFilePath", value = as.character(fileName))
    
    if (!dir.exists(gsub('(.*)/\\w+', "\\1", fileName))){
      dir.create(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", fileName))), recursive = TRUE)
    }
    fp = file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", fileName), "/"))
    
    imageName <- gsub("/", "_", gsub("(.+?\\/)(.*)", "\\2", fileName))
    image_write(savedSample, path = paste0(fp, imageName, ".png"), format = "png")
    ### DUMP ORIGINAL INTO ANOTHER FOLDER??
    showNotification("Sample Cropped and Saved!", action = a(href = "javascript:location.reload();", "Reload page"))
    file.rename(paste0(stagepath, input$file1$name), paste0(gsub(".png", "", paste0(stagepath, input$file1$name)), "_processed.png"))
  })
  
  observeEvent(input$surveyAction, {
    vert_frac = .15                               ## fraction of the vertical pixels that marks just below the line.
    sample = image_read(input$file1$datapath)
    dims = dim(as.raster(sample))                 ## [1] = rows (vert, y), [2] = cols (hor, x)
    top_y = vert_frac*dims[1]
    bottom_y = (1-vert_frac)*dims[1]
    
    tempSample = image_chop(sample, paste0(dims[2]*.75,"x", bottom_y,"+0+",top_y-40)) ;plot(tempSample)
    image_write(tempSample, path = "test.png", format = "png")
    
    source_python(paste0(pypath, 'decoder.py'))
    fileName <- decode_file("test.png")
    fn <- "test.png"
    if (file.exists(fn)) 
      file.remove(fn)
    updateTextInput(session, "popFilePath", value = as.character(fileName))
    
    if (!dir.exists(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", fileName), "/")))){  #everything before last fwd slash
      dir.create(file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", fileName), "/")), recursive = TRUE)
    }
    fp = file.path(paste0(rootpath, gsub('(.*)/\\w+', "\\1", fileName), "/"))
    
    image_write(sample, path = paste0(fp, gsub("/", "_", gsub("(.+?\\/)(.*)", "\\2", fileName)), ".png"), format = "png")
    file.rename(paste0(stagepath, input$file1$name), paste0(gsub(".png", "", paste0(stagepath, input$file1$name)), "_processed.png"))
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