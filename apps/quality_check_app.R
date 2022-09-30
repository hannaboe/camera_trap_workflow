if (!require('bslib')) install.packages("bslib"); library('bslib')
if (!require('shiny')) install.packages("shiny"); library('shiny')
if (!require('shinyalert')) install.packages("shinyalert"); library('shinyalert')
if (!require('jpeg')) install.packages("jpeg"); library('jpeg')
if (!require('tidyverse')) install.packages("tidyverse"); library('tidyverse')
#if (!require('uuid')) install.packages("uuid"); library('uuid')
if (!require('shinyFiles')) install.packages("shinyFiles"); library('shinyFiles')



## set direcotries



  ui = fluidPage(
    #useShinyalert(),
    tabsetPanel(id = "inTabset",
      tabPanel("Set up", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(id = "startPanel", style = "overflow-y:scroll; max-height: 100vh; position:relative;",
                              width = 3,
                              p("Select the main folder that contains the subfolders with the images and all other related files, then press MAIN FOLDER SELECTED."),
                              shinyDirButton('folder', 'Select folder', 'Select the main folder (click on the arrow to go into subfolders)', FALSE,  width = "100%", buttonType = "default"),
                              br(), br(),
                              actionButton("folder_selected", label = "MAIN FOLDER SELECTED", width = "100%", class = "btn-primary"),
                              br(), br(), hr(),
                              textInput("year", "Select year", value = "write the year.."),
                              textInput("locality", "Select locality", value = "write the locality..."),
                              textInput("model_name", "Select folder with classification files", value = "write the model name..."),
                              br(), hr(),
                              selectInput("type", "Select type of quality check", choices = c("random", "class", "confidence"), selected = "random"),
                              p("Random: check random subset of all images"),
                              p("Class: check random images of each class"),
                              p("Confidence: check random images of each confidence class"),
                              br(),
                              textInput("no_images", "Select how many images should be checked", value = "500"),
                              p("If type is 'random', this is the total number of images."),
                              p("If type is 'class' or 'confidence, this is the number of images per class or confidence class."),
                              br(),
                              textInput("observer", label = "Observer", value = "your initials..."),
                              #p("Use to GET OVERVIEW for a summary of the images that have to be checked (this might take a while)."),
                              #actionButton("get_overview", label = "GET OVERVIEW", width = "100%", class = "btn-primary"),
                              br(), hr(),
                              p("Press START QUALITY CHECK after selecting all values. The images are displayed in the Classification-tab. One file with the results will be saved."),
                              #textInput("site", label = "Select site", value = "ko_ko_m_a"),
                              actionButton("start", label = "START QUALITY CHECK", width = "100%", class = "btn-primary"),
                              br(), br(), hr(),
                              p("Use STOP to close the app"),
                              actionButton("stop_app2", label = "STOP", width = "100%")
                 ),
                 
                 mainPanel(id = "startPanel2", style = "overflow-y:scroll; max-height: 100vh; position:relative;",
                   tableOutput("overview_out"),
                   textOutput("text1")
                 ))),
      
      tabPanel("Classification", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(id = "classPanel", style = "overflow-y:scroll; max-height: 100vh; position:relative;",
                   width = 2, 
                                p("Select class:"),
                              actionButton("bad_quality", label = "Bad quality", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(), 
                              actionButton("empty", label = "Empty", width = "100%", style = "margin-bottom: 7px; height: 50px"),
                              br(),
                              actionButton("vole", label = "Vole", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("lemming", label = "Lemming", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("shrew", label = "Shrew", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("least_weasel", label = "Least weasel", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("stoat", label = "Stoat", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("bird", label = "Bird", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("difficult", label = "Difficult", width = "100%", style = "margin-bottom: 10px; height: 50px;"),
                              br(), hr(),
                              p("Check the previous or next image (in the next tab) if it is difficult to identify the species."),
                              actionButton("next_image", label = "Next image", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("pre_image", label = "Previous image", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(), hr(),
                              textInput("comment", label = "Add a comment", value = "", width = "100%"),
                              br(),hr(),
                              p("Use NEXT to skip the image"),
                              p("Use BACK to go back to the last image and correct the annotation"),
                              p("Use STOP to close the app"),
                              actionButton("skip", label = "NEXT", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("back", label = "BACK", width = "100%", style = "margin-bottom: 7px; height: 50px;"),
                              br(),
                              actionButton("stop_app", label = "STOP", width = "100%", style = "margin-bottom: 10px; height: 50px;")
                             
                              
                              
                  ),
                 mainPanel(width = 10,
                           fillPage(
                             tags$style(type = "text/css", "#image {height: calc(100vh - 80px) !important;}"),
                             plotOutput("image",  width = "100%", height = "1200px"),
                             textOutput("all_checked")
                           )
                 ))),
      tabPanel("Check other images", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(width = 0,
                   #actionButton("next_image_2", label = "Next image", width = "80%", style = "margin-bottom: 7px;"),
                   #br(),
                   #actionButton("pre_image_2", label = "Previous image", width = "80%", style = "margin-bottom: 7px;")
                 ),
                 mainPanel(width = 10,
                           fillPage(
                             tags$style(type = "text/css", "#image_new {height: calc(100vh - 80px) !important;}"),
                             plotOutput("image_new",  width = "100%", height = "1200px"),
                           )
                 )
               )),
    ))
  
  server = function(input, output, session) {
    
    #### SET UP --------
    
    shinyDirChoose(input, 'folder', roots=c(wd=drive), filetypes=c('', 'txt'))
    
    path <- reactive({
      #print(input$folder)
      parseDirPath(roots=c(wd=drive), input$folder)
    })
    
    observeEvent(input$folder_selected, {
      setwd(path())
      #localities <- dir(paste(path(), "images_renamed", sep = "/"))
      #year <- dir(paste(path(), "automatic_classification/model2022_classification", sep =  "/"))
    })
    
    
    
    ## make pathes
      input.dir <- reactive(sub("//", "/", paste("automatic_classification", input$model_name, input$locality, input$year, sep = "/")))
      #output.dir <- reactive(paste("manual_classification", input$locality, input$year, sep = "/"))
      qual.check.dir <- reactive(paste("quality_check", input$model_name, input$year, sep = "/"))
      image.dir.short <- reactive(paste("images_renamed", sep = "/"))
      #image.dir <- reactive(paste("images_renamed", input$locality, input$site, input$year, sep = "/"))
      results.name <- reactive(sub("__", "_", paste0("quality_check_", input$locality, "_", input$year,  ".txt")))  
      
    
    
    ## create file for results if it doesn't exist yet
    observeEvent(input$start, {
      if (!file.exists(qual.check.dir())) dir.create(qual.check.dir())  # does not work
      if (!file.exists(paste(qual.check.dir(), results.name(), sep = "/"))) {
      results<-data.frame(site = "0", year = "0", filename= "0", class_id = "0", type = "0", guess1 = "0", confidence1 = "1", confidence_class = "0", observer = "0",  comment = "0")
      results<-results[-1,]
      write.table(results, paste(qual.check.dir(), results.name(), sep = "/"), row.names = FALSE)
    }
      })
    
    
    #### GET DATA -------
    
    ## load data (automatic classifications)
    dat_complete <- eventReactive(input$start, {
      
      filenames <- dir(input.dir())
      myfile <- map_dfr(paste(input.dir(), filenames, sep = "/"), read.table, header = TRUE)
      
    
      ## add categories to classification file
      myfile$class_id<-ifelse(myfile$guess1==0, "bad_quality", 
                              ifelse(myfile$guess1==1, "empty", 
                                     ifelse(myfile$guess1==2, "bird", 
                                            ifelse(myfile$guess1==3, "vole", 
                                                   ifelse(myfile$guess1==4, "least_weasel", 
                                                          ifelse(myfile$guess1==5, "lemming", 
                                                                 ifelse(myfile$guess1==6, "shrew", 
                                                                        ifelse(myfile$guess1==7, "stoat", NA))))))))
      
      complete <- myfile
    })
      
   
    
    ## prepare data
    dat <- eventReactive(dat_complete(), {
      
      classes <- unique(dat_complete()$class_id)
      conf <- seq(from = 0, to = 1, length =11)
      
      ## random qyality check
      if (input$type == "random") {
        results<-read.table(paste(qual.check.dir(), results.name(), sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
        n_select <- as.numeric(input$no_images)-nrow(results[results$type == "random",])
        
        if(nrow(results) != 0) {
          toto <- dat_complete()[!dat_complete()$filename %in% results$filename,]
        } else {
          toto <- dat_complete()
        }
        toto <- toto[sample(nrow(toto), n_select),]
      } else if (input$type == "class") {
        results<-read.table(paste(qual.check.dir(), results.name(), sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
        toto <- c()
        for (i in 1:length(classes)) {
          n_select <- as.numeric(input$no_images)-nrow(results[results$type == "class" & results$guess1 == classes[i],])
          
          if(nrow(results) != 0) {
            toto1 <- dat_complete()[(!dat_complete()$filename %in% results$filename) & dat_complete()$class_id == classes[i],]
          } else {
            toto1 <- dat_complete()[dat_complete()$class_id == classes[i],]
          }
          
          if(nrow(toto1) > n_select) toto1 <- toto1[sample(nrow(toto1), n_select),]
          toto[[i]] <- toto1
        }
        
        toto <- do.call(rbind, toto)
      } else if (input$type == "confidence") {
        results<-read.table(paste(qual.check.dir(), results.name(), sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
        toto <- c()
        for (i in 1:(length(conf)-1)) {
          n_select <- as.numeric(input$no_images)-nrow(results[results$type == "confidence" & results$confidence1 > conf[i] & results$confidence1 <= conf[i+1],])
          
          if(nrow(results) != 0) {
            toto1 <- dat_complete()[(!dat_complete()$filename %in% results$filename) & dat_complete()$confidence1 > conf[i] & dat_complete()$confidence1 <= conf[i+1],]
          } else {
            toto1 <- dat_complete()[dat_complete()$confidence1 > conf[i] & dat_complete()$confidence1 <= conf[i+1],]
          }
          if(nrow(toto1) > n_select) toto1 <- toto1[sample(nrow(toto1), n_select),]
          toto[[i]] <- toto1
        }
        
        toto <- do.call(rbind, toto)
      }
      
    })
    
    
    observeEvent(input$start, {
      updateTabsetPanel(session, "inTabset", selected = "Classification")
    })
    
    observeEvent(dat(), {
      shinyalert(title = input$type, 
                 text = paste0(nrow(dat()), " images have to be checked"),
                 type = "info")
    })
    
    #### CLASSIFICATION --------
    
    # make reactive value for looping through the images
    i <- reactiveVal(1)
    observeEvent(input$start, {i(1)}) # reset to 1 if the input changed
    
    ## load image 
    pic2 <- reactive( {
      if (i() <= nrow(dat())) {
        #pic <- load.image(paste(image.dir(), dat()$filename[i()], sep = "/"))
        #pic <- resize(pic, 1800, 1024, 1, 3)
        image.dir <- reactive(sub("//", "/", paste(image.dir.short(), input$locality, dat()$site[i()], input$year, sep = "/")))
        
        pic <- readJPEG(paste(image.dir(), dat()$filename[i()], sep = "/"), native = TRUE)
        par(mar = c(7.1, 0.1, 2.1, 0.1))
        par(xpd=NA)
        plot(1:2, type='n', axes = FALSE, xlab = "")
        rasterImage(pic, 1, 1, 2, 2)
        #plot(pic, axes = FALSE, ylim = c(1024,0))
        mtext(dat()$filename[i()], 1, line = 2, cex = 1.5)
        mtext(paste(dat()$class_id[i()], "(Confidence =", round(dat()$confidence1[i()], digits = 3), ")"), 1, line = 4, cex = 1.5)
        #previous <- which(dat_complete()$filename == dat()$filename[i()])
        #mtext(paste("previous image:", dat_complete()$class_id[previous-1], "(Confidence =", round(dat_complete()$confidence1[previous-1], digits = 3), ")"), 1, line = 6, cex = 1.5)
      }
    })
    
    ## meassage if all images are checked
    observe({
      if (i() > nrow(dat())) {
        shinyalert(title = "", 
                   text = "All images checked",
                   type = "info")
      }
    })
    
    ## plot image
    output$image <- renderPlot({
      pic2()
    })
    
    ## save the selected class
    class_val <- reactiveValues()
    
    observeEvent(input$bad_quality, {class_val$class <- "bad_quality"
    class_val$time = Sys.time()})
    observeEvent(input$empty, {class_val$class <- "empty"
    class_val$time = Sys.time()})
    observeEvent(input$vole, {class_val$class <- "vole"
    class_val$time = Sys.time()})
    observeEvent(input$lemming, {class_val$class <- "lemming"
    class_val$time = Sys.time()})
    observeEvent(input$shrew, {class_val$class <- "shrew"
    class_val$time = Sys.time()})
    observeEvent(input$stoat, {class_val$class <- "stoat"
    class_val$time = Sys.time()})
    observeEvent(input$least_weasel, {class_val$class <- "least_weasel"
    class_val$time = Sys.time()})
    observeEvent(input$bird, {class_val$class <- "bird"
    class_val$time = Sys.time()})
    observeEvent(input$difficult, {class_val$class <- "difficult"
    class_val$time = Sys.time()})
    
    
    ## add the classification to the results file
    observeEvent(class_val$time, {
      if(i() <= nrow(dat())) {
        results<-read.table(paste(qual.check.dir(), results.name(), sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
        results<-add_row(results, site = dat()$site[i()], year = as.character(input$year), observer = input$observer, filename = dat()$filename[i()], class_id = class_val$class, 
                         type = input$type, guess1 = dat()$class_id[i()], confidence1 = as.character(dat()$confidence1[i()]), comment = input$comment) # add results
        write.table(results, paste(qual.check.dir(), results.name(), sep = "/"), row.names = FALSE) # save results
        #print(i)
        new_i <- i()+1
        i(new_i)
        updateTextInput(session, "comment", value = "")
      } else {
        print("all checked")
      }
      
    }
  )
    ## skip image 
    next_val <- reactiveValues()
    observeEvent(input$skip, {next_val$time <- Sys.time()})
    
    observeEvent(next_val$time, {
      #print(i)
      new_i <- i()+1
      i(new_i)
    })
    
    ## stop the app  
    observeEvent(input$stop_app, {stopApp()})
    observeEvent(input$stop_app2, {stopApp()})
    
    ## go back to previous image (and delete the annotation)
    observeEvent(input$back, {
      results<-read.table(paste(qual.check.dir(), results.name(), sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
      results<-results[-nrow(results),] # add results
      write.table(results, paste(qual.check.dir(), results.name(), sep = "/"), row.names = FALSE) # save results
      print(i)
      new_i <- i()-1
      i(new_i)
    })
    
    
    #### Previous/next image ---------
    
    ## get the position of the previous or next image
    pos <- reactiveVal()
    
    observeEvent(input$next_image, {pos("next")})
    observeEvent(input$pre_image, {pos("pre")})
    #observeEvent(input$next_image_2, {pos("next_2")})
    #observeEvent(input$pre_image_2, {pos("pre_2")})
    
    ## get name of image
    new_image_name <- eventReactive(pos(), {

      if(pos() == "pre") {
        image_pos <- which(dat_complete()$filename == dat()$filename[i()])
        new_image_name1 <- dat_complete()$filename[image_pos-1]
      } else if (pos() == "next") {
        image_pos <- which(dat_complete()$filename == dat()$filename[i()])
        new_image_name2 <- dat_complete()$filename[image_pos+1]
      } #else if (pos() == "next_2") {
        #toto <- new_image_name()
        #image_pos <- which(dat_complete()$filename == toto)
        #new_image_name3 <- dat_complete()$filename[image_pos+1]
      #} else {
       # image_pos <- which(dat_complete()$filename == old_name())
        #new_image_name4 <- dat_complete()$filename[image_pos-1]
      #}
      
     
      
    })
    
    ## load image
    pic_new <- reactive({
      #pic_n <- load.image(paste(image.dir(), new_image_name(), sep = "/"))
      #pic_n <- resize(pic_n, 1800, 1024, 1, 3)
      image.dir <- reactive(sub("//", "/", paste(image.dir.short(), input$locality, dat_complete()$site, input$year, sep = "/")))
      pic <- readJPEG(paste(image.dir(), new_image_name(), sep = "/"), native = TRUE)
      par(mar = c(7.1, 0.1, 2.1, 0.1))
      #plot(pic_n, axes = FALSE, ylim = c(1024,0))
      plot(1:2, type='n', axes = FALSE, xlab = "")
      rasterImage(pic, 1, 1, 2, 2)
      mtext(new_image_name(), 1, line = 2, cex = 1.5)
    })
    

    ## plot image
    output$image_new <- renderPlot({
      pic_new()
    })
    
    
  }


  
shinyApp(ui = ui, server = server)


