if (!require('bslib')) install.packages("bslib"); library('bslib')
if (!require('shiny')) install.packages("shiny"); library('shiny')
if (!require('shinyalert')) install.packages("shinyalert"); library('shinyalert')
if (!require('jpeg')) install.packages("jpeg"); library('jpeg')
if (!require('tidyverse')) install.packages("tidyverse"); library('tidyverse')
#if (!require('uuid')) install.packages("uuid"); library('uuid')
if (!require('shinyFiles')) install.packages("shinyFiles"); library('shinyFiles')

## set direcotries

drive <- "E:/"

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
                              #selectInput("year", "Select year", choices = c(2016:2025), selected = 2021),
                              #textInput("locality", "Select locality", value = ""),
                              #br(),
                              sliderInput("confidence", label = "Select confidence", min = 0, max = 1, value = 0.9),
                              p("All images with a confidence below the selected confidence will be displayed. Set confidence to 1 for displaying all images."),
                              br(), br(), 
                              selectInput("class", "Select class", choices = c("bad_quality", "empty", "vole", "lemming", "shrew", "least_weasel", "stoat", "bird", "all"), selected = "all"),
                              p("All images classified as the selected class by the machine learning model will be displayed. Select ALL for displaying all images." ),
                              br(), br(),
                              p("Use to GET OVERVIEW for a summary of the images that have to be checked (this might take a while)."),
                              actionButton("get_overview", label = "GET OVERVIEW", width = "100%"),
                              br(), br(), hr(),
                              p("The images are classified per site. Press START CLASSIFICATION after selecting site and observer. The images are displayed in the Classification-tab. One file per site with the results will be saved."),
                              #textInput("site", label = "Select site", value = "ko_ko_m_a"),
                              selectInput("site", "Select site", choices = ""),
                              textInput("observer", label = "Observer", value = "your initials..."),
                              actionButton("start", label = "START CLASSIFICATION", width = "100%"),
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
      parseDirPath(roots=c(wd=drive), input$folder)
    })
    
    observeEvent(input$folder_selected, {
      setwd(path())
      })
    
    
    
    ## make pathes
    input.dir <- reactive(sub("//", "/", paste("automatic_classification", input$model_name, input$locality, input$year, sep = "/")))
    output.dir <- reactive(sub("//", "/", paste("manual_classification", input$locality, sep = "/")))
    qual.check.dir <- reactive(sub("//", "/", paste("quality_check", input$model_name, input$year, sep = "/")))
    image.dir.short <- reactive(paste("images_renamed", sep = "/"))
    image.dir <- reactive(sub("//", "/", paste("images_renamed", input$locality, input$site, input$year, sep = "/")))
    #results.name <- reactive(paste0(input$locality, "test2.txt"))  
    
    ## create file for results if it doesn't exist yet
    results.name <- reactive(paste0("manual_classification_", input$site, "_", input$year,  ".txt"))  
    observeEvent(input$start, {
      if (!file.exists(output.dir())) dir.create(output.dir())
      if (!file.exists(paste(output.dir(), input$year, sep = "/"))) dir.create(paste(output.dir(), input$year, sep = "/"))
      if (!file.exists(paste(output.dir(), input$year, results.name(), sep = "/"))) {
        results<-data.frame(site = "0", year = "0", observer = "0", filename= "0", class_id = "0", comment = "0")
        results<-results[-1,]
        write.table(results, paste(output.dir(), input$year, results.name(), sep = "/"), row.names = FALSE)
      }
    })
    
    ## get the available sites
    sites <- reactive({
      sites <- dir(paste(image.dir.short(), input$locality, sep = "/"))
      keep <- rep(NA, length(sites))
      for (i in 1:length(sites)) {
        if (file.exists(sub("//", "/", paste(image.dir.short(), input$locality, sites[i], input$year, sep = "/")))) keep[i] <- i
      }
      keep <- keep[!is.na(keep)]
      sites <- sites[keep]
      
    })
    
    ## make the input selection for sites
    observe({
      updateSelectInput(session, "site", choices = sites())
    })
    
    #### GET OVERVIEW ------
    
    overview <- eventReactive(input$get_overview, {
      
      overview <- data.frame(site = "xx", message = "xx")
      overview <- overview[-1,]
      
      for (i in 1:length(sites())) {
        
        ## load automatic classification file
        auto.name <- grep(sites()[i], dir(input.dir()), value = TRUE)
        if (length(auto.name) == 0) {
          overview <- add_row(overview, site = sites()[i], message = "Images have not been classified automatically")
          #overview[i] <- paste0(sites()[i], ": images have not been classified automatically")
        } else {
          auto <- read.table(paste(input.dir(), auto.name, sep = "/"), header = TRUE)
          
          ## add categories to classification file
          auto$class_id<-ifelse(auto$guess1==0, "bad_quality", 
                                ifelse(auto$guess1==1, "empty", 
                                       ifelse(auto$guess1==2, "bird", 
                                              ifelse(auto$guess1==3, "vole", 
                                                     ifelse(auto$guess1==4, "least_weasel", 
                                                            ifelse(auto$guess1==5, "lemming", 
                                                                   ifelse(auto$guess1==6, "shrew", 
                                                                          ifelse(auto$guess1==7, "stoat", NA))))))))
          
          if (input$class == "all") {
            auto.low <- auto$filename[auto$confidence1 <= input$confidence]
          } else {
            auto.low <- auto$filename[auto$confidence1 <= input$confidence & auto$class_id == input$class]
          } 
          
          if (length(auto.low) == 0) {
            overview <- add_row(overview, site = sites()[i], message = "No images to be checked")
            #overview[i] <- paste0(sites()[i], ": No images to be checked")
          } else {
            
            ## load manual classification
            manual.name <- grep(sites()[i], dir(paste(output.dir(), input$year, sep = "/")), value = TRUE)
            if (length(manual.name) == 0) {
              manual <- NULL
            } else {
              manual <- read.table(paste(output.dir(), input$year, manual.name, sep = "/"), header = TRUE)
            }
            
            ## load quality check files
            qual.names <- dir(qual.check.dir())
            
            if (length(qual.names) == 0) {
              qual <- NULL
            } else {
              qual <- map_dfr(paste(qual.check.dir(), qual.names, sep = "/"), read.table, header = TRUE)
            }
            
            
            ## take out images that have been classified manually already
            checked <- c(manual$filename[manual$filename %in% auto.low], qual$filename[qual$filename %in% auto.low])
            checked <- checked[!duplicated(checked)]
            
            to_be_checked <- length(auto.low)-length(checked)
            
            if (input$class != "all") {
              if (to_be_checked != 0) {
                overview <- add_row(overview, site = sites()[i], message = paste0(length(auto.low), " ", input$class, " images. ", to_be_checked, " images have to be checked"))
                #overview[i] <- paste0(sites()[i], ": ", length(auto.low), " ", input$class, " images. ", to_be_checked, " images have to be checked")
              } else {
                overview <- add_row(overview, site = sites()[i], message = paste0(length(auto.low), " ", input$class, " images. All images are checked"))
                #overview[i] <- paste0(sites()[i], ": ", length(auto.low), " ", input$class, " images. All images are checked")
                
              }
            } else {
              if (to_be_checked != 0) {
                overview <- add_row(overview, site = sites()[i], message = paste0(length(auto.low), " images with confidence below ", input$confidence, ". ", to_be_checked, " images have to be checked."))
                #overview[i] <- paste0(sites()[i], ": ", length(auto.low), " images with confidence below ", input$confidence, ". ", to_be_checked, " images have to be checked.")
              } else {
                if ("difficult" %in% manual$class_id) {
                  difficult <- length(which(manual$class_id == "difficult"))
                  overview <- add_row(overview, site = sites()[i], message = paste0(difficult, " images labeled as difficult and have to be checked"))
                  #overview[i] <- paste0(sites()[i], ": ", difficult, " images labeled as difficult and have to be checked")
                } else {
                  overview <- add_row(overview, site = sites()[i], message = "all images have been checked")
                  #overview[i] <- paste0(sites()[i], ": all images have been checked")
                }
            }
          }
        }
      }
      }
      colnames(overview) <- c("", "")
      overview <- overview
    })
    
    ## display the overview table
    output$overview_out <- renderTable(overview())
    
    #### GET DATA -------
    
    ## load data (automatic classifications)
    dat_complete <- eventReactive(input$start, {
      
      filenames <- dir(input.dir())
      myfile<-read.table(paste(input.dir(), grep(input$site, filenames, value = TRUE), sep = "/"), header = TRUE)
      
      ## add categories to classification file
      myfile$class_id<-ifelse(myfile$guess1==0, "bad_quality", 
                              ifelse(myfile$guess1==1, "empty", 
                                     ifelse(myfile$guess1==2, "bird", 
                                            ifelse(myfile$guess1==3, "vole", 
                                                   ifelse(myfile$guess1==4, "least_weasel", 
                                                          ifelse(myfile$guess1==5, "lemming", 
                                                                 ifelse(myfile$guess1==6, "shrew", 
                                                                        ifelse(myfile$guess1==7, "stoat", NA))))))))
      
      toto <- myfile
    })
      
      
    ## prepare data
    dat <- eventReactive(dat_complete(), {
      
      ## subset images with low confidence (specified) and/or the specified class
      if (input$class=="all") {
        con.low <- dat_complete()[dat_complete()$confidence1 <= input$confidence,] # all images that have been classified with the specified confidence
      } else {
        con.low <- dat_complete()[dat_complete()$confidence1 <= input$confidence & dat_complete()$class_id==input$class,]
      } 
     
      
      ## load file with results
      results<-read.table(paste(output.dir(), input$year, results.name(), sep = "/" ), header = TRUE, colClasses = "character") 
      
      ## load files from quality check
      qual.names <- dir(qual.check.dir())
      
      if (length(qual.names) == 0) {
        qual <- NULL
      } else {
        qual <- map_dfr(paste(qual.check.dir(), qual.names, sep = "/"), read.table, header = TRUE)
      }
      
      
      ## take out images that have been classified manually already
      if (is.null(qual)) {
        con.low<-con.low[!con.low$filename %in% c(results$filename),]
      } else {
        con.low<-con.low[!con.low$filename %in% c(results$filename, qual$filename),]
      }
    })
    
    
    observeEvent(input$start, {
      updateTabsetPanel(session, "inTabset", selected = "Classification")
    })
    
    observeEvent(dat(), {
      shinyalert(title = input$site, 
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
        pic <- readJPEG(paste(image.dir(), dat()$filename[i()], sep = "/"), native = TRUE)
        par(mar = c(7.1, 0.1, 2.1, 0.1))
        par(xpd=NA)
        plot(1:2, type='n', axes = FALSE, xlab = "")
        rasterImage(pic, 1, 1, 2, 2)
        #plot(pic, axes = FALSE, ylim = c(1024,0))
        mtext(dat()$filename[i()], 1, line = 2, cex = 1.5)
        mtext(paste(dat()$class_id[i()], "(Confidence =", round(dat()$confidence1[i()], digits = 3), ")"), 1, line = 4, cex = 1.5)
        previous <- which(dat_complete()$filename == dat()$filename[i()])
        mtext(paste("previous image:", dat_complete()$class_id[previous-1], "(Confidence =", round(dat_complete()$confidence1[previous-1], digits = 3), ")"), 1, line = 6, cex = 1.5)
      }
    })
    
    ## message if all images are checked
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
        results<-read.table(paste(output.dir(), input$year, results.name(), sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
        results<-add_row(results, site = dat()$site[i()], year = as.character(input$year), observer = input$observer, filename = dat()$filename[i()], class_id = class_val$class, comment = input$comment) # add results
        write.table(results, paste(output.dir(), input$year, results.name(), sep = "/"), row.names = FALSE) # save results
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
      print(i)
      new_i <- i()+1
      i(new_i)
    })
    
    ## stop the app  
    observeEvent(input$stop_app, {stopApp()})
    observeEvent(input$stop_app2, {stopApp()})
    
    ## go back to previous image (and delete the annotation)
    observeEvent(input$back, {
      results<-read.table(paste(output.dir(), input$year, results.name(), sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
      results<-results[-nrow(results),] # add results
      write.table(results, paste(output.dir(), input$year, results.name(), sep = "/"), row.names = FALSE) # save results
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


