##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Quality check automatic image labels
## Author: Hanna Böhner
## Date: 2022-09-29
##
## This script is part of the semi-automatic workflow for processing camera trap images and comes with the publication 
## Böhner H., Kleiven E. F., Ims R. A., Soininen E. M. (2022) A semi-automatic workflow to process camera trap images, BioRxiv
## It comes without any warranty and is free for everyone to use or modify
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


## Instructions ------------

## To test the workflow, download or clone the github-repository https://github.com/hannaboe/camera_trap_workflow. 
## This folder contains all scripts needed for the workflow, the small mammal classification model and a small test data set. 
## The files are organized as we suggest in 'processing_camera_trap_images_step_by_step.pdf'. 

## To run the workflow on your own images without adjusting the script, the files should be organized as described in 'processing_camera_trap_images_step_by_step'. 
## If the files are organized differently, the script has to be adapted.


## This script is used to verify automatic image labels labels and thus, assumes that the images have been classified automatically using the script 02_classify_images_automatically.R. 
## If the images have been classified with another model and the model output files have a different structure, the script has to be modified.

## Open the Rstudio project 'camera_trap_workflow.Rproj', then open the script 03_quality_check.R and run the script
## Specify the parameters for the quality check in the SET-UP part.
## You can either select random images, images per class or images per confidence class.
## The selected images will presented for manual labeling in a new plotting window.
## Select the correct label by clicking on the label. The included labels are empty, bad quality, vole, lemming, shrew, stoat, least weasel and bird. The script can be modified to include other labels.
## You can also skip the image (NEXT), go back to the last images in case of a mistake (BACK), check the previous or the next images in case it is difficult to identify the species.
## One file per locality with the manual labels will be saved in the specified folder.


## The file contains the following columns
##  site          camera trap siteID
##  year          year when the image was taken (when the image was downloaded)
##  filename      image name
##  class_id      manual image label
##  type          type of quality check (random, per class or per confidence class)
##  guess1        automatic image label, class with the highest confidence extracted from the model output
##  confidence1   confidence that the image belongs to the given class (guess1)
##  observer      initial of the person who labeled the images
##  comment       any kind of remarks

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries
library(imager)  # for loading images
library(tidyverse)   # for data handling

## set options
OldOpt = options("locatorBell") # save options
options(locatorBell = FALSE) # turn off locator bell

## set directories
input.dir <- "data/automatic_classification/small_mammal_classification_model_2022"  # write here the path to the folder where the model output files are saved
image.dir <- "data/images_renamed"  # write here the path to the folder where all images are saved
results.dir <- "data/quality_check/small_mammal_classification_model_2022"  # write here the path to the folder where the files with the manual labels should be saved

# set locality
locality <- "vestre_jakobselv"   # write here the name of the locality (folder name) you want to process

## set year
year <- 2020  # write here which year of images (folder name) you want to process

## select the type of quality check
type <- "random"  # write here the type of quality check, options are 'random', 'class' or 'confidence'
## if type is 'random', x random images will be selected and displayed for manual classification
## if type is 'class', x random images of each class (based on model labels) will be selected and displayed for manual classification
## if type is 'confidence', x random images of each confidence class (0-0.1, 0.1-0.2 ...) will be selected and displayed for manual classification 

## select number of images
no_images <- 500  # write here the number of images for the quality check
## if type is 'random', this is the total number of images
## if type is 'class' or 'confidence', this is the number of images per class or confidence class

## set observer
observer <- "hb"  # write here your initials, the observer will be included in the output file



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PREPARE DATA  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


## load data
myfile <- map_dfr(dir(paste(input.dir, locality, year, sep = "/"), full.names = TRUE), read.table, header = TRUE)

## add categories to classification file
myfile$class_id<-ifelse(myfile$guess1==0, "bad_quality", 
                        ifelse(myfile$guess1==1, "empty", 
                               ifelse(myfile$guess1==2, "bird", 
                                      ifelse(myfile$guess1==3, "vole", 
                                             ifelse(myfile$guess1==4, "least_weasel", 
                                                    ifelse(myfile$guess1==5, "lemming", 
                                                           ifelse(myfile$guess1==6, "shrew", 
                                                                  ifelse(myfile$guess1==7, "stoat", NA))))))))

## make empty file to store the manual labels if it doesn't exist yet
results_name <- paste0("quality_check_", locality, "_", year, ".txt")

if (!dir.exists(paste(results.dir, year, sep = "/"))) dir.create(paste(results.dir, year, sep = "/"))  # create folder for the year

if (!results_name %in% dir(paste(results.dir, year, sep = "/"))) {
  results<-data.frame(site = "0", year = "0", filename= "0", class_id = "0", type = "0", guess1 = "0", confidence1 = "1", observer = "0",  comment = "0")
  results<-results[-1,]
  write.table(results, paste(results.dir, year, results_name, sep = "/"), row.names = FALSE)
}


## select images
classes <- unique(myfile$class_id)  # all classes
conf <- seq(from = 0, to = 1, length =11)  # all confidence classes

if (type == "random") {
  results <- read.table(paste(results.dir, year, results_name, sep = "/"), header = TRUE, colClasses = "character")  # load the file with manual labels
  n_select <- no_images - nrow(results[results$type == "random",])  
  
  if (nrow(results != 0)) {
    dat <- myfile[!myfile$filename %in% results$filename,]
  } else {
    dat <- myfile
  }
  
  if (nrow(dat) > n_select) dat <- dat[sample(nrow(dat), n_select),]
  
} else if (type == "class") {
  results <- read.table(paste(results.dir, year, results_name, sep = "/"), header = TRUE, colClasses = "character")  # load the file with manual labels
  
  dat <- c()
  for (i in 1:length(classes)) {
    n_select <- no_images - nrow(results[results$type == "class" & results$guess1 == classes[i],])
    
    if (nrow(results) != 0) {
      dat[[i]] <- myfile[(!myfile$filename %in% results$filename) & myfile$class_id == classes [i],]
    } else {
      dat[[i]] <- myfile[myfile$class_id == classes[i],]
    }
    
    if(nrow(dat[[i]]) > n_select) dat[[i]] <- dat[[i]][sample(nrow(dat[[i]]), n_select),]
  }
  dat <- do.call(rbind, dat)
  
} else if (type == "confidence") {
  results<-read.table(paste(results.dir, year, results_name, sep = "/" ), header = TRUE, colClasses = "character") # load the file with with results
  
  dat <- c()
  for (i in 1:(length(conf)-1)) {
    n_select <- no_images - nrow(results[results$type == "confidence" & results$confidence1 > conf[i] & results$confidence1 <= conf[i+1],])
    
    if(nrow(results) != 0) {
      dat[[i]] <- myfile[(!myfile$filename %in% results$filename) & myfile$confidence1 > conf[i] & myfile$confidence1 <= conf[i+1],]
    } else {
      dat[[i]] <- myfile[myfile$confidence1 > conf[i] & myfile$confidence1 <= conf[i+1],]
    }
    if(nrow(dat[[i]]) > n_select) dat[[i]] <- dat[[i]][sample(nrow(dat[[i]]), n_select),]
  }
  dat <- do.call(rbind, dat)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## LABEL IMAGES  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## the selected images will be displayed and can be labeled by clicking on the correct label

for(i in 1:nrow(dat)){
  
  if(i==1){
    x11(height = 1024/72, width = 1500/72)
    par(xpd=NA)
  }
  
  ## repeat previous image if 'back' was hit
  if (i!=1 & i!=2){
    if (xx>1560 & xx<1740 & yy>110 & yy<190){
      i<-i-2
      
      ## delete last entry from results
      results<-read.table(paste(results.dir, year, results_name, sep = "/" ), header = TRUE) # load file
      results<-results[results$filename!=dat$filename[i],]
      write.table(results, paste(results.dir, year, results_name, sep = "/"), row.names = FALSE) # save file
    }
  }
  
  ## check if image has been checked already
  results<-read.table(paste(results.dir, year, results_name, sep = "/" ), header = TRUE) # load file with checked images
  
  if (dat$filename[i] %in% results$filename) {
    next
  } else {
    
    ## load image
    pic <- load.image(paste(image.dir, locality, dat$site[i], year, dat$filename[i], sep = "/")) 
    pic <- resize(pic, 1500, 1024, 1, 3)
    
    ## make interactive plot
    
    plot(pic, axes = FALSE, ylim = c(1024,0))
    
    #text(x=-280, y=-70, labels = paste("Guess 1:", sub$guess1[i]), adj = 0, cex = 1.2)
    #text(x=-280, y=-30, labels = paste("Confidence:", sub$confidence1[i]), adj = 0, cex=1.2)
    mtext(dat$filename[i], 1)
    
    text(x=-160, y=50, labels = "Bad quality", cex=1.3)
    text(x=-160, y=150, labels = "Empty", cex=1.3)
    text(x=-160, y=250, labels = "Vole", cex=1.3)
    text(x=-160, y=350, labels = "Least weasel", cex=1.3)
    text(x=-160, y=450, labels = "Lemming", cex=1.3)
    text(x=-160, y=550, labels = "Stoat", cex=1.3)
    text(x=-160, y=650, labels = "Shrew", cex=1.3)
    text(x=-160, y=750, labels = "Bird", cex=1.3)
    text(x=-160, y=900, labels = "Difficult", cex=1.3)
    
    text(x=1650, y=50, labels = "NEXT", cex =1.3)
    text(x=1650, y=150, labels = "BACK", cex =1.3)
    text(x=1650, y=250, labels = "STOP", cex =1.3)
    
    rect(-250, 90, -70, 10)
    rect(-250, 190, -70, 110)
    rect(-250, 290, -70, 210)
    rect(-250, 390, -70, 310)
    rect(-250, 490, -70, 410)
    rect(-250, 590, -70, 510)
    rect(-250, 690, -70, 610)
    rect(-250, 790, -70, 710)
    rect(-250, 940, -70, 860)
    
    rect(1560, 90, 1740, 10)
    rect(1560, 190, 1740, 110)
    rect(1560, 290, 1740, 210)
    
    text(x = 1650, y = 550, labels = "Next \n image", cex = 1.3)
    text(x = 1650, y = 650, labels = "Previous \n image", cex = 1.3)
    
    rect(1560, 590, 1740, 510)
    rect(1560, 690, 1740, 610)
    
    text(x = 1650, y = 450, labels = "Check the next or the \n previous image if it is \n difficult to identify the species", cex = 1)
    
    xy<-locator(n=1)
    xx<-xy$x
    yy<-xy$y
    
    if (xx > 1560 & xx < 1740 & ((yy > 510 & yy< 590) |  (yy > 610 & yy< 690))) {
      image_pos <- which(myfile$filename == dat$filename[i])
      if (xx > 1560 & xx < 1740 & yy > 510 & yy< 590) new_image_name <- myfile$filename[image_pos+1]
      if (xx > 1560 & xx < 1740 & yy > 610 & yy< 690) new_image_name <- myfile$filename[image_pos-1]
      
      new_pic <- load.image(paste(image.dir, locality, dat$site[i], year, new_image_name, sep = "/"))
      new_pic <- resize(new_pic, 1500, 1024, 1, 3)
      
      windows(height = 1024/72, width = 1500/72)
      par(xpd=NA)  
      plot(new_pic, axes = FALSE, ylim = c(1024,0))
      
      mtext(new_image_name, 1)
      #mtext(paste(myfile$class_id[myfile$filename == new_image_name], "(Confidence =", round(myfile$confidence1[myfile$filename == new_image_name], digits = 3), ")"), 1, line = -5)
      #mtext("", 1, line = 2)
      
      text(x=1650, y=50, labels = "CLOSE", cex =1.3)
      rect(1560, 90, 1740, 10)
      
      current.dev <- dev.cur()
      
      xy<-locator(n=1)
      dev.off()
      
      
      
      dev.set(dev.prev())
      
      xy<-locator(n=1)
      xx<-xy$x
      yy<-xy$y
    }
    
    ## save class id
    correct_class<-ifelse(xx>(-250) & xx<(-70) & yy>10 & yy<90, "bad quality", 
                          ifelse(xx>(-250) & xx<(-70) & yy>110 & yy<190, "empty", 
                                 ifelse(xx>(-250) & xx<(-70) & yy>210 & yy<290, "vole", 
                                        ifelse(xx>(-250) & xx<(-70) & yy>310 & yy<390, "least_weasel", 
                                               ifelse(xx>(-250) & xx<(-70) & yy>410 & yy<490, "lemming", 
                                                      ifelse(xx>(-250) & xx<(-70) & yy>510 & yy<590, "stoat", 
                                                             ifelse(xx>(-250) & xx<(-70) & yy>610 & yy<690, "shrew", 
                                                                    ifelse(xx>(-250) & xx<(-70) & yy>710 & yy<790, "bird",
                                                                           ifelse(xx>(-250) & xx<(-70) & yy>860 & yy<940, "difficult",NA)))))))))
  }
  
  ## add correct class id to results
  if(!is.na(correct_class)) {
    results<-read.table(paste(results.dir, year, results_name, sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
    results<-add_row(results, site = dat$site[i], year = as.character(dat$year[i]), filename=dat$filename[i], class_id=correct_class, type = type,
                     guess1 = dat$class_id[i], confidence1 = as.character(dat$confidence1[i]), observer = observer) # add results
    write.table(results, paste(results.dir, year, results_name, sep = "/"), row.names = FALSE) # save results
  }
  
  ## stop loop if stop was hit
  if (xx>1560 & xx<1740 & yy>210 & yy<290) {
    
    ## save results  
    write.table(results, paste(results.dir, year, results_name, sep = "/"), row.names = FALSE)  # save results
    dev.off()
    break
  }
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## CHECK MANUAL LABELS  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Check that all images are labeled. If not enough images have been labeled manually (for example if images were skipped), 
## select new images (run the par 'DATA PREPARATION' and label them (run the part 'LABEL IMAGES'))

## load the result file
results<-read.table(paste(results.dir, year, results_name, sep = "/" ), header = TRUE, colClasses = "character")

## check random 
nrow(results[results$type == "random",])  # is this the correct number?

## check class
classes <- unique(results$class_id) ## all classes
for (i in 1:length(classes)) {
  no_checked <- nrow(results[results$guess1 == classes[i] & results$type == "class",])
  print(paste0(classes[i], ": ", no_checked, " images"))
}
# are the numbers correct?

## check confidence
conf <- seq(from = 0, to = 1, length =11)  # all confidence classes
for (i in 1:(length(conf)-1)) {
  no_checked <- nrow(results[results$type == "confidence" & results$confidence1 > conf[i] & results$confidence1 <= conf[i+1],])
  print(paste0(conf[i], "-", conf[i+1], ": ", no_checked, " images"))
}
# are the numbers correct?

