##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Correct automatic image labels (manual classification)
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



## This script is used to correct automatic image labels and thus, assumes that the images have been classified automatically using the script '02_classify_images_automatically.R'. 
## If the images have been classified with another model and the model output files have a different structure, the script has to be modified.

## Open the Rstudio project 'camera_trap_workflow.Rproj', then open the script '05_correct_model_labels.R' and run the script.
## Specify the parameters for correcting the model labels in the SET-UP part.
## You can select images from a certain site, from a certain class and/or below a certain confidence level.
## The selected images will be presented for manual labeling in a new plotting window.
## Select the correct label by clicking on the label. The included labels are empty, bad quality, vole, lemming, shrew, stoat, least weasel and bird. The script can be modified to include other labels-
## You can also skip the image (NEXT), go back to the last images in case of a mistake (BACK), check the previous or the next images in case it is difficult to identify the species
## One file per site with the manual labels will be saved in the specified folder.


##  The file contains the following columns
##  site          camera trap siteID
##  year          year when the image was taken (when the image was downloaded)
##  filename      image name
##  class_id      manual image label
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
qual.check.dir <- "data/quality_check/small_mammal_classification_model_2022"  # write here the path to the folder where the files with the manual labels from the quality check are saved
results.dir <- "data/manual_classification"  # write here the path to the folder where the files with the manual labels should be saved

# set locality
locality <- "vestre_jakobselv"   # write here the name of the locality (folder name) you want to process

## set year
year <- 2020  # write here which year of images (folder name) you want to process

## select site
site <- "vj_ga_hu_1"  # write here the names of the site (folder name) you want to process

## select class
class_id <- "all"  # write here which class you want to check, use 'all' to check all images

## select confidence levels
confidence <- 1.1  # write here the confidence threshold, images with a confidence below the threshold will be selected.
## use a value above 1 (e.g. 1.1) to select all images

## set observer
observer <- "hb"  # write here your initials, the observer will be included in the outpt file


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PREPARE DATA  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load data
filenames <- dir(paste(input.dir, locality, year, sep = "/"), full.names = TRUE)
auto <- read.table(grep(site, filenames, value = TRUE), header = TRUE)

## add categories to classification file
auto$class_id<-ifelse(auto$guess1==0, "bad_quality", 
                        ifelse(auto$guess1==1, "empty", 
                               ifelse(auto$guess1==2, "bird", 
                                      ifelse(auto$guess1==3, "vole", 
                                             ifelse(auto$guess1==4, "least_weasel", 
                                                    ifelse(auto$guess1==5, "lemming", 
                                                           ifelse(auto$guess1==6, "shrew", 
                                                                  ifelse(auto$guess1==7, "stoat", NA))))))))

## load file with manual labels from the quality check
qual_check_name <- paste0("quality_check_", locality, "_", year, ".txt") # filename
qual_check <- read.table(paste(qual.check.dir, year, qual_check_name, sep = "/"), header = TRUE)  #load data

## delete images that have been quality checked
auto <- auto[!auto$filename %in% qual_check$filename,]

## make empty file to store the manual labels if it doesn't exist yet

if (!dir.exists(paste(results.dir, locality, sep = "/"))) dir.create(paste(results.dir, locality, sep = "/"))  # create folder for locality if it doesn't exist
if (!dir.exists(paste(results.dir, locality, year, sep = "/"))) dir.create(paste(results.dir, locality, year, sep = "/"))  # create folder for the year if it doesn't exist
results_name <- paste0("manual_classification_", site, "_", year, ".txt")
if (!results_name %in% dir(paste(results.dir, locality, year, sep = "/"))) {
  results<-data.frame(site = "0", year = "0", filename= "0", class_id = "0", guess1 = "0", confidence1 = "1", observer = "0",  comment = "0")
  results<-results[-1,]
  write.table(results, paste(results.dir, locality, year, results_name, sep = "/"), row.names = FALSE)
}

## select images
if (class_id == "all") {
  dat <- auto[auto$confidence1 < confidence,]
} else if (class_id %in% auto$class_id) {
  dat <- auto[auto$confidence1 < confidence & auto$class_id == class_id,]
} else {
  print("class does not exist")
}

## delete images that have been checked already
results <- read.table(paste(results.dir, locality, year, results_name, sep = "/"), header = TRUE)
dat <- dat[!dat$filename %in% results$filename,]

## check how many images have to be checked
print(paste(nrow(dat), "images have to be checked"))


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
      results<-read.table(paste(results.dir, locality, year, results_name, sep = "/" ), header = TRUE) # load file
      results<-results[results$filename!=dat$filename[i],]
      write.table(results, paste(results.dir, locality, year, results_name, sep = "/"), row.names = FALSE) # save file
    }
  }
  
  ## check if image has been checked already
  results<-read.table(paste(results.dir, locality, year, results_name, sep = "/" ), header = TRUE) # load file with checked images
  
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
      image_pos <- which(auto$filename == dat$filename[i])
      if (xx > 1560 & xx < 1740 & yy > 510 & yy< 590) new_image_name <- auto$filename[image_pos+1]
      if (xx > 1560 & xx < 1740 & yy > 610 & yy< 690) new_image_name <- auto$filename[image_pos-1]
      
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
    results<-read.table(paste(results.dir, locality, year, results_name, sep = "/" ), header = TRUE, colClasses = "character") # load file with with results
    results<-add_row(results, site = dat$site[i], year = as.character(dat$year[i]), filename=dat$filename[i], class_id=correct_class,
                     guess1 = dat$class_id[i], confidence1 = as.character(dat$confidence1[i]), observer = observer) # add results
    write.table(results, paste(results.dir, locality, year, results_name, sep = "/"), row.names = FALSE) # save results
  }
  
  ## stop loop if stop was hit
  if (xx>1560 & xx<1740 & yy>210 & yy<290) {
    
    ## save results  
    write.table(results, paste(results.dir, locality, year, results_name, sep = "/"), row.names = FALSE)  # save results
    dev.off()
    break
  }
}
