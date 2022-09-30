##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Select training images
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



## This script is used to select new training images. 
## The images are selected based on automatic classification and thus, assumes that the images have been classified automatically using the script '02_classify_images_automatically.R'. 
## If the images have been classified with another model and the model output files have a different structure, the script has to be modified.

## Open the Rstudio project 'camera_trap_workflow.Rproj', then open the script '11_select training_images.R'.
## Specify the parameters for selecting training images in the SET-UP part:
## You can select images from a certain site, from a certain class and/or below a certain confidence level.
## The selected images will presented for manual labeling in a new plotting window.
## Select the correct label by clicking on the label. The included labels are empty, bad quality, vole, lemming, shrew, stoat, least weasel and bird. The script can be modified to include other labels.
## The image will then be copied to the folder for the correct class within in the folder for training images
## If you don't want to add the image to the training data set, click NEXT to skip the image.
## A file with all images that have been checked (also the images that were skipped) will be saved in the folder for training images to avoid checking the same images again.


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
output.dir <- "data/training_images"  # write here the path to the folder where a file with all checked images will be saved

# set locality
locality <- "vestre_jakobselv"   # write here the name of the locality (folder name) you want to process

## set year
year <- 2020  # write here which year of images (folder name) you want to process

## select site
site <- NA  # write here the names of the site (folder name) you want to process, set this to NA if you want to select from images from all sites,

## select class
class_id <- "all"  # write here which class you want to check, use 'all' to check all images

## select confidence levels
confidence <- 0.9  # write here the confidence threshold, images with a confidence below the threshold will be selected.
## use a value above 1 (e.g. 1.1) to select all images

## set observer
observer <- "hb"  # write here your initials, the observer will be included in the output file

## specify number of images that should be select
n = 10  # select here the number of images you want to select per class and site (if site is specified), works only if class is not 'all'



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PREPARE DATA  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load data
if (is.na(site)) {
  auto <- map_dfr(dir(paste(input.dir, locality, year, sep = "/"), full.names = TRUE), read.table, header = TRUE)
} else {
  filenames <- dir(paste(input.dir, locality, year, sep = "/"), full.names = TRUE)
  auto <- read.table(grep(site, filenames, value = TRUE), header = TRUE)
}

## add categories to classification file
auto$class_id<-ifelse(auto$guess1==0, "bad_quality", 
                      ifelse(auto$guess1==1, "empty", 
                             ifelse(auto$guess1==2, "bird", 
                                    ifelse(auto$guess1==3, "vole", 
                                           ifelse(auto$guess1==4, "least_weasel", 
                                                  ifelse(auto$guess1==5, "lemming", 
                                                         ifelse(auto$guess1==6, "shrew", 
                                                                ifelse(auto$guess1==7, "stoat", NA))))))))

## get filenames of images that have been selected
training_images<-c(dir(paste(output.dir, "bad_quality", sep="/")), dir(paste(output.dir, "empty", sep="/")), dir(paste(output.dir, "vole", sep="/")), 
                   dir(paste(output.dir, "lemming", sep="/")), dir(paste(output.dir, "least_weasel", sep="/")), dir(paste(output.dir, "stoat", sep="/")), 
                   dir(paste(output.dir, "shrew", sep="/")), dir(paste(output.dir, "bird", sep="/")))



## load file with images that have been checked
checked_name <- paste0("checked_images_", year, ".txt")

if (file.exists(paste(output.dir, checked_name, sep = "/"))) {
  checked<-read.table(paste(output.dir, checked_name, sep = "/"), header = TRUE)
} else {
  checked <- data.frame(filename = "ggg", selected = "yes", locality = "a", observer = "a", class_id = "xx")
  checked <- checked[-1,]
  write.table(checked, paste(output.dir, checked_name, sep = "/"), row.names = FALSE)
  
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
dat <- dat[!dat$filename %in% checked$filename,]
dat <- dat[!dat$filename %in% training_images,]

## randomize order of images to avoid selecting only images from the same box
dat <- dat[sample(1:nrow(dat), nrow(dat)),]



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SELECT TRAINING IMAGES  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## the selected images will be displayed and can be added to the training images by clicking on the correct label

for(i in 1:nrow(dat)){
  
  ## open plotting window if it is the first image
  if(i==1){
    x11(height = 20, width = 40)
    par(xpd=NA)
  }
  
  ## repeat previous image if 'back' was hit
  if (i!=1){
    if (xx>(1300) & xx<(1500) & yy>250 & yy<350){
      i<-i-2
      
      ## delete image from training folder
      file.remove(pre.dir)
      
      ## delete image from result file
      checked<-read.table(paste(output.dir, checked_name, sep = "/"), header = TRUE)
      checked<-checked[-which(checked$filename==pre.name),]
      write.table(checked, paste(output.dir, checked_name, sep = "/"), row.names = FALSE)
    }
  }
  
  
  ## check if the image has already been selected
  if(!dat$filename[i] %in% training_images){
    
    ## load image if it has not been selcted and choose if it should be part of the training dataset
    pic <- load.image(paste(image.dir, locality, dat$site[i], year, dat$filename[i], sep = "/"))
    pic2 <- resize(pic, 1280, 900, 1, 3)
    
    ## plot image
    plot(pic2, axes = FALSE)
    
    mtext( dat$filename[i], side = 3, line = 1, cex = 1.4, font = 2)
    
    mtext(paste(paste(toupper(substr(dat$class_id[i], 1, 1)), substr(dat$class_id[i], 2, nchar(dat$class_id[i])), sep = ""), 
                "   (confidence: ", dat$confidence1[i], ")", sep = ""), 
          side = 1, line = 1, cex = 1.1)
    
    mtext("select as", side = 3, line = 1, cex = 1.2, at = -150)
    
    text(x=-150, y=50, labels = "Bad quality", cex=1.2)
    text(x=-150, y=130, labels = "Empty", cex=1.2)
    text(x=-150, y=210, labels = "Vole", cex=1.2)
    text(x=-150, y=290, labels = "Lemming", cex=1.2)
    text(x=-150, y=370, labels = "Least weasel", cex=1.2)
    text(x=-150, y=450, labels = "Stoat", cex=1.2)
    text(x=-150, y=530, labels = "Shrew", cex=1.2)
    text(x=-150, y=610, labels = "Bird", cex=1.2)
    
    rect(-250, 20, -50, 80)
    rect(-250, 100, -50, 160)
    rect(-250, 180, -50, 240)
    rect(-250, 260, -50, 320)
    rect(-250, 340, -50, 400)
    rect(-250, 420, -50, 480)
    rect(-250, 500, -50, 560)
    rect(-250, 580, -50, 640)
    
    text(x=1400, y=100, labels = "NEXT", cex=1.2)
    text(x=1400, y=300, labels = "BACK", cex=1.2)
    text(x=1400, y=500, labels = "STOP", cex=1.2)
    
    rect(1300, 50, 1500, 150)
    rect(1300, 250, 1500, 350)
    rect(1300, 450, 1500, 550)
    
    
    ## start locator 
    loc<-locator(n=1)
    yy<-loc$y
    xx<-loc$x
    
    ## save class id
    class<-ifelse(xx>(-250) & xx<(-50) & yy>20 & yy<80, "bad_quality", 
                  ifelse(xx>(-250) & xx<(-50) & yy>100 & yy<160, "empty", 
                         ifelse(xx>(-250) & xx<(-50) & yy>180 & yy<240, "vole", 
                                ifelse(xx>(-250) & xx<(-50) & yy>260 & yy<320, "lemming", 
                                       ifelse(xx>(-250) & xx<(-50) & yy>340 & yy<400, "least_weasel", 
                                              ifelse(xx>(-250) & xx<(-50) & yy>420 & yy<480, "stoat",
                                                     ifelse(xx>(-250) & xx<(-50) & yy>500 & yy<560, "shrew", 
                                                            ifelse(xx>(-250) & xx<(-50) & yy>580 & yy<640, "bird",
                                                                   ifelse(xx>(-250) & xx<(-50) & yy>660 & yy<720, "tail_vole", 
                                                                          ifelse(xx>(-250) & xx<(-50) & yy>740 & yy<800, "tail_lemming",
                                                                                 ifelse(xx>(-250) & xx<(-50) & yy>820 & yy<880, "mustelid", NA)))))))))))
    
    ##### COPY IMAGE AND FILL IN RESULTS IF IMAGE WAS SELECTED
    
    if (xx>(-250) & xx<(-50) & yy>20 & yy<80 |
        xx>(-250) & xx<(-50) & yy>100 & yy<160 |
        xx>(-250) & xx<(-50) & yy>180 & yy<240 |
        xx>(-250) & xx<(-50) & yy>260 & yy<320 |
        xx>(-250) & xx<(-50) & yy>340 & yy<400 |
        xx>(-250) & xx<(-50) & yy>420 & yy<480 |
        xx>(-250) & xx<(-50) & yy>500 & yy<560 |
        xx>(-250) & xx<(-50) & yy>580 & yy<640 |
        xx>(-250) & xx<(-50) & yy>660 & yy<720 |
        xx>(-250) & xx<(-50) & yy>740 & yy<800 |
        xx>(-250) & xx<(-50) & yy>820 & yy<880){
      
      ## copy file in trainingsfolder
      file.copy(paste(image.dir, locality, dat$site[i], year, dat$filename[i], sep = "/"), 
                paste(output.dir, class, dat$filename[i], sep = "/"))
      
      ## save path to copied image in case it should be deleted
      pre.dir<-paste(output.dir, class, dat$filename[i], sep = "/")
      
      ## save results
      checked <- read.table(paste(output.dir, checked_name, sep = "/"), header = TRUE, colClasses = "character")
      checked <- add_row(checked, filename = dat$filename[i], selected = "yes", class_id = class, locality = locality, observer = observer)
      write.table(checked, paste(output.dir, checked_name, sep = "/"), row.names = FALSE)
      
      ## save image name of copied image in case it should be deleted
      pre.name <- dat$filename[i]
    }
    
    
    ## save image name if image was not selected -> if 'NEXT' was hit
    if (xx>(1300) & xx<(1500) & yy>50 & yy<150){
      checked <- read.table(paste(output.dir, checked_name, sep = "/"), header = TRUE, colClasses = "character")
      checked <- add_row(checked, filename = dat$filename[i], selected = "no", class_id = NA,  locality = locality, observer = observer)
      write.table(checked, paste(output.dir, checked_name, sep = "/"), row.names = FALSE)
      
    }
    
    
    ## stop loop if stop was hit
    if (xx>(1300) & xx<(1500) & yy>450 & yy<550){
      dev.off()
      print(paste(i, "images checked"))
      break
    }
    
    if (is.na(site)) {
      if (length(which(checked$class_id == class_id & checked$locality == locality & checked$selected == "yes")) > n) {
        print(paste(n, "images checked"))
        break
      }
    } else {
      if (length(which(checked$class_id == class_id & checked$locality == locality & checked$selected == "yes" & site == site)) > n) {
        print(paste(n, "images checked"))
        break
      }
    }
  }
}
 






