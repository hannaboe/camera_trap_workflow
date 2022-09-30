##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Format final data file
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


## This script is used to combine automatic and manual image labels to a final data file that can then be used in ecological analyses.
## The script assumes that the images have been processed with the provided scripts, if the processing pipeline was changed, this scrip needs to be adapted.

## Open the Rstudio project 'camera_trap_workflow.Rproj', then open the script 06_format_final_data_file.R and run the script
## Specify the parameters in the SET-UP part.
## You can select the locality and the year from which the data should be formatted.
## The automatic and the manual image label will be combined (in to separate columns), metadata will be added and the data will be converted to long format
## One file per locality and year will be saved in the specified folder.


## The file contains the following columns
## locality                     locality of the camera traps
##  site                        camera trap siteID
##  date                        date when the image was taken
##  time                        time when the image was taken
##  image_name                  image name
##  class_id                    classes included in the data set
##  presence_automatic          presence (1) or absence (2) of the class on the image from automatic classification
##  presence_manual             presence (1) or absence (2) of the class on the image from manual classification (from quality check or corrected image label)
##  confidence_automatic        confidence of the automatic image label
##  observer_manual             initials of the person who reviewed the image manually
##  type_manual_classification  reason for manual classification (quality check or corrected image label)
##  comment                     any kind of remarks



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries
library(plyr)  # for data handling
library(tidyverse)   # for data handling


## set directories
auto.dir <- "data/automatic_classification/small_mammal_classification_model_2022"  # write here the path to the folder where the model output files are saved
manual.dir <- "data/manual_classification"  # write here the path to the folder where the files with the corrected image labels are saved
qual.check.dir <- "data/quality_check/small_mammal_classification_model_2022"  # write here the path to the folder where the files with the manual labels from the quality check are saved
meta.dir <- "data/metadata"   # write here the path to the folder wherethe files with the image metadata are saved
image.dir <- "data/images_renamed"  # write here the path to the folder where all images are saved
out.dir <- "data/formatted_data"  # write here the path to the folder where the formatted files should be saved

# set locality
locality <- "vestre_jakobselv"   # write here the name of the locality (folder name) you want to process

## set year
year <- 2020  # write here which year of images (folder name) you want to process


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## LOAD DATA  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## get all site_ids -> needed
sites <- dir(paste(image.dir, locality, sep = "/"))

## keep only sites with new images 
keep <- rep(NA, length(sites))
for (i in 1:length(sites)) {
  if (file.exists(paste(image.dir, locality, sites[i], year, sep = "/"))) keep[i] <- i
}
keep <- keep[!is.na(keep)]
sites <- sites[keep]

## load automatic classification files
auto <-  map_dfr(dir(paste(auto.dir, locality, year, sep = "/"), full.names = TRUE), read.table, header = TRUE) 

## load manual classification files
manual <- map_dfr(dir(paste(manual.dir, locality, year, sep = "/"), full.names = TRUE), read.table, header = TRUE, colClasses = "character")

## load quality check file
qual_check <- map_dfr(dir(paste(qual.check.dir, year, sep = "/"), full.names = TRUE), read.table, header = TRUE, colClasses = "character")

## load meta data files
meta <- map_dfr(dir(paste(meta.dir, locality, year, sep = "/"), full.names = TRUE), read.table, header = TRUE, colClasses = "character")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SOME CHECKING  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## check if all images are included in metadata
res <- c()
for (i in 1:length(sites)) {
  images <- dir(paste(image.dir, locality, sites[i], year, sep = "/"), pattern = "JPG")
  if (!all(images %in% meta$NewFileName)) {
    print(paste0(sites[i], ": images missing in metadata"))
  } 
  res[i] <- all(images %in% meta$NewFileName)
}
if (all(res)) print("all images included in metadata")

## check if all images are included in the automatic classification files
all (meta$NewFileName %in% auto$filename)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## FORMAT DATA  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## recode classes to species names or groups that should be used in the formatted files -> this needs to be adapted to match the your classes
auto$guess1 <- recode(auto$guess1, `3` = "cricetidae", `2` = "aves", `4` = "mus_niv", `7` = "mus_erm", `6` = "sor_sp", `5` = "lem_lem",
                         `0` = "bad_quality", `1` = "empty")
manual$class_id <- recode(manual$class_id, vole = "cricetidae", bird = "aves", `least_weasel` = "mus_niv", stoat = "mus_erm", shrew = "sor_sp", lemming = "lem_lem")
qual_check$class_id <- recode(qual_check$class_id, vole = "cricetidae", bird = "aves", `least_weasel` = "mus_niv", stoat = "mus_erm", shrew = "sor_sp", lemming = "lem_lem")

## convert automatic classifications to wide format to get all categories for all images
newfile <-  pivot_wider(auto, names_from = guess1, values_from = confidence1, values_fill = 0)

## set all other categories to NA if image has bad quality, if the bad_quality
classes <- colnames(newfile)[-(1:3)]
for (i in classes) {
  xx <- newfile[,which(colnames(newfile) == i)]
  xx[which(newfile$bad_quality != 0),] <-NA
  newfile[,which(colnames(newfile) == i)] <- xx
}

## add date and time -> check if this is needed or can be added later togheter with other metadata
if (all(meta$v_image_name == newfile$filename)) {
  newfile$date <- gsub(":", "-", substr(meta$DateTimeOriginal, 1, 10))
  newfile$time <- substr(meta$DateTimeOriginal, 12, nchar(meta$DateTimeOriginal))
}

## convert to long format
newfile <- pivot_longer(newfile, cols = classes, names_to = "class_id", values_to = "confidence_automatic")

## add column for presence or absence of a class
newfile$presence_automatic <- ifelse(newfile$confidence_automatic == 0, 0, 
                                       ifelse(is.na(newfile$confidence_automatic), NA, 1))

## set confidence of all other categories to NA
newfile$confidence_automatic[newfile$confidence_automatic == 0] <- NA


## add the corrected images labels (from manual classification) in a new column
newfile$presence_manual <- 0
newfile$observer_manual <- NA

for (i in 1:nrow(manual)) {
  image <- manual$filename[i]
  class <- manual$class_id[i]
  newfile$presence_manual[newfile$filename == image & newfile$class_id == class] <- 1
  newfile$observer_manual[newfile$filename == image] <- manual$observer[i]
}


## add manual classifiations from quality check
for (i in 1:nrow(qual_check)) {
  image <- qual_check$filename[i]
  class <- qual_check$class_id[i]
  newfile$presence_manual[newfile$filename == image & newfile$class_id == class] <- 1
  newfile$observer_manual[newfile$filename == image] <- qual_check$observer[i]
  
}



## set all other categories to NA if image has bad quality
bad_quality <- newfile$filename[newfile$class_id == "bad_quality" & newfile$presence_manual == 1]
newfile$presence_manual[newfile$filename %in% bad_quality & newfile$class_id != "bad_quality"] <- NA

#missing <- ddply(newfile, "filename", function(x) all(x$v_presence_manual == 0 | is.na(x$v_presence_manual)))
#newfile$v_presence_manual[newfile$filename %in% missing$filename[missing$V1]] <- NA

## add column with information about the type of manual classification
newfile$type_manual_classification <- ifelse(newfile$filename %in% qual_check$filename[qual_check$type == "random"], "quality_check_random",
                                               ifelse(newfile$filename %in% qual_check$filename[qual_check$type == "confidence"], "quality_check_confidence_level",
                                                      ifelse(newfile$filename %in% qual_check$filename[qual_check$type == "class"], "quality_check_classes",
                                                             ifelse(newfile$filename %in% manual$filename, "corrected_image_label", NA))))
unique(newfile$type_manual_classification)


## add locality and image_name (instead of filename)
newfile$locality <- locality
newfile$image_name<-newfile$filename


## add comment
newfile$comment<-NA

## check if there are comments from tha manual classification and add them if necessary (or add other comments)
unique(manual$comment)

## keep only necessary columns
newfile2<-newfile[,c("locality", "site", "date", "time", "image_name", 
                     "class_id", "presence_automatic", "presence_manual", "confidence_automatic", "observer_manual", "type_manual_classification", "comment")]

## reorder file
newfile2<-arrange(newfile2, site, image_name, class_id)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## FORMAT DATA  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

head(newfile2)  # look at the file

new_filename <- paste0("image_classification_", locality, "_", year, ".txt")  # write here the name for the formatted file
write.table(newfile2, paste(out.dir, new_filename, sep = "/"), row.names = FALSE)  # save the formatted file


