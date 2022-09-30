##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Create files for training, validation and testing
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


## This script is used to to create files that contain image names and class of all images that will be used for training, validation and testing when training a classification model 
## All images that should be used for training, validation and testing should have been selected and copied to a folder for the category as described in '11_select_training_images.R' 
## If the images are organized in another way or the output files should have different a structure, the script has to be modified.

## Open the Rstudio project 'camera_trap_workflow.Rproj', then open the script '12_create training_files.R' and run the script.
## The image dataset will be split in training images, validation images and test images.
## 80 percent of the images will be used for training, 10 percent for validation and 10 percent for testing in this script.
## However, the number split should be adjusted depending on the structure of the dataset.
## For example, you might want to make sure that all classes are included in the validation and test dataset.
## 3 files (training, validation and test) that contain the path to the image (folder name and file name) as well as the class will be created.
## The files will then be saved in the folder 'training_files'.
## The classes have to be converted to numbers from 0 to n, where n is the number of classes included in the dataset.

## We will use the following conversion:

## bad quality    -   0
## empty          -   1
## bird           -   2
## vole           -   3
## least weasel   -   4
## lemming        -   5
## shrew          -   6
## stoat          -   7


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## set directories
input.dir <- "data/training_images"  # write here the path to the folder where all training images are saved
output.dir <- "data/training_files"  # write here the path to the folder where a the files used for training a classification model should be saved

## get the names of all classes
classes <- list.dirs(input.dir, full.names = FALSE, recursive = FALSE)

## get all image names
bad_quality <- dir(paste(input.dir, "bad_quality", sep = "/"))
empty <- dir(paste(input.dir, "empty", sep = "/"))
bird <- dir(paste(input.dir, "bird", sep = "/"))
vole <- dir(paste(input.dir, "vole", sep = "/"))
least_weasel <- dir(paste(input.dir, "least_weasel", sep = "/"))
lemming <- dir(paste(input.dir, "lemming", sep = "/"))
shrew <- dir(paste(input.dir, "shrew", sep = "/"))
stoat <- dir(paste(input.dir, "stoat", sep = "/"))



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## MAKE THE FILES  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


## make a dataframe with all image names
all_images <- data.frame(filename = c(bad_quality, empty, bird, vole, least_weasel, lemming, shrew, stoat))

## add class to the data frame
all_images$class = ifelse(all_images$filename %in% bad_quality, 0, 
                          ifelse(all_images$filename %in% empty, 1, 
                                 ifelse(all_images$filename %in% bird, 2, 
                                        ifelse(all_images$filename %in% vole, 3, 
                                               ifelse(all_images$filename %in% least_weasel, 4, 
                                                      ifelse(all_images$filename %in% lemming, 5, 
                                                             ifelse(all_images$filename %in% shrew, 6, 
                                                                    ifelse(all_images$filename %in% stoat, 7, NA))))))))

## add folder names (class) to images names
all_images$filename = ifelse(all_images$filename %in% bad_quality, paste("bad_quality", all_images$filename, sep = "/"), 
                          ifelse(all_images$filename %in% empty, paste("empty", all_images$filename, sep = "/"), 
                                 ifelse(all_images$filename %in% bird, paste("bird", all_images$filename, sep = "/"), 
                                        ifelse(all_images$filename %in% vole, paste("vole", all_images$filename, sep = "/"), 
                                               ifelse(all_images$filename %in% least_weasel, paste("least_weasel", all_images$filename, sep = "/"), 
                                                      ifelse(all_images$filename %in% lemming, paste("lemming", all_images$filename, sep = "/"), 
                                                             ifelse(all_images$filename %in% shrew, paste("shrew", all_images$filename, sep = "/"), 
                                                                    ifelse(all_images$filename %in% stoat, paste("stoat", all_images$filename, sep = "/"), NA))))))))

## separate training, validation and test images
training_rows <- sample(nrow(all_images), nrow(all_images)*0.8)  # random selection of 80 percent of the images for training
val_test_rows <- which(!1:nrow(all_images) %in% training_rows)   # images not selected for training
validation_rows <- sample(val_test_rows, length(val_test_rows)*0.5)   # 50 percent of these images will be used for validation during training
test_rows <- val_test_rows[!val_test_rows %in% validation_rows]  # the other 50 percen  will be used for testing

training_data <- all_images[training_rows,]
validation_data <- all_images[validation_rows,]
test_data <- all_images[test_rows,]


## checck that the same image is not included in two of the datasets
which(training_data$filename %in% validation_data$filename)  # should be 0
which(training_data$filename %in% test_data$filename)  # should be 0
which(test_data$filename %in% validation_data$filename)  # should be 0

## check the sturcture of the datasets
table(training_data$class)
table(validation_data$class)
table(test_data$class)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SAVE THE FILES  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

write.table(training_data, paste(output.dir, "training_data.txt", sep = "/"), row.names = FALSE)
write.table(validation_data, paste(output.dir, "validation_data.txt", sep = "/"), row.names = FALSE)
write.table(test_data, paste(output.dir, "test_data.txt", sep = "/"), row.names = FALSE)


