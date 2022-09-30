##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Classify images automatically
## Author: Hanna Böhner
## Date: 2022-09-29
##
## This script is part of the semi-automatic workflow for processing camera trap images and comes with the publication 
## Böhner H., Kleiven E. F., Ims R. A., Soininen E. M. (2022) A semi-automatic workflow to process camera trap images, BioRxiv
## It comes without any warranty and is free for everyone to use or modify
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


## Instructions ------------

## To test the workflow, download or clone the github-repository https://github.com/hannaboe/camera_trap_workflow. 
## This folder contains all scripts needed for the workflow, the small mammal classification and a small test data set. 
## The files are organized as we suggest in 'processing_camera_trap_images_step_by_step.pdf'. 

## To run the workflow on your own images without adjusting the script, the files should be organized as described in 'processing_camera_trap_images_step_by_step'. 
## If the files are organized differently, the script has to be adapted.


## This script is used to automatically classify camera trap images.
## You can either use the provided model (small_mamma_classification_model_2022.h5) for classification of small mammals or use your own model.
## Script adjustments might be necessary if you are using a different model.


## Open the Rstudio project 'camera_trap_workflow.Rproj', open the script 02_classify_images_automatically.R and run the script
## In the SET-UP part, you can specify where your images are saved, where the output should be saved and which model should be used
## Then the images will be classified automatically
## A file with the model labels will be saved in the specified folder. The files will be organized by locality and year. 
## Folders for the locality and year will be created automatically within the specified output folder it they don't exist.

##  The file contains the following coloumns
##  site        camera trap siteID
##  year        year when the image was taken (when the image was downloaded)
##  filename    image name
##  guess1      automatic image label, class with the highest confidence extrated from the model output
##  confidence  confidence that the image belongs to the given class (guess1)

## ----
## There might be some warning messages which can be ignored.
## ----

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries
library("keras")
library("tensorflow")

## set up keras and tensorflow
keras::use_condaenv("keras_r") ## tell R which anaconda environment it should use
reticulate::py_discover_config()  # check that R is using the correct anaconda environment (the python version R is using should be inside the environment)


## set directories
model.dir <- "model"  # write here the name to the folder where the model is saved
model.name <- "small_mammal_classification_model_2022.h5"  # write here the name of the model that should be used for automatic classification
image.dir <- "data/images_renamed" # write here the name to the folder where all images that should be classified are 
out.dir <- "data/automatic_classification" # write here the path to the folder where the output files should be saved,
## a folder for localities and year within locality will be created automatically

## set locality
locality <- "vestre_jakobselv" # write here the name of the locality (folder name) you want to process

## set year
year <- "2020" # write here which year of images (folder name) you want to process

## get all sites ids
sites <- dir(paste(image.dir, locality, sep = "/"))

## load the model
model <- load_model_hdf5(paste(model.dir, model.name, sep = "/"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## CLASSIFY IMAGES AUTOMATICALLY ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## the following loop will go through all sites, classify the images automatically and save the output files

for (i in c(1:length(sites))) {
  if (year %in% dir(paste(image.dir, locality, sites[i], sep = "/"))) {
    
    # skip sites that have been processed already
    filename <- paste0("classification_", sub(".h5", "", model.name), "_", sites[i], "_", year, ".txt")
    if (file.exists(paste(out.dir, locality, year,  filename, sep = "/"))) {
      print(paste0(sites[i], ": already processed"))
      next
    }
    
    start <- Sys.time()
    image.path <- paste(image.dir, locality, sites[i], year,  sep = "/")
    filenames <- dir(image.path, pattern = "JPG")
    
    print(paste0(sites[i], ": ", length(filenames), " images"))
    
    ## load and prepare data
    test_data <- data.frame(filename = filenames, class = "0")
    
    testgen <- image_data_generator(rescale = 1/255)
    test_generator <- flow_images_from_dataframe(test_data, 
                                                 directory = image.path, 
                                                 x_col = "filename", 
                                                 y_col = "class", 
                                                 generator = testgen, 
                                                 target_size = c(224, 224), 
                                                 batch_size = 128, 
                                                 class_mode = "categorical", 
                                                 shuffle = FALSE)
    
    ## predict classes
    pred <- model %>% predict_generator(test_generator, steps = ceiling(nrow(test_data)/128))
    
    ## make output nice
    results_test <- data.frame(site = sites[i], year = year, filename = test_data$filename, 
                               guess1 = (apply(pred, 1, which.max))-1, confidence1 = apply(pred, 1, max))
    
    
    ## create folder for output
    suppressWarnings(dir.create(paste(out.dir,sub(".h5", "", model.name) , sep = "/")))     
    suppressWarnings(dir.create(paste(out.dir, sub(".h5", "", model.name), locality, sep = "/")))
    suppressWarnings(dir.create(paste(out.dir, sub(".h5", "", model.name), locality, year, sep = "/")))
    
    ## save output
    filename <- paste0("classification_", sub(".h5", "", model.name), "_", sites[i], "_", year, ".txt")
    
    write.table(results_test, paste(out.dir, sub(".h5", "", model.name), locality, year,  filename, sep = "/"), row.names = FALSE)
    
    end <- Sys.time()
    diff <- sub("Time difference of", "", difftime(end, start, units = "hours"))
    print(paste0(sites[i], ": Classification of ", length(filenames), " images took ", diff, " hours"))
    
  }
}



