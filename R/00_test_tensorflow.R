##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Test if tensorflow was installed correctly
## Author: Hanna Böhner
## Date: 2022-09-29
##
## This script comes with the publication  
## Böhner H., Kleiven E. F., Ims R. A., Soininen E. M. (2022) A semi-automatic workflow to process camera trap images, BioRxiv
## It comes without any warranty and is free for everyone to use or modify
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##



## Run this script to test if tensorflow is installed correctly

## Download or clone the github-repository https://github.com/hannaboe/camera_trap_workflow. 
## Open the Rstudio project 'camera_trap_workflow.Rproj', open the script 00_test_tensorflow.R and run the script
## Some images will be classified manually to test if tensorflow works properly

## ----
## There might be some warning messages which can be ignored.
## ----

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries
library("keras")
library("tensorflow")
library("zen4R")  # for downloading the model from zenodo

## set up keras and tensorflow
keras::use_condaenv("keras_r") ## tell R which anaconda environment it should use
reticulate::py_discover_config()  # check that R is using the correct anaconda environment (the python version R is using should be inside the environment)


## set directories
model.dir <- "model"  # folder where the model is saved
model.name <- "small_mammal_classification_model_2022.h5"  # name of the classification model
image.dir <- "data/raw_images" # folder with images

## set locality
locality <- "vestre_jakobselv" # locality (folder name) 

## download the model from zenodo (if it hasn't been downloaded before), it will be saved in the model folder
if (!file.exists(paste(model.dir, model.name, sep = "/"))) {
  options(timeout=2000)  # set the timeout when downloading will be stopped to a high number (downloading the model takes a while)
  download_zenodo(doi = "10.5281/zenodo.7142734", path = model.dir, files = list(model.name))  # download the model
}

## load the model
model <- load_model_hdf5(paste(model.dir, model.name, sep = "/"))

## get some images
images_names <- dir(paste(image.dir, "vestre_jakobselv", "vj_ga_hu_2", "2020",  sep = "/"))[seq(1, 50, 5)]


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## AUTOMATIC CLASSIFICATION ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load and prepare data (some error and warning messages can be ignored)
test_data <- data.frame(filename = images_names, class = "0")

testgen <- image_data_generator(rescale = 1/255)
test_generator <- flow_images_from_dataframe(test_data, 
                                             directory = paste(image.dir, "vestre_jakobselv", "vj_ga_hu_2", "2020",  sep = "/"), 
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
results_test <- data.frame(site = "vj_ga_hu_1", year = 2020, filename = test_data$filename, 
                           guess1 = (apply(pred, 1, which.max))-1, confidence1 = apply(pred, 1, max))




## check the output
head(results_test)
## this should show the first rows of the classification file that contains guess1 (the class), which will be 1 in most cases (empty image) and the confidence

## we use the following coding for the classes
## 0  bad quality
## 1  empty
## 2  bird
## 3  vole
## 4  least weasel
## 5  lemming
## 6  shrew
## 7  stoat

