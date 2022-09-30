##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Evaluate a classification model
## Author: Hanna Böhner
## Date: 2022-09-29
##
## This script is part of the semi-automatic workflow for processing camera trap images and comes with the publication 
## Böhner H., Kleiven E. F., Ims R. A., Soininen E. M. (2022) A semi-automatic workflow to process camera trap images, BioRxiv
## It comes without any warranty and is free for everyone to use or modify
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


## Instructions ------------

## To test the workflow, download xx. This folder contains all scripts needed for the workflow, the small mammal classification model
## a test dataset and a training dataset. The files are organized as we suggest in 'processing_camera_trap_images_step_by_step'. 

## To run the workflow on you own images without adjusting the script, the files should be organized as described in 'processing_camera_trap_images_step_by_step'. 
## If the files are organized differently, the script has to be adapted.


## This script can be used to evaluate a classification model on the test data set.
## All images that should be used for training, validation and testing should have been selected and copied to a folder for the category as described in '11_select_training_images.R' 
## The script requires files that contain names and class of all test images. See script '12_create_training_files.R' for instruction how these files should be formatted.
## To run a classification model, keras and tensoflow have to be installed and tested as described in 'processing_camera_trap_images_step_by_step'.


## Start a new RStudio session, open the Rstudio project 'camera_trap_workflow.Rproj', then open the script '14_evaluate_test_images.R'.
## The test images will be classified automatically using the specified model.
## Then model accuracy, precision, recall and F1 will be calculated.



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries
library("keras")
library("tensorflow")
library("caret")

## set up keras and tensorflow
keras::use_condaenv("keras_r") ## tell R which anaconda environment it should use
reticulate::py_discover_config()  # check that R is using the correct anaconda environment (the python version R is using should be inside the environment)

## set directories
image.dir <- "data/training_images"  # write here the path to the folder where all training images are saved
file.dir <- "data/training_files"  # write here the path to the folder where the files for training and validation are saved
model.dir <- "model"  # write here the path to the folder where the model is saved

## set file names
model_name <- "small_mammal_classification_model_2022.h5"  # write here the name of the model that should evaluated
test_name <- "test_data.txt"  # write here the name of the file with all test images and classes


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## CLASSIFY IMAGES  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## set up image generators for preprocessing images
testgen <- image_data_generator(rescale = 1/255)

## get the test images (the error 'not all arguments converted during string formatting' can be ignored)
test_data <- read.table(paste(file.dir, test_name, sep = "/"), header = TRUE, colClasses = c("character", "character"))
test_generator <- flow_images_from_dataframe(test_data, 
                                            directory = image.dir, 
                                            x_col = "filename", 
                                            y_col = "class", 
                                            generator = testgen, 
                                            target_size = c(224, 224), 
                                            batch_size = 128, 
                                            class_mode = "categorical", 
                                            shuffle = FALSE)


## load model
model <- load_model_hdf5(paste(model.dir, model_name, sep = "/"))


## predict classes
pred <- model %>% predict_generator(test_generator, steps = ceiling(nrow(test_data)/128))


## make output nice
results_test <- data.frame(filename = test_data$filename, class_id = test_data$class,
                           guess1 = (apply(pred, 1, which.max))-1, confidence1 = apply(pred, 1, max))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## EVALUATE MODEL  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


## overall model accuracy (using random quality check images)
length(which(results_test$class_id == results_test$guess1))/nrow(results_test)

## calculate precision, recall and F1 score per class
conf <- confusionMatrix(data = as.factor(results_test$guess1), reference = as.factor(results_test$class_id), dnn = c("Prediction", "Reference")) 
conf$byClass[, c("Precision", "Recall", "F1")]

## plot a confusion matrix
conf <- as.data.frame(conf$table)
num_class <- as.data.frame(table(results_test$class_id))

classes <- unique(results_test$class_id)
conf$percent <- NA
for (i in 1:length(classes)) {
  conf$percent[conf$Reference == classes[i]] <- conf$Freq[conf$Reference == classes[i]]/num_class$Freq[num_class$Var1 == classes[i]]
}

cols <- RColorBrewer::brewer.pal(5, "Blues") 

plot_conf <- ggplot(conf, aes(Prediction,sort(Reference), fill= percent)) +
  geom_tile() + geom_text(aes(label=round(percent, digits = 3), fontface = 2, size = 6)) +
  scale_fill_gradientn(colours = c("white", cols[2:5]), values = c(0, 0.01, 0.1, 0.9, 1), na.value = "white") +
  labs(title = "", y = "Reference",x = "Prediction")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour="black"), 
        legend.title = element_blank(), 
        legend.position = "none",
        axis.text = element_text(size = 11))

plot(plot_conf)







