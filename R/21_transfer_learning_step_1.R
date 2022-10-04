##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Transfer learning - Step 1
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



## This script can be used to to train a new classification model with our small mammal classification model as a base model using transfer learning.
## It can also be adapted to be used with other base models.

## All images that should be used for training, validation and testing should have been selected and stored in a folder called 'training_images' with subfolders for each category as described in '11_select_training_images.R' 
## The script requires files that contain names and class of all training and validation images. See script '12_create_training_files.R' for instructions how these files should be formatted.
## To train a classification model, keras and tensoflow have to be installed and tested as described in 'processing_camera_trap_images_step_by_step'.

## We use similar parameters as we used for the original model. 
## However, the optimal parameters and learning rate policy will depend on the data set and it might be worth to test different parameters. 


## Start a new RStudio session, open the Rstudio project 'camera_trap_workflow.Rproj', then open the script 31_transfer_learning_step_1.R
## Specify the parameters for training the model in the SET-UP part:
## You can specify the number of epochs, the learning rate scheme and the learning rate.
## The base model will be frozen, new top layers will be added and trained. This can take a long time, especially if your not using a GPU.
## After training is finished, the best model as well as the final model will be saved in the model folder.
## This model can then be fine-tuned using the script 32_transfer_learning_step_1.R
## A file with the training history will be saved in the history folder
## The file contains training accuracy, training loss, validation accuracy and validation loss for each epoch


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries
library("keras")
library("tensorflow")
library("zen4R")  # for downloading the model from zenodo

## set up keras and tensorflow
keras::use_condaenv("keras_r") ## tell R which anaconda environment it should use
reticulate::py_discover_config()  # check that R is using the correct anaconda environment (the python version R is using should be inside the environment)

## set directories
image.dir <- "data/training_images"  # write here the path to the folder where all training images are saved
file.dir <- "data/training_files"  # write here the path to the folder where the files for training and validation are saved
model.dir <- "model"  # write here the path to the folder where the model should be saved
history.dir <- "history"  # write here the path to the folder where the history file should be saved

## set model parameters
base_model_name <- "small_mammal_classification_model_2022.h5"  # write here the name of the model that should be retrained, the model has to be saved in the model folder
epochs <- 30  # for how many epochs should the model be trained? 
batch_size <- 64  # how many images should the algorithm process in one iteration? Smaller batch size leads to more precise models but training takes more time.
learning_rate_scheme <- "triangular"  # possibilities are "triangular", "triangular2", "exp_range" or sinus. See http://thecooldata.com/2019/01/learning-rate-finder-with-cifar10-keras-r/ for an explanation of the learning rate schemes
learning_rate_min <- 0.000001  # minimum learning rate when using cycling learning rate. It's recommended to test different values when training a new model.
learning_rate_max <- 0.001  # maximum learning rate when using cyclig learning rate. It's recommended to test different values when training a new model.

## set file names
training_name <- "training_data.txt"  # write here the name of the file with all training images and classes
validation_name <- "validation_data.txt"  # # write here the name of the file with all training images and classes

model_name <- paste(paste("transfer_learning_1", sub(".h5", "", base_model_name), epochs, batch_size, learning_rate_min, learning_rate_max, sep = "_"), "h5", sep = ".")
history_name <- paste(paste("transfer_learning_1", sub(".h5", "", base_model_name), epochs, batch_size, learning_rate_min, learning_rate_max, sep = "_"), "csv", sep = ".")

## load functions for cyclic learning rate
source("rscripts/functions_cyclic_learning_rate.R")

## download the model from zenodo (if it hasn't been downloaded before), it will be saved in the model folder
if (!file.exists(paste(model.dir, model.name, sep = "/"))) {
  options(timeout=2000)  # set the timeout when downloading will be stopped to a high number (downloading the model takes a while)
  download_zenodo(doi = "10.5281/zenodo.7142734", path = model.dir, files = list(model.name))  # download the model
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## LOAD IMAGES AND PREPARE THE MODEL  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## set up image generators for preprocessing images, the parameters can be changed
datagen <- image_data_generator(rescale = 1/255,
                                rotation_range = 40,
                                width_shift_range = 0.2,
                                height_shift_range = 0.2,
                                shear_range = 0.2,
                                zoom_range = 0.2,
                                horizontal_flip = TRUE,
                                fill_mode = "nearest")

testgen <- image_data_generator(rescale = 1/255)


## get the training images (the error 'not all arguments converted during string formatting' can be ignored)
train_data <- read.table(paste(file.dir, training_name, sep = "/"), header = TRUE, colClasses = c("character", "character"))
train_generator <- flow_images_from_dataframe(train_data, 
                                              directory = image.dir, 
                                              x_col = "filename", 
                                              y_col = "class", 
                                              generator = datagen, 
                                              target_size = c(224, 224), 
                                              batch_size = batch_size, 
                                              class_mode = "categorical")

## get validation images (the error 'not all arguments converted during string formatting' can be ignored)
val_data <- read.table(paste(file.dir, validation_name, sep = "/"), header = TRUE, colClasses = c("character", "character"))
val_generator <- flow_images_from_dataframe(val_data, 
                                            directory = image.dir, 
                                            x_col = "filename", 
                                            y_col = "class", 
                                            generator = testgen, 
                                            target_size = c(224, 224), 
                                            batch_size = batch_size, 
                                            class_mode = "categorical")

## load the base model
base_model <- load_model_hdf5(paste(model.dir, base_model_name, sep = "/"))

## remove the last 2 layers
base_model <- keras_model(inputs = base_model$input, output = get_layer(base_model, index = -1)$output)
base_model <- keras_model(inputs = base_model$input, output = get_layer(base_model, index = -1)$output)

## freeze the layers of the base model
base_model$trainable <- FALSE

## add new layers (step 1) -> best??
inputs = layer_input(shape=c(224, 224, 3))
x <- base_model(inputs, training = FALSE)  # set training = FALSE -> so batch_normalization layers won't be updated when fine-tuning

predictions <- x %>% 
  #layer_average_pooling_2d(pool_size = c(2, 2), name = "avg_pool") %>%
  layer_flatten() %>% 
  layer_dense(length(unique(train_data$class)), activation = "softmax", name = "probs", kernel_initializer = 'lecun_normal')

model <- keras_model(inputs = inputs, outputs = predictions)

## check layers
for (layer in model$layers) {
  print(paste(layer$name, layer$trainable))
}

## compile the model
model <- model %>%   compile(loss = 'categorical_crossentropy', optimizer = optimizer_adam(lr = learning_rate_min), metrics = c("accuracy"))



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## DEFINE CALLBACKS FOR CYCLIG LEARNING RATE  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## set up cyclic learning rate
n_iter <- epochs * ceiling((nrow(train_data)/batch_size))
l_rate <- Cyclic_LR(iteration = 1:n_iter, base_lr = learning_rate_min, max_lr = learning_rate_max, step_size = n_iter/2, mode = learning_rate_scheme, 
                    gamma = 1, scale_fn = NULL, scale_mode = 'cycle')

## create the callbacks
callback_lr <- callback_lambda(on_train_begin=callback_lr_init, on_batch_begin=callback_lr_set)
callback_logger <- callback_lambda(on_batch_end=callback_lr_log)

## initiate the the logger
callback_log_acc_lr <- LogMetrics$new()

## put all callbacks in a list
callbacks_list <- list(callback_csv_logger(filename = paste(history.dir, history_name, sep = "/"), append = TRUE),
                       callback_lr, callback_logger, callback_log_acc_lr
)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## TRAIN THE MODEL  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

history <- model %>% fit_generator(train_generator, 
                                   steps_per_epoch = ceiling(nrow(train_data)/batch_size), 
                                   epochs = epochs, 
                                   callbacks = callbacks_list,
                                   validation_data = val_generator, 
                                   validation_steps = ceiling(nrow(val_data)/batch_size))

## save the final model
save_model_hdf5(model, paste0(model.dir, "/final_", model_name))







