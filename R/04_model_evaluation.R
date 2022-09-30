##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Model evaluation
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


## This script is used for model evaluation and thus, assumes that the images have been classified automatically and quality checked using the 'script 03_quality_check.R'. 

## Open the Rstudio project 'camera_trap_workflow.Rproj', then open the script '04_model_evaluation.R' and run the script.
## Specify the directories and other parameters in the SET-UP part.
## You can then calculate model accuracy, precision, recall and F1 as well as plot a confusion matrix.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries
library(caret)  # for calculating accuracy measures
library(tidyverse)  # for data handling

## set directories
input.dir <- "data/automatic_classification/small_mammal_classification_model_2022"  # write here the path to the folder where the model output files are saved
results.dir <- "data/quality_check/small_mammal_classification_model_2022"  # write here the path to the folder where the files with the manual labels are saved

# set locality
locality <- "vestre_jakobselv"   # write here the name of the locality (folder name) you want to process

## set year
year <- 2020  # write here which year of images (folder name) you want to process


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PREPARE DATA  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load files with model labels
auto <- map_dfr(dir(paste(input.dir, locality, year, sep = "/"), full.names = TRUE), read.table, header = TRUE)

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
results_name <- paste0("quality_check_", locality, "_", year, ".txt") # filename
man <- read.table(paste(results.dir, year, results_name, sep = "/"), header = TRUE)  #load data


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## MODEL EVALUATION  ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## summarize the automatic classification
table(auto$class_id)

## check model confidences
hist(auto$confidence1) # histogram of the model confidences
length(which(auto$confidence1 > 0.9))/nrow(auto) # amount of images that have been classified with a confidence above 0.9

## overall model accuracy (using random quality check images)
length(which(man$class_id[man$type == "random"] == man$guess1[man$type == "random"]))/nrow(man[man$type == "random",])

## calculate precision, recall and F1 score per class
man_class <- man[man$type == "class" | man$type == "random",]  # use only images from random and per class quality check
conf <- confusionMatrix(data = as.factor(man_class$guess1), reference = as.factor(man_class$class_id), dnn = c("Prediction", "Reference")) 
conf$byClass[, c("Precision", "Recall", "F1")]

## plot a confusion matrix
conf <- as.data.frame(conf$table)
num_class <- as.data.frame(table(man_class$class_id))

classes <- unique(man_class$class_id)
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

## calculate and plot accuracy per confidence level
man_conf <- man[man$type == "confidence",]  # use only images from per confidence class quality check
conf <- seq(0,1, 0.1)

acc <- c()
no_im <- c()

for (i in 1:(length(conf)-1)) {
  toto <- man_conf[man_conf$confidence1 > conf[i] & man_conf$confidence1 <= conf[i+1],]
  acc[i] <- length(which(toto$guess1 == toto$class_id)) / nrow(toto)
  no_im[i] <- nrow(toto)
}

cbind(conf[-1], acc, no_im)

plot(conf[-1], acc, type = "b", 
     xlab = "Confidence class", ylab = "Accuracy")

