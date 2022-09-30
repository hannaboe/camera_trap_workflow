##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Quality check model labels - APP
## Author: Hanna Böhner
## Date: 2022-09-29
##
## This app is part of the semi-automatic workflow for processing camera trap images and comes with the publication 
## Böhner H., Kleiven E. F., Ims R. A., Soininen E. M. (2022) A semi-automatic workflow to process camera trap images, BioRxiv
## It comes without any warranty and is free for everyone to use or modify
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


## Instructions ------------

## To test the workflow, download or clone the github-repository https://github.com/hannaboe/camera_trap_workflow. 
## This folder contains all scripts needed for the workflow, the small mammal classification model and a small test data set. 
## The files are organized as we suggest in 'processing_camera_trap_images_step_by_step.pdf'. 

## To run the workflow on your own images without adjusting the app, the files should be organized as described in 'processing_camera_trap_images_step_by_step'. 
## If the files are organized differently, the app has to be adapted.


## This app is used to verify automatic image labels and thus, assumes that the images have been classified automatically using the script 02_classify_images_automatically.R. 
## If the images have been classified with another model and the model output files have a different structure, the app has to be modified.

## Open the Rstudio project 'camera_trap_workflow.Rproj', then open the script '03_quality_check_app.R' and run the app
## Specify the parameters for the quality check in the after starting the app
## you can either select random images, images per class or images per confidence class.
## The selected images will presented for manual labeling
## Select the correct label by clicking on the label. The included labels are empty, bad quality, vole, lemming, shrew, stoat, least weasel and bird. The script can be modified to include other labels-
## You can also skip the image (NEXT), go back to the last images in case of a mistake (BACK), check the previous or the next images in case it is difficult to identify the species
## One file per locality with the manual labels will be saved in the quality-check folder.


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
## RUN APP
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

if (!require('shiny')) install.packages("shiny"); library('shiny')  # load the shiny package

drive <- "C:/"  # specify on which drive the images and classification files are saved. If you saved in on your computer, it's usually the C-drive.

source("apps/quality_check_app.R")  # load the app
shinyApp(ui = ui, server = server)  # run the app
