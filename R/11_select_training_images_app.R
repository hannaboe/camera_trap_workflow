##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Select training images - APP
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


## This app is used to select new training images. 
## The images are selected based on automatic classification and thus, assumes that the images have been classified automatically using the script '02_classify_images_automatically.R'. 
## If the images have been classified with another model and the model output files have a different structure, the app has to be modified.

## Open the Rstudio project 'camera_trap_workflow.Rproj', then open the script '11_select training_images_app.R'.
## Specify the parameters for selecting training images in the SET-UP part:
## You can select images from a certain site, from a certain class and/or below a certain confidence level.
## The selected images will presented for manual labeling in a new plotting window.
## Select the correct label by clicking on the label. The included labels are empty, bad quality, vole, lemming, shrew, stoat, least weasel and bird. The app can be modified to include other labels.
## The image will then be copied to the folder for the correct class within in the folder for training images
## If you don't want to add the image to the training data set, click NEXT to skip the image.
## A file with all images that have been checked (also the images that were skipped) will be saved in the folder for training images to avoid checking the same images again.




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## RUN APP
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

if (!require('shiny')) install.packages("shiny"); library('shiny')  # load the shiny package

drive <- "E:/"  # specify on which drive the images and classification files are saved. If you saved in on your computer, it's usually the C-drive.

source("apps/select_training_images_app.R")  # load the app
shinyApp(ui = ui, server = server)  # run the app
