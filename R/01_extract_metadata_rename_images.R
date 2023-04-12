##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Extracting metadata and renaming of camera trap images 
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


## Open the Rstudio project 'camera_trap_workflow.Rproj', open the script 01_extract_metadata_rename_images.R and run the script.
## You can specify the metadata fields should be extracted.
## Then, metadata will be extracted and saved in the metadata-folder. The images will be renamed with a unique name and saved in the folder for renamed images.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## SET UP ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries (the libraries have to be installed before)
library(dplyr)  # for data handling
library(exifr)  # for extracting exif metadata from images

## set working directories
in.dir <- "data/raw_images"  # path to folder where raw images are saved
out.dir <- "data/images_renamed"  # path to folder where renamed images should be saved
meta.dir <- "data/metadata"  # path to folder where renamed images should be saved

## set localiy
locality <- "vestre_jakobselv" # write here the name of the locality (folder name) you want to process

## set year
year <- "2020" # write here which year of images (folder name) you want to process

## get all sites
sites <- dir(paste(in.dir, locality, sep = "/"))

## delete sites without new images
new_sites <- c()

for (i in 1:length(sites)) {
  years <- dir(paste(in.dir, locality, sites[i], sep="/"))
  if(year %in% years){
    new_sites[i] <- sites[i]
  }
}

new_sites<-new_sites[!is.na(new_sites)]  # only sites with new pictures

## check which metadata tags are available
colnames(read_exif(dir(paste(in.dir, locality, new_sites[1], year, sep = "/"), full.names = TRUE)[1]))

## write here the metadata tags that should be extracted
tags <- c("FileName", "Make", "Model", "DateTimeOriginal", "TriggerMode", "TriggerMode", "Sequence", "EventNumber", "MoonPhase", 
          "AmbientTemperature", "SerialNumber", "Contrast", "Brightness", "Sharpness", "Saturation")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## RENAME AND COPY IMAGES ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## The following loop goes through all sites, extracts metadata and renames the images.

for (i in 1:length(new_sites)){
  
  ## get image names and extract metadata
  
    if (all(grepl("RECNX", list.files(paste(in.dir, locality, new_sites[i], year, sep = "/"))))) {  # if images of one site are in different folders
      folders <- list.files(paste(in.dir, locality, new_sites[i], year, sep = "/"))                 # !! this has to be adapted if folder names differ from the default RECONYX-names
      
      filenames_list <- c()
      info_list <- c()
      for (j in 1:length(folders)){
        filenames_list[[j]] <- list.files(paste(in.dir, locality, new_sites[i], year, folders[j], sep = "/"), pattern = "JPG")
        filenames <- filenames_list[[j]]
        
        ## get metadata of images
        info_list[[j]] <- read_exif(paste(in.dir, locality, new_sites[i],  year, folders[j], filenames, sep="/"), 
                        tags = tags)
      }
      filenames <- unlist(filenames_list)
      info <- do.call(rbind, info_list)
      
    } else {  # if images of one site are in the same folder
      filenames <- list.files(paste(in.dir, locality, new_sites[i], year, sep = "/"), pattern = "JPG")  # get image names
      
      ## get metadata of images
      info <- read_exif(paste(in.dir, locality, new_sites[i],  year, filenames, sep="/"), 
                      tags = tags)
      
    }
  
    print(paste("metadata extracted", new_sites[i], Sys.time()))
    
    ## create new image names with the structure 'SiteID_date_0001-
    new_filenames <- paste(new_sites[i], gsub(":", "-",sub(" .*", "", info$DateTimeOriginal)), sep = "_")   
    
      
    ## add numbers to new image names to separate images from the same site and date
    unique_names <- unique(new_filenames)
    for ( k in 1: length(unique_names)) {
      n <- length(new_filenames[which(new_filenames == unique_names[k])])
      numbers <- as.character(1:n)
      numbers[nchar(numbers) == 1] <- paste("000", numbers[nchar(numbers) == 1], sep = "")
      numbers[nchar(numbers) == 2] <- paste("00", numbers[nchar(numbers) == 2], sep = "")
      numbers[nchar(numbers) == 3] <- paste("0", numbers[nchar(numbers) == 3], sep = "")
      new_filenames[new_filenames == unique_names[k]] <- paste(new_filenames[new_filenames == unique_names[k]], numbers, sep = "_")
    }
    new_filenames <- paste0(new_filenames, ".JPG")
    
    
    ## check if the year before has pictures with the same name (from the day when the pictures were downloaded) -> continue numbering
    if ((as.numeric(year)-1) %in% dir(paste(out.dir, locality, new_sites[i], sep="/"))){
        old_filenames <- list.files(paste(out.dir, locality, new_sites[i], as.numeric(year)-1, sep = "/"), pattern = "JPG")  # old image names
        if(length(old_filenames[old_filenames %in% new_filenames]) != 0){
          last_day <- tail(old_filenames[old_filenames %in% new_filenames], n = 1) %>% 
            str_split("_") %>%
            map(3) %>% 
            unlist(.)
          
          last_dup <- str_extract(old_filenames, last_day) 
          last_dup <- tail(which(!is.na(last_dup)), n = 1)
          
          last_number <- old_filenames[last_dup] %>% 
            strsplit("_") %>% 
            map(length(.[[1]])) %>% 
            sub(".JPG", "", .)
          correct_site <- sub(paste0("_", last_number), "", sub(".JPG", "", old_filenames[last_dup]))
          
          new_numbers<-as.character(seq(from=as.numeric(last_number)+1, length.out = length(new_filenames[grep(correct_site, new_filenames)])))
          new_numbers[nchar(new_numbers)==1]<-paste("000", new_numbers[nchar(new_numbers)==1], sep="")
          new_numbers[nchar(new_numbers)==2]<-paste("00", new_numbers[nchar(new_numbers)==2], sep="")
          new_numbers[nchar(new_numbers)==3]<-paste("0", new_numbers[nchar(new_numbers)==3], sep="")
          
          corrected_names<-paste0(correct_site, "_", new_numbers, ".JPG")
          new_filenames[grep(correct_site, new_filenames)]<-corrected_names
        }
      }
    
    
    ## create directories for renamed files
    if(!dir.exists(paste(out.dir, locality, sep = "/"))) dir.create(paste(out.dir, locality, sep = "/"))  # create locality-folder if it doesn't exist
    
    if (!new_sites[i] %in% dir(paste(out.dir, locality, sep = "/"))) {
      suppressWarnings(dir.create(paste(out.dir, locality, new_sites[i], sep = "/")))  # create site-folder if it doesn't exist 
    }
    dir.create(paste(out.dir, locality, new_sites[i], year, sep = "/"))  # create year folder
    
    
    ## copy and rename images
    if(all(grepl("RECNX", list.files(paste(in.dir, locality, new_sites[i], year, sep = "/"))))){
      for (j in 1:length(folders)){
        num <- length(list.files(paste(in.dir, locality, new_sites[i], year, folders[j], sep = "/"), pattern = "JPG"))
        if(folders[j] == folders[1]){
          file.copy(paste(in.dir, locality, new_sites[i], year, folders[j], filenames[1:num], sep = "/"), 
                    paste(out.dir, locality, new_sites[i], year,new_filenames[1:num], sep = "/"))
          num_before<-num
        } else {
          file.copy(paste(in.dir, locality, new_sites[i], year, folders[j], filenames[(num_before+1):(num_before+num)], sep = "/"), 
                    paste(out.dir, locality, new_sites[i], year, new_filenames[(num_before+1):(num_before+num)], sep = "/"))
          num_before<-num_before+num
        }
      }
    } else {
      file.copy(paste(in.dir, locality, new_sites[i], year, filenames, sep = "/"), 
                paste(out.dir, locality, new_sites[i], year, new_filenames, sep = "/"))
    }
    
    
    ## make metadata file and save it
    if(!dir.exists(paste(meta.dir, locality, sep = "/"))) dir.create(paste(meta.dir, locality, sep = "/"))  # create locality-folder if it doesn't exist
    if(!dir.exists(paste(meta.dir, locality, year, sep = "/"))) dir.create(paste(meta.dir, locality, year, sep = "/"))  # create year-folder if it doesn't exist
    
    info$NewFileName <- new_filenames  # add new image names to metadata file
    write.table(info, paste(meta.dir, locality, year, paste("metadata_", new_sites[i], "_", year, ".txt", sep=""), sep = "/"), row.names = FALSE, sep="\t")
    
    print(paste("images renamed", new_sites[i], Sys.time()))
}


