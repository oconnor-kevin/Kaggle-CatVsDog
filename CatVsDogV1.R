# Program: CatVsDogV1.R
# Author: Kevin O'Connor
# Purpose: Produce probability of being a dog for pictures of cats and dogs.
# Date Created: 09/25/16
# Date Modified: 09/25/16

# README: This version will use logistic regression to produce a probability for each image.

# Libraries
library(jpeg)
library(glmnet)
library(EBImage)

# Functions
folderToDataFrame <- function(folder.name, num.files = -1, size = c(100,100)){
	# Transforms a list of jpeg files in the folder, folder.name, into a dataframe
	#  with each image as a row and each column as a pixel.
	#
	# Args:
	#   folder.name: Name of folder in which image files are located.
	#   num.files: Number of files to be read in. If left blank all files are used.
	#   size: A 2-dimensional vector giving the height and width to which each image
	#    should be resized.
	#
	# Returns:
	#   A dataframe with rows corresponding to images and columns corresponding to pixels.
	# Get list of file names.
	if (num.files == -1){
		file.names <- list.files(folder.name, pattern = "*.jpg", full.names = TRUE)
	} else if (num.files > 0){
		file.names <- list.files(folder.name, pattern = "*.jpg", full.names = TRUE)[1:num.files]
	}
	data <- lapply(file.names, readImage)  # Read each image
	data <- lapply(data, resize, h = size[1], w = size[2])  # Resize each image
	data <- lapply(data, channel, mode = "gray")  # Convert each image to grayscale
	data <- lapply(data, imageData)  # Get image data for each image
	data <- lapply(data, as.vector)  # Coerce each image to vector
	data <- data.frame(data)  # Combine to dataframe
	data <- t(data)  # Transpose
	return(data)
}

# Executed Statements
# Set working directory
setwd('/Users/kevinoconnor/Documents/Kaggle Competitions/CatVsDog/Kaggle-CatVsDog')
# Read in training data
train.data <- folderToDataFrame("train", num.files = 10)
# Running sparse logistic regression
