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
folderToDataFrame <- function(folder.name, subset.files = c(), size = c(100,100)){
	# Transforms a list of jpeg files in the folder, folder.name, into a dataframe
	#  with each image as a row and each column as a pixel.
	#
	# Args:
	#   folder.name: Name of folder in which image files are located.
	#   subset.files: Index vector of files to be used. If left blank, all files used.
	#   size: A 2-dimensional vector giving the height and width to which each image
	#    should be resized.
	#
	# Returns:
	#   A dataframe where the rows correspond to an individual image, the first 
	#    column corresponds to the label, and the other columns correspond to 
	#    pixels. 
	# Get list of file names. 
	if (length(subset.files) == 0){
		file.names <- list.files(folder.name, pattern = "*.jpg", full.names = TRUE)
	} else {
		file.names <- list.files(folder.name, pattern = "*.jpg", full.names = TRUE)[subset.files]
	}
	labels <- as.numeric(grepl("dog", file.names))  # Create labels vector
	data <- lapply(file.names, readImage)  # Read each image
	data <- lapply(data, resize, h = size[1], w = size[2])  # Resize each image
	data <- lapply(data, channel, mode = "gray")  # Convert each image to grayscale
	data <- lapply(data, imageData)  # Get image data for each image
	data <- lapply(data, as.vector)  # Coerce each image to vector
	data <- data.frame(data)  # Combine to dataframe
	data <- t(data)  # Transpose
	data <- cbind(labels, data)  # Append labels
	return(data)
}

# Executed Statements
# Set working directory
setwd('/Users/kevinoconnor/Documents/Kaggle Competitions/CatVsDog/Kaggle-CatVsDog')
# Read in training data
train.data <- folderToDataFrame("train", subset.files = c(1:1000, 24001:25000))
# Fitting sparse logistic model
model <- cv.glmnet(train.data[, -1], train.data[, 1], family = "binomial", type.measure = "class")
# Reading in test data
test.data <- folderToDataFrame("test", subset.files = sample(1:12500, 1000))
predictions <- as.vector(predict(model, newx = test.data[, -1], s = "lambda.min", type = "class"))
sum(predictions != test.data[, 1]) / length(predictions)  # Outputting error rate
