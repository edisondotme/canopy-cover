# main
library("imager")
# setwd('~/media/Documents/UIUC/2016/ESE 389/Final Project/photos/Braulio - Mar 20')

getPercentLeaf <- function(file_path) {
  # get a kmeans df
  # input image file path
  # output percent leaf cover first sky, second leaves I think
  
  image <- load.image(file_path)
  image <- grayscale(image)
  # Note, there is a bug where if the image is already BW, the above line causes a failure
  
  # do the kmeans clustering, looking for 2 clusters. as.vector is needed or else it only does the first row for some reason
  km <- kmeans(as.vector(image), centers = 2)
  # note km$size returns a two vector - first is sky, second is leaves
  
  num_pixels <- dim(image)[1] * dim(image)[2]
  percents <- km$size/num_pixels*100
  return(percents)
  
}

# next step is to setup the function to loop through files in folder and make data frames and graphs!!

loopFiles <- function(photos_path) {
  # loops through folders and files inside of photos_path and makes a big ol dataframe
  directories <- list.dirs()
}

makeGraphs <- function(df) {
  # function that takes the df with all the data and makes a bunch of pretty graphs in a pdf format
  
}

ims <- list()
percents <- matrix(ncol = 2)
for(folder in d) {
  # print(list.files(folder))
  
  ims <- append(ims, )
}

for(photo in list.files()) {
  # ims <- append(ims, load.image(photo))
  percents <- append(percents, getPercentLeaf(photo))
}