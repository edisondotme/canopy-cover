# main
library("imager")
library("foreach")
# note, imagemagick needs to be installed too for some reason
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
  
  if(mean(image) > 0.5) {
    # higher than .5 means the image is mostly sky
    # make sure to sort decreasing if this is the case
    percents <- sort(percents, decreasing = TRUE)
  } else {
    percents <- sort(percents)
  }
  return(percents)
}

# next step is to setup the function to loop through files in folder and make data frames and graphs!!

loopFiles <- function(photos_path, par = TRUE) {
  # loops through files inside of photos_path and makes a big ol matrix
  orgdir <- getwd()
  setwd(photos_path)
  percents <- c()
  
  if(par) {
    # for some reason, the parallel processing code isn't going that much faster
    print('doing in parallel!!!')
    foreach(photo = list.files()) %dopar% {
      percents <- append(percents, c(getPercentLeaf(photo), photo))
    }
  } else {
    print('not doing in parallel!!')
    for(photo in list.files()) {
      # ims <- append(ims, load.image(photo)) # uses too much ram
      percents <- append(percents, c(getPercentLeaf(photo), photo))
    }
  }
  
  # to test parallel processing time, use system.time(loopFiles(...), par = T or F)
  
  # clean up the matrix
  percents <- percents[!is.na(percents)]
  percents <- matrix(percents, ncol = 3, byrow = TRUE)
  
  percents <- data.frame(percents)
  colnames(percents) <- c('sky', 'leaf', 'filename')
  
  # restore working directory
  setwd(orgdir)
  
  return(percents)
}

makeGraphs <- function(photo_dir) {
  # function that takes the directory with all the photo directories, runs the analysis and then makes the graphs
  myresults <- list()
  for(folder in list.dirs(recursive = F)) {
    print(folder)
    myresults <- append(myresults, loopFiles(folder))
  }
}

#####################################################################################

#testing area
