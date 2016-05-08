# main
library("imager")
#library("foreach")
#library("doMC")
# registerDoMC(cores = 6) # something is wrong and parallel doesn't work. oh well
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
      percents <- append(percents, c(as.numeric(getPercentLeaf(photo)), photo))
    }
  } else {
    print('not doing in parallel!!')
    for(photo in list.files()) {
      # ims <- append(ims, load.image(photo)) # uses too much ram
      percents <- append(percents, c(as.numeric(getPercentLeaf(photo)), photo))
    }
  }
  
  # to test parallel processing time, use system.time(loopFiles(...), par = T or F)
  
  # clean up the matrix
  percents <- percents[!is.na(percents)]
  percents <- matrix(percents, ncol = 3, byrow = TRUE)
  
  percents <- data.frame(percents, stringsAsFactors = FALSE)
  colnames(percents) <- c('sky', 'leaf', 'filename')
  
  # manually make columns numeric
  percents$sky <- as.numeric(percents$sky)
  percents$leaf <- as.numeric(percents$leaf)
  
  # restore working directory
  setwd(orgdir)
  
  return(percents)
}

makeGraphs <- function(photo_dir) {
  # function that takes the directory with all the photo directories, runs the analysis and then makes the graphs
  myresults <- list()
  for(folder in list.dirs(recursive = F)) {
    print(folder)
    #myresults <- append(myresults, loopFiles(folder))
  }
  
  
}

#####################################################################################

#testing area

# main plot
barplot(avgs, col = 'springgreen4', main = 'Percent Canopy Cover in Costa Rica Forests', names.arg = c('Cahuita', 'Arenal', 'Braulio', 'Cloud Forest'), ylab = '"Average" percent leaf cover')

# braulio plot
barplot(braulio$leaf, main = "Percent Leaf Cover in Braulio", xlab = 'Image instance', ylab = 'Percent leaf cover', col = 'darkolivegreen')

# arenal plot
barplot(arenal$leaf, main = "Percent Leaf Cover in Arenal", xlab = 'Image instance', ylab = 'Percent leaf cover', col = 'darkolivegreen')

# cahuita plot
barplot(cahuita$leaf, main = "Percent Leaf Cover in Cahuita", xlab = 'Image instance', ylab = 'Percent leaf cover', col = 'darkolivegreen')

# cloud_forest plot
barplot(cloud_forest$leaf, main = "Percent Leaf Cover in Cloud Forest", xlab = 'Image instance', ylab = 'Percent leaf cover', col = 'darkolivegreen')
