# main.R

# ESE 389
# Canopy Cover Assessment in Costa 
# Rica Forests Using Leaf Gap Fraction 
# Analysis and Hemispherical 
# Photograph

# Load image processing library
library("imager")

# load parallization libraries
#library("foreach")
#library("doMC")
# registerDoMC(cores = 6) # something is wrong and parallel doesn't work. oh well

# note, imagemagick needs to be installed too for some reason - this could be a problem for ROGER

getPercentLeaf <- function(file_path) {
  # get a kmeans df
  # input image file path
  # output percent leaf cover first sky, second leaves
  
  image <- load.image(file_path)
  image <- grayscale(image)
  # Note, there is a bug where if the image is already BW, the above line causes a failure
  
  # do the kmeans clustering, looking for 2 clusters. as.vector is needed or else it only does the first row for some reason
  km <- kmeans(as.vector(image), centers = 2)
  # note km$size returns a two vector - first is sky, second is leaves
  
  # get percent leaf/sky
  num_pixels <- dim(image)[1] * dim(image)[2]
  percents <- km$size/num_pixels*100
  
  if(mean(image) > 0.5) {
    # sometimes percents is returned in the wrong order, this if statement fixes that
    # higher than .5 means the image is estimated to be mostly sky
    # make sure to sort decreasing if this is the case
    percents <- sort(percents, decreasing = TRUE)
  } else {
    percents <- sort(percents)
  }
  return(percents)
}

loopFiles <- function(photos_path, par = FALSE) {
  # loops through files inside of photos_path and makes a df
  # par determines whether or not to run in parallel or not
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
      print(photo)
      # ims <- append(ims, load.image(photo)) # uses too much ram
      percents <- append(percents, c(as.numeric(getPercentLeaf(photo)), photo))
    }
  }
  
  # Note: to test parallel processing time, use system.time(loopFiles(...), par = T or F)
  
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

makeGraphs <- function(dfList) {
  # function that takes list of dataframes generated using loopFiles(), runs the analysis and then makes the graphs
  
  avgs <- c()
  # start pdf
  orgdir <- getwd()
  setwd(dirname(parent.frame(3)$ofile))
  #setwd("~/canopy-cover/") # remove if not on roger
  pdf("outplots.pdf")
  
  # I should be using lapply here, but oh well
  for(df in dfList) {
    #print(deparse(substitute(df)))
    avgs <- append(avgs, mean(df$leaf))
    # for improvement: label in the title the forest
    barplot(df$leaf, main = "Average leaf cover", xlab = "Image instance", ylab = "Percent leaf cover", col = "springgreen4")
  }
  
  # for improvement: sort from low to high and label accordingly
  barplot(avgs, col = 'springgreen4', main = 'Percent Canopy Cover in Costa Rica Forests', names.arg = c('Arenal', 'Braulio', 'Cahuita', 'Cloud Forest'), ylab = '"Average" percent leaf cover')
  dev.off()
  
  # restore working directory
  setwd(orgdir)
}

execute <- function(test = F) {
  if(test){
    print("Script works.")
  } else {
    script.dir <- dirname(sys.frame(1)$ofile)
    setwd(script.dir)
    
    # hard code directory - this will only allow it to run on roger
    #setwd("~/canopy-cover/")
    
    out = list()
    for(i in 1:length(list.dirs(path ="./photos/", recursive = FALSE))){
      dirs <- list.dirs(path = "./photos/", recursive = F)
      # print(dirs)
      out[[i]] <- loopFiles(dirs[i])
    }
    return(out)
  }
}

makeGraphs(execute())


#####################################################################################
# testing area
while(FALSE) {
  
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

}
