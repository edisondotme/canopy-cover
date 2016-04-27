# main
library("imager")

clusterize <- function(file_path) {
  # get a kmeans df
  # input image file path
  # output percent leaf cover first sky, second leaves I think
  
  image <- load.image(file_path)
  image <- grayscale(image)
  
  km <- kmeans(image, centers = 2)
  # note km$size returns a two vector - first is sky, second is leaves
  
  num_pixels <- dim(image)[1] * dim(image)[2]
  return(km$size/num_pixels*100)
  
}

