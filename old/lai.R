# ESE 389
# Canopy Cover Assessment in Costa 
# Rica Forests Using Leaf Gap Fraction 
# Analysis and Hemispherical 
# Photography

library("imager")
setwd(dir = "media/Documents/UIUC/2016/ESE 389/Final Project/")
# load parallel library
library("foreach")
file = "./test.JPG"

im <- load.image(file)

# show image
plot(im)


im.blurry <- isoblur(im,10) #Blurry trees!
plot(im.blurry)

# How to save an image
#The file format is defined by the extension. Here we save as JPEG
imager::save.image(parrots,"/tmp/parrots.jpeg")
#We call imager::save.image to avoid ambiguity, as base R already has a save.image function

# displaying images
# display(im = )
# This is faster and loads externally in a big viewer

# "dimension" of images
dim(im) # 4000 3000    1    3
# x pixels wide, y pixels tall, 1 frame of video, 3 dimensions for color (RGB)
# Also note: coordinates start at top left (1, 1)
# +x is to the right +y is down

# Turn image grayscale
# This will be useful since color doesn't really matter here. 
# Err, actually maybe it does. I'll think about it
# Interesting thought: leaves tend to get brighter the higher up they are. Perhaps I could translate
# the shade of green to determine height. That is too much work for this project, but something 
# interesting to think about
gray.im <- grayscale(im)

plot(gray.im)


# 
make_noise <- function(color) {
  noise <- array(round(runif(5*5*5*3)),c(5,5,1, color)) #5x5 pixels, 1 frames, n colours. All noise
  noise <- as.cimg(noise)
  plot(noise)
  return(noise)
}

# I can turn images into dataframes
imd <- as.data.frame(im)
imdg <- as.data.frame(gray.im)


# testing small scale
t <- head(imdg, 1000)

c <- foreach(i = 1:length(t)) %do% {
  if() # TODO
}

# First, let's do this non parallel


np <- for (pixel in t) {
  cut <- .09
  if(pixel$value < cut) {
    t$leaf = TRUE
  } else {
    t$leaf = FALSE
  }
}

