

library(magick)
library(stringr)

# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150401")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150402")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150403")
setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150404")


# add time stamp to RGB images

start <- as.POSIXct("2015-04-04") # from 6:00am to 4:00pm

interval <- 15 #minutes
end <- start + as.difftime(1, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS <- TS[25:72]
TS


# Example image
# RGB_img <- image_read("RGB_20150401_219.jpg")
# plot(RGB_img)

patt <- ".jpg"
filenames <- list.files(pattern = patt)


# i <- 2

# Add CUSTOM LABEL DATE

for (i in 1:length(TS)) {
name <- str_sub(filenames[i], start = 1, end = -5)
RGB_img <- image_read(filenames[i])
annotation <- paste(as.character(TS[i]), "UTC+04:00")
RGB_img <- image_annotate(RGB_img, annotation, size = 90, color = "black", boxcolor = "pink",
               degrees = 0, location = "+40+10")
plot(RGB_img)
image_write(RGB_img, path = paste(name, "_date",".jpg", sep = ""), format = "jpg")
plot(RGB_img)

}


#### from MIKE WESTON.....add a stamp to geotiff
# Great! Worked perfectly.
# To plot time stamp I did
# par(col.axis="white",col.lab="white",tck=0) # makes the axes white in next step
# plot(my_stack,axes=TRUE,main=heading,legend=FALSE) # Use main to put timestamp. Axes are not visible.

############# COMBINE IMAGES: RGB and DUST mask #####------------------------

library(magick)
library(stringr)

# read images separately

# i <- 2

setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150401")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150402")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150403")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150404")
patt <- ".jpg"
filenames_RGB <- list.files(pattern = patt)

for (i in 1: length(filenames_RGB)) {

 setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150401")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150402")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150403")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150404")
  patt <- ".jpg"
filenames_RGB <- list.files(pattern = patt)
RGB <- image_read(filenames_RGB[i])
plot(RGB)


setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/images_png/20150401_selected_dust")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/images_png/20150402_selected_dust")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/images_png/20150403_selected_dust")
# setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/images_png/20150404_selected_dust")

patt <- ".png"
filenames_DUST <- list.files(pattern = patt)
DUST_MASK <- image_read(filenames_DUST[i])
plot(DUST_MASK)


# stack images in one stack
img <- c(RGB, DUST_MASK)
img <- image_scale(img, "500x500")
image_info(img)

left_to_right <- image_append(image_scale(img, "x600"))
plot(left_to_right)


setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/combined_DUST_Mask_and_RGB")


name <- str_sub(filenames_DUST[i], start = 1, end = -5)
image_write(left_to_right, path = paste(name, "_combined_RGB_DUST",".jpg", sep = ""), format = "jpg")

}


# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files

# magick -delay 100 -loop 0 *.jpg SEVIRI_RGB_and_DUST_mask_event_02_April_2015.gif

###########################################################################
###########################################################################
###########################################################################












# Example image
frink <- image_read("https://jeroenooms.github.io/images/frink.png")

# Trim margins
image_trim(frink)

# Passport pica
image_crop(frink, "100x150+50")

# Resize
image_scale(frink, "200x") # width: 200px
image_scale(frink, "x200") # height: 200px

# Rotate or mirror
image_rotate(frink, 45)
image_flip(frink)
image_flop(frink)

# Set a background color
image_background(frink, "pink", flatten = TRUE)

# World-cup outfit (Flood fill)
image_fill(frink, "orange", "+100+200", 30000)

# Add randomness
image_blur(frink, 10, 5)
image_noise(frink)

# Silly filters
image_charcoal(frink)
image_oilpaint(frink)
image_emboss(frink)
image_edge(frink)
image_negate(frink)

# Add some text
image_annotate(frink, "I like R!", size = 50)

# Customize text
image_annotate(frink, "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink",
               degrees = 60, location = "+50+100")

# Only works if ImageMagick has fontconfig
image_annotate(frink, "The quick brown fox", font = 'times-new-roman', size = 30)

##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
############# COMBINE IMAGES #############################################


frink <- image_read("https://jeroenooms.github.io/images/frink.png")
logo <- image_read("https://www.r-project.org/logo/Rlogo.png")

# stack images in one stack
img <- c(logo, frink)
img <- image_scale(img, "300x300")
image_info(img)

image_mosaic(img)

left_to_right <- image_append(image_scale(img, "x200"))
image_background(left_to_right, "white", flatten = TRUE)

top_to_bottom <- image_append(image_scale(img, "100"), stack = TRUE)
image_background(top_to_bottom, "white", flatten = TRUE)

# save images ????????

#################################################################
#################################################################
#################################################################
#### ANIMATIONS #################################################
#################################################################


animation <- image_animate(image_scale(img, "200x200"), fps = 1, dispose = "previous")
print(animation)
# save animations ?????????
image_write(animation, "Rlogo-frick.gif")


oldlogo <- image_read("https://developer.r-project.org/Logo/Rlogo-2.png")
newlogo <- image_read("https://www.r-project.org/logo/Rlogo.png")
logos <- c(oldlogo, newlogo)
logos <- image_scale(logos, "400x400")
print(logos)

# Create GIF
(animation1 <- image_animate(logos))
image_write(animation1, "img.gif")


(animation2 <- image_animate(image_morph(logos, frames = 10)))
image_write(animation2, "anim2.gif")


animation <- image_animate(image_join(logos))
print(animation)
image_write(animation1, "imgAAA.gif")


banana <- image_read("https://jeroen.github.io/images/banana.gif")
banana <- image_scale(banana, "150")
image_info(banana)
image_info(img)


background <- image_background(image_scale(logo, "200"), "white", flatten = TRUE)

# Combine and flatten frames
frames <- lapply(banana, function(frame) {
  image_composite(background, frame, offset = "+70+30")
})

frames <- lapply(img, function(frame) {
  image_composite(background, frame, offset = "+70+30")
})

frames <- image_composite(img, offset = "+70+30")

# Turn frames into animation
animation <- image_animate(image_join(frames))
print(animation)






print(animation)

image_write(animation, "Rlogo-banana.gif")
image_write(animation, "Rlogo-frick.gif")
