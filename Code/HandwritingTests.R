# # Tests
#install("handwriting")
library(handwriting)

london = readPNGBinary("../../../../../Desktop/LondonSmall.png", .15)
london = crop(london)
london_thin = thinImage(london, verbose = TRUE)
plotImageThinned(london, london_thin)

letter = readPNGBinary("/Users/Nick/Documents/Projects/CSAFE/Data/by_field/hsf_0/lower/74/74_00054.png")
letter = crop(letter)
letter_thin = thinImage(letter, verbose = "Image")
plotImageThinned(letter, letter_thin)

cells = readPNGBinary("../../../../../Desktop/CellImage.png", inversion = TRUE)
cells = crop(cells)
cells_thin = thinImage(cells, verbose = TRUE)
plotImageThinned(cells, cells_thin)

message = readPNGBinary("../../../../../Desktop/MyFBIParagraphFull.png", .15)
message = crop(message)
image(message)
message_thin = thinImage(message, verbose = TRUE)
plotImageThinned(message, message_thin)


###### Get enpdoints and crosses <- Counting neighbors can't give the crosses, but can point to potential locations and make searching easier.
###### Points with 1 neighbor are end points, though all end points don't have 1 neighbor. For small circles, will give nodes right next to eachother. Should check for and correct this.

img = letter
img.m = melt(img)
neighborMat = matrix(apply(X = matrix(cbind(img.m$Var1, img.m$Var2), ncol = 2), MARGIN = 1, FUN = numNeighbors, img = letter_thin), byrow = F, ncol = dim(img)[2])
neighborMat = ifelse(letter_thin == 0, neighborMat, 0)
nodes = matrix(0, ncol = dim(img)[2], nrow = dim(img)[1])
nodes[] = 0
nodes[which((neighborMat < 2 | neighborMat > 3) & letter_thin == 0)] = 1
image(nodes)

l.m = melt(letter)
t.m = melt(letter_thin)
n.m = melt(neighborMat)

l.m$value = mapvalues(l.m$value, c(1,0), c("Background", "Full Letter"))
l.m$value[t.m$value != 1] = "Thin Letter"
l.m$value[(n.m$value < 2 | n.m$value > 3) & t.m$value == 0] = "Potential Node"
l.m$value = factor(l.m$value, levels = c("Background", "Full Letter", "Thin Letter", "Potential Node"))
ggplot(data= l.m, aes(x = Var2, y = rev(Var1), fill = as.factor(value))) + geom_raster() + scale_fill_manual("Neighbors", values = c("white", "lightgray", "black", "red"), breaks = c("Background", "Full Letter", "Thin Letter", "Potential Node")) + theme_void()

