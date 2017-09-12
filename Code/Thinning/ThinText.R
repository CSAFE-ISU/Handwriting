library(plyr)
library(ggplot2)
library(reshape2)
library(png)

# https://rosettacode.org/wiki/Zhang-Suen_thinning_algorithm

readPNGBinary = function(path, binarizationCutoff = 0, inversion = FALSE)
{
  img = readPNG(path, native = FALSE)
  img = apply(img, c(1,2), FUN = function(x){1-any(x < 1-binarizationCutoff)})
  if(inversion == TRUE)
    img = 1-img
  return(img)
}

plotImage = function(x)
{
  xm = melt(x)
  p = ggplot(xm, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value))) + scale_fill_manual(values = c("black", "white"), guide = FALSE) + theme_void()
  return(p)
}

# In the read in PNG files, (1,1) is top left.
# Value of 1 is blank and 0 is filled in.

neighborChanges = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
  {
    neighbs = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,3,6,9,8,7,4,1,2)]
    return(sum(neighbs == 1 & c(neighbs[-1], neighbs[1]) == 0))
  }
  else
    return(0)
}
numNeighbors = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
  {
    neighbs = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,3,6,9,8,7,4,1)]
    return(sum(neighbs == 0))
  }
  else
    return(0)
}
neighbs246468 = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
  {
    neighbs246 = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,6,8)]
    neighbs468 = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(4,6,8)]
    return(as.numeric(any(neighbs246 == 1) & any(neighbs468 == 1)))
  }
  else
    return(0)
}

neighbs248268 = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
  {
    neighbs248 = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,4,6)]
    neighbs268 = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,4,8)]
    return(as.numeric(any(neighbs248 == 1) & any(neighbs268 == 1)))
  }
  else
    return(0)
}

stepA = function(img, coords)
{
  imgA = matrix(apply(X = coords, MARGIN = 1, FUN = neighborChanges, img = img), byrow = F, nrow = 1)
  imgB = matrix(apply(X = coords, MARGIN = 1, FUN = numNeighbors, img = img), byrow = F, nrow = 1)
  img246468 = matrix(apply(X = coords, MARGIN = 1, FUN =  neighbs246468, img = img), byrow = F, nrow = 1)
  imgFlag = as.numeric(imgB >= 2 & imgB <= 6 & imgA == 1 & img246468 == 1)
  return(imgFlag)
}
stepB = function(img, coords)
{
  imgA = matrix(apply(X = coords, MARGIN = 1, FUN = neighborChanges, img = img), byrow = F, nrow = 1)
  imgB = matrix(apply(X = coords, MARGIN = 1, FUN = numNeighbors, img = img), byrow = F, nrow = 1)
  img248268 = matrix(apply(X = coords, MARGIN = 1, FUN =  neighbs248268, img = img), byrow = F, nrow = 1)
  imgFlag = as.numeric(imgB >= 2 & imgB <= 6 & imgA == 1 & img248268 == 1)
  return(imgFlag)
}

thinImage = function(img, verbose = FALSE)
{
  flag = TRUE
  if(verbose) iterCount = 1
  thinned = img
  change = which(img == 0)
  while(flag == TRUE)
  {
    if(verbose != FALSE) start.time <- Sys.time()
    index = change[thinned[change] == 0]
    img.m = cbind(((index - 1) %% dim(img)[1]) + 1, ((index - 1) %/% dim(img)[1]) + 1)
    flagA = stepA(thinned, img.m)
    thinned[index] = ifelse(c(thinned[index] == 0) & flagA == 0, 0, 1)
    flagB = stepB(thinned, img.m)
    thinned[index] = ifelse(c(thinned[index] == 0) & flagB == 0, 0, 1)
    
    if(sum(flagA + flagB, na.rm = T) == 0)
    {
      flag = FALSE
    }
    else
    {
      change = index[(flagA | flagB)]
      change = unique(rep(change, each = 9) + rep(c(0,1, -1, dim(img)[1]-1, dim(img)[1]+1, -dim(img)[1]+1, -dim(img)[1]-1, dim(img)[1], -dim(img)[1]), length(change)))
      change = change[change>=1 & change<=prod(dim(img))]
    }

    if(verbose == TRUE)
    {
      cat("\nIteration", iterCount, "done:", sum(flagA | flagB), "changes.")
      cat("\nLeft to check:", ifelse(sum(flagA | flagB) != 0, length(change), 0))
      iterCount = iterCount + 1
    }
    flagA[] = 0
    flagB[] = 0
    
    if(verbose == TRUE)
    {
      #temp = matrix(1, dim(img)[1], dim(img)[2])
      #temp[change] = 0
      #image(temp, main = "Need to update")
      
      end.time <- Sys.time()
      cat("\nIteration Time:", end.time - start.time, "\n")
    }
  }
  return(thinned)
}

plotImageThinned = function(img, thinned)
{
  l.m = melt(img)
  t.m = melt(thinned)
  l.m$value[t.m$value == 0] = 2
  p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value != 1), alpha = ifelse(value==0,.3,1))) + scale_alpha_continuous(guide = FALSE) + scale_fill_manual(values = c("white", "black"), guide = FALSE) + theme_void()
  return(p)
}

crop = function(img)
{
  if(any(img[,1] != 1)) {img = cbind(rep(1, dim(img)[1]), img)}
  if(any(img[,dim(img)[2]] != 1)) {img = cbind(img, rep(1, dim(img)[1]))}
  if(any(img[1,] != 1)) {img = rbind(rep(1, dim(img)[2]), img)}
  if(any(img[dim(img)[1],] != 1)) {img = rbind(img, rep(1, dim(img)[2]))}
  
  rows = apply(img, 1, FUN = function(x){any(x != 1)})
  cols = apply(img, 2, FUN = function(x){any(x != 1)})
  x.min = max(which(rows)[1] - 1, 1)
  x.max = min(which(rows)[sum(rows)] + 1, length(rows))
  y.min = max(which(cols)[1] - 1, 1)
  y.max = min(which(cols)[sum(cols)] + 1, length(cols))
  
  return(img[x.min:x.max,y.min:y.max])
}

# Tests
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
###### Points with 1 neighbor are end points, though. For small circles, will give nodes right next to eachother. Should check for and correct this.

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

