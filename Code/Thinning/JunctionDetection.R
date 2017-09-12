## Junction Detection.
## Provide skeletonized image from ThinText.

#A black pixel becomes a node if its removal creates exactly one or at least 
#three 4-connected black components in its 1-neighborhood.

# Also from Zhang thinning paper

postRemovalCount = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  neighbs = img[((rr-2):(rr+2)), ((cc-2):(cc+2))]#[c(2,3,6,9,8,7,4,1)]
  neighbs[3,3] = 1
  
}
isNode = function(img, img.m)
{
  postRemovalCount = matrix(apply(X = coords, MARGIN = 1, FUN = countAfterRemoval, img = img), byrow = F, nrow = 1)
  
}