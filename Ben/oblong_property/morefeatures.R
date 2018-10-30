if(FALSE){
  #helper functions
  xy_to_index = function(x,y,img_dim){
    return(y*img_dim[1]+x)
  }
  index_to_xy = function(index,img_dim){
    yRow = index %/% img_dim[[1]]
    xCol = index %% img_dim[[1]]
    return(list(xCol,yRow))
  }
  #diff_range(y/x)
  indicies_to_xy = function(indicies,img_dim){
    yRows = indicies %/% img_dim[[1]]
    xCols = indicies %% img_dim[[1]]
    return(list(xCols,yRows))
  }
  xys_to_indicies = function(xs,ys,img_dim){
    indicies = (ys*img_dim[1]+xs)
    return(indicies)
  }
  
  #' Oblong / Loop length
  #' 
  #' Returns the longest eucledian distance between verticies in a loop
  #' @param loopList List of loops recieved from processHandwriting
  #' @param dim, dimensions of original image used to calculate x y coords from index
  #' 
  #' 
  loopMeasure = function(loopList, dims){
    #y coord
    #row modulo add 1
    #subtract 1 from the index, perform operation, add 1
    #col div add 1
    rowList = loopList %/% dims[1]
    #x coord
    colList = loopList %% dims[2]
    #whats the regular way to do this
    longestPath = -9999999
    targ_x1 = NULL
    targ_y1 = NULL
    targ_x2 = NULL
    targ_y2 = NULL
    i_index = NULL
    j_index = NULL
    for(i in 1:length(rowList)){
      y1 = rowList[i]
      x1 = colList[i]
      i_index = loopList[i]
      for(j in 1:length(rowList)){
        y2 = rowList[j]
        x2 = colList[j]
        euDist = sqrt((x2-x1)^2+(y2-y1)^2)
        if(euDist > longestPath){
          longestPath = euDist
          targ_x1 = x1
          targ_y1 = y1
          targ_x2 = x2
          targ_y2 = y2
          j_index = loopList[j]
        }
      }
    }
    #heres an idea of returning stuff
    #return(data.frame(valNames = c("pathLen","x1","x2","y1","y2"),index = c("hmm",i_index,j_index,i_index,j_index) ,vals=c(longestPath,targ_x1,targ_x2,targ_y1,targ_y2)))
    #heres another one that seems more useful for returning usable elements
    return(list(list(i_index,j_index),longestPath))
  }
  
  loopMeasures = function(loopListAll, dims){
    loopMeasure_results = list(1:length(loopListAll))
    loopMeasure_points = list(1:length(loopListAll))
    for(i in 1:length(loopListAll)){
      result = loopMeasure(loopListAll[[i]],dims)
      loopMeasure_results[[i]] = result
      loopMeasure_points[[i]] = result[[1]]
    }
    return(list(loopMeasure_points,loopMeasure_results))
  }
  
  getFeaturesFromGrapheme = function(grapheme){
    
  }
  getWidth = function(grapheme,img_dim){
    grapheme = indicies_to_xy(grapheme,img_dim)
    graphemeCols = grapheme[[1]]
    graphemeRows = grapheme[[2]]
    leftmostCol = which(graphemeCols == min(graphemeCols))[1]
    rightMostCol = which(graphemeCols == max(graphemeCols))[1]
    euDist = sqrt((graphemeRows[rightMostCol]-graphemeRows[leftmostCol])^2 +(graphemeCols[rightMostCol]-graphemeCols[leftmostCol])^2)
    return(euDist)
  }
  getHeight = function(grapheme,img_dim){
    grapheme = indicies_to_xy(grapheme,img_dim)
    graphemeCols = grapheme[[1]]
    graphemeRows = grapheme[[2]]
    leftmostRow = which(graphemeRows == min(graphemeRows))[1]
    rightMostRow = which(graphemeRows == max(graphemeRows))[1]
    euDist = sqrt((graphemeRows[rightMostRow]-graphemeRows[leftmostRow])^2 +(graphemeCols[rightMostRow]-graphemeCols[leftmostRow])^2)
    return(euDist)
  }
  #this probably returns the incorrect centroid
  #center of height and width, probably often doesn't end up in the actual letter
  centroidTest = function(grapheme,img_dim){
    grapheme = indicies_to_xy(grapheme,img_dim)
    graphemeCols = grapheme[[1]]
    graphemeRows = grapheme[[2]]
    leftmostRow = which(graphemeRows == min(graphemeRows))[1]
    rightMostRow = which(graphemeRows == max(graphemeRows))[1]
    leftmostCol = which(graphemeCols == min(graphemeCols))[1]
    rightMostCol = which(graphemeCols == max(graphemeCols))[1]
    centroidRow = ((graphemeRows[rightMostRow] - graphemeRows[leftmostRow])/2)+graphemeRows[leftmostRow]
    centroidCol = ((graphemeCols[rightMostCol] - graphemeCols[leftmostCol])/2)+graphemeCols[leftmostCol]
    return(list(centroidRow,centroidCol))
  }
  #i acknowledge unneeded operations are being used, just want to keep things organized
  getAspectRatio = function(grapheme,img_dim){
    return(getHeight(grapheme,img_dim)/getWidth(grapheme,img_dim))
  }
  getFeaturesFromDocument = function(graphemeList){
    
  }

#29 oct stuff

getWidthNew = function(grapheme,img_dim){
  grapheme = indicies_to_xy(grapheme,img_dim)
  yCols = grapheme[[1]]
  lmc_i = which(yCols == min(yCols))[1]
  rmc_i = which(yCols == max(yCols))[1]
  return(yCols[rmc_i]-yCols[lmc_i])
}
getHeightNew = function(grapheme,img_dim){
  grapheme = indicies_to_xy(grapheme,img_dim)
  xRows = grapheme[[2]]
  lmr_i = which(xRows== min(xRows))[1]
  rmr_i = which(xRows== max(xRows))[1]
  return(xRows[rmr_i]-yCols[lmr_i])
}

getAspectRatioNew = function(grapheme, img_dim){
  return(getHeightNew(grapheme,img_dim)/getWidthNew(grapheme,img_dim))
}
#end multiline comment
}

toRC = function(nodes, dims)
{
  cs = (nodes-1)%/%dims[1] + 1
  rs = (nodes-1)%%dims[1] + 1
  return(matrix(c(rs,cs), ncol = 2))
}
toRCi = function(nodes, dims)
{
  cs = (nodes-1)%/%dims[1] + 1
  rs = (nodes-1)%%dims[1] + 1
  rowcolmatrix = matrix(c(rs,cs,nodes), ncol = 3)
  colnames(rowcolmatrix) = c('y','x','index')
  return(rowcolmatrix)
}
#(column-1)*dim(image)[1] + row.
#ehh really weird behavior with non-ints
rc_to_i = function(row_y,col_x,img_dim)
{
  row_y = as.integer(row_y)
  col_x = as.integer(col_x)
  return((col_x-1)*img_dim[1]+row_y)
}
#30 oct 18 below


#returns list of {aspect ratio, vertical dist, horiz dist}
get_aspect_ratio = function(grapheme_list, img_dim)
{
  rowcol = toRCi(grapheme_list,img_dim)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  row_dist = max(rows_y) - min(rows_y) #vertical distance
  col_dist = max(cols_x) - min(cols_x) #horizontal distance
  aspect_info = matrix(c(row_dist/col_dist,row_dist,col_dist),ncol = 3)
  colnames(aspect_info) = c('aspect_ratio','height','width')
  return(aspect_info)
}
#average x,y coords
get_centroid = function(grapheme_list, img_dim)
{
  rowcol = toRCi(grapheme_list,img_dim)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  rc_to_i(mean(rows_y),mean(cols_x),img_dim)
  return()
}
