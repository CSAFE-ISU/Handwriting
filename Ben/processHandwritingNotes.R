#For sure need to find / better understand a solution for the built package running out of sync with the one inside of R-STUDIO at any given time. Maybe a build function? 
#Something is absolutely wrong with my current reference to the handwriter package, masking over the package by manually sourcing the file first seems to resolve most issues
processHandwriting = function(img, dims)
{
  # Next, we have to follow certain rules to find non intersection breakpoints.

  cat("Starting Processing...\n")
  #Thin image essentially is a 1D matrix of indices marking where a pixel exists
  #We save it as indices
  indices = img
  #Convert img's index values to all 1 given dimensions (all indicies) of img
  img = matrix(1, nrow = dims[1], ncol = dims[2])
  #Using known indicies where pixels reside, mark these all as filled in img
  img[indices] = 0
  cat("Getting Nodes...\n")
  #Gets nodes
  #Summary:
  #Mark endpoints and intersections of thinned image
  #What does cbind do? with %% %/%
  #Room for improvement on 2x2
  #So now we have a list of nodes
  nodeList = getNodes(indices, dims)
  #Do these convert indices to col, rows?
  img.m = cbind(((indices-1) %% dims[1]) + 1, ((indices - 1) %/% dims[1]) + 1)
  #Empty matrix of indices (row) by 8 (col)
  neighborList = matrix(NA, nrow = indices, ncol = 8)
  #This takes the empty neighborList and populates it using the whichNeighbors function to return back the rows/cols (??) where neighbors occur
  #Summary of whereNeighbors: I believe it applies a check all around each node to each index in the row, col cords, marking with a 1 when black neighbors are found
  neighborList = t(apply(as.matrix(img.m, ncol = 2), 1, whichNeighbors, img = img))
  #What is a molten data frame? It looks like var1 is the index from leftmost to rightmost that resets each new line in the binary iamge, with the unnammed variable being the index
  graphdf = melt(neighborList)
  #subset seems to filter out everything without any black neighbors
  graphdf = subset(graphdf, value == 1)
  #adds a from, so where the index of the source of the neighbor is from?
  #occurances of 2 in col var2 (which I interpret as 2 black neighbors) have a single from as well
  graphdf$from = indices[graphdf$Var1]
  #same as above comment but the to, or the neighbors it reaches? I believe?
  graphdf$to = graphdf$from + c(-1, dims[1]-1, dims[1], dims[1] + 1, 1, 1-dims[1], -dims[1], -1-dims[1])[graphdf$Var2]
  #various distances used for calculations
  graphdf$man_dist = rep(c(1,2), 4)[graphdf$Var2]
  #i believe this is farthest distance btwn two points on a graph
  graphdf$euc_dist = c(1,sqrt(2), 1, sqrt(2), 1, sqrt(2), 1, sqrt(2))[graphdf$Var2]
  #do not know what this is
  graphdf$pen_dist = c(1,3,1,3,1,3,1,3)[graphdf$Var2]
  #if any of these distances are used, i will comment **M **E **P respectively

  #very slight differences between the two matricies, exploring whichNeighbors0...
  #pretty much the same thing a neighborList except excludes diags, assuming to aid in preventing 1 px loop around paths that misdirect nodes

  neighborList0 = matrix(NA, nrow = indices, ncol = 8)
  neighborList0 = t(apply(as.matrix(img.m, ncol = 2), 1, whichNeighbors0, img = img))
  graphdf0 = melt(neighborList0)
  graphdf0 = subset(graphdf0, value == 1)
  graphdf0$from = indices[graphdf0$Var1]
  graphdf0$to = graphdf0$from + c(-1, dims[1]-1, dims[1], dims[1] + 1, 1, 1-dims[1], -dims[1], -1-dims[1])[graphdf0$Var2]
  #the title of the col makes sense but I'm not sure what it means
  graphdf0$nodeOnlyDist = ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0)
  #not sure what this is doing, converting from number to char?
  #so the subset doesn't remove graphdf0?
  graphdf0$from = as.character(graphdf0$from)
  graphdf0$to = as.character(graphdf0$to)
  graphdf0 = subset(graphdf0, select = c(from, to, nodeOnlyDist))
  #something similar
  graphdf$from = as.character(graphdf$from)
  graphdf$to = as.character(graphdf$to)
  #retain only distances and char representation of from and to
  graphdf = subset(graphdf, select = c(from, to, man_dist, euc_dist, pen_dist))
  #so we have graphdf being from, to, and **M **E **P
  #and graphdf0 being just the node distances and from to indices
  #skel_grpah turning these into lists
  skel_graph = graph_from_data_frame(d = graphdf, vertices = as.character(indices), directed = FALSE)
  skel_graph0 = graph_from_data_frame(d = graphdf0, vertices = as.character(indices), directed = FALSE)
  #extremely lost with skel_graph, infinitely nested lists of indices?
  skel_graph = simplify(skel_graph, remove.multiple = TRUE, edge.attr.comb="mean")
  skel_graph0 = simplify(skel_graph0, remove.multiple = TRUE, edge.attr.comb="mean")
  #agian, not sure
  V(skel_graph)$color = ifelse(V(skel_graph)$name %in% nodeList, 1, 0)
  V(skel_graph0)$color = ifelse(V(skel_graph0)$name %in% nodeList, 1, 0)
  #maybe distances between nodes? using skel_graph0 as weights for these distances?
  dists0 = distances(skel_graph0, v = as.character(nodeList), to = as.character(nodeList), weights = E(skel_graph0)$nodeOnlyDist)
  #an adjanacy matrix?
  adj0 = ifelse(dists0 == 1 | dists0 == 2, 1, 0)
  #lower and upper part of this matrix? what is a triangular part? consideration of diags now?
  adj0[lower.tri(adj0)] = 0
  #isolate var1s var2s (from, tos) to their value
  adj.m = melt(adj0)
  adj.m = subset(adj.m, value == 1)
  names(adj.m) = c("from", "to", "value")

  cat("Finding direct paths..")
  #Getting paths in isolation
  #Consider only paths that are not loops to itself (end on its src)
  #Using from and to, traverse and delete edges as we go to find a path from one node to the other, not deleting edges will result in paths
  pathList = AllUniquePaths(adj.m, skel_graph, skel_graph0)
  cat("and loops...\n")
  #absolutely want to implement the returning of a list as well
  #hmm internally it seems super complex but the resultant and finding the furthest point should be what matters
  loopList = getLoops(nodeList, skel_graph, skel_graph0, pathList, dim(img))

  allPaths = append(pathList, loopList)
  #candidates for consideration
  #distance between nodes? horizontal / vert / euc

  #I believe oblong both eucledian as well as x and y distance from the two points could tell us something

  #height and width
  #width can be taken from distance between two breaks
  #quantity of loops compared against:
  #graphemes

  ####################### This is after path finding. Find breakpoints and check rules for removal.
  #Nominate and check candidate breakpoints
  cat("Looking for letter break points...")
  troughNodes = c()
  candidateNodes = c()
  for(i in 1:length(allPaths))
  {
    # Look for troughs in edges.
    hasTrough = FALSE
    tempPath = allPaths[[i]]
    if(length(tempPath) > 10)
    {
      rows = ((tempPath-1) %% dims[1]) + 1
      for(j in 6:(length(rows)-5))
      {
        if(any(rows[1:(j-1)] < rows[j]-1) & any(rows[(j+1):length(rows)] < rows[j]-1))
        {
          lowerEnd = max(which(rows[1:(j-1)] < rows[j]-1))
          upperEnd = min(which(rows[(j+1):length(rows)] < rows[j]-1))
          if(!any(rows[lowerEnd:(j+upperEnd)] > rows[j]))
          {
            troughNodes = c(troughNodes, tempPath[j])
            hasTrough = TRUE
          }
        }
      }
    }
    if(hasTrough == FALSE)
    {
      candidateNodes = c(candidateNodes, tempPath[ceiling(length(tempPath)/2)])
    }
  }
  breaks = which((((troughNodes[-1]-1) %% dims[1]) + 1) != (((troughNodes[-length(troughNodes)] - 1) %% dims[1]) + 1) |
                   ((((troughNodes[-1]-1) %/% dims[1]) + 1) != (((troughNodes[-length(troughNodes)] - 1) %/% dims[1])) &
                      (((troughNodes[-1]-1) %/% dims[1])) != (((troughNodes[-length(troughNodes)] - 1) %/% dims[1]) + 1)))
  breaks = c(1, breaks, length(troughNodes))
  candidateNodes = c(candidateNodes, troughNodes[ceiling((breaks[-1] + breaks[-length(breaks)])/2)])

 # print(plotPath(candidateNodes, img, img, zoomBorder = NA))
  cat("and discarding bad ones...\n")
  
  goodBreaks = checkBreakPoints(candidateNodes = candidateNodes, allPaths = allPaths, nodeGraph = getNodeGraph(allPaths, nodeList), dims)
  preStackBreaks = candidateNodes[goodBreaks]
  
  ##################### Potential breakpoints (except for stacked graphemes) found. Break into grapheme paths.

  cat("Isolating letter paths...")
  #after appropriate breaks are found, categorize via grapheme 
  graphemesList = graphemePaths(allpaths, skel_graph0, preStackBreaks)
  graphemes = graphemesList[[1]]
  V(skel_graph0)$graphemeID = graphemesList[[2]]

  finalBreaks = preStackBreaks[!(checkStacking(preStackBreaks, allPaths, graphemes, skel_graph0, dims))]
  
  pathsWithBreaks = lapply(pathList, function(x){which(x %in% finalBreaks)})
  for(i in which(lapply(pathsWithBreaks, length) > 0))
  {
    newNodes = which(pathList[[i]] %in% finalBreaks)
    newNodes = c(newNodes + 1, newNodes - 1)
    nodeList = c(nodeList, pathList[[i]][newNodes])
    pathList[[i]] = list(pathList[[i]][1:(newNodes[1]-1)], pathList[[i]][(newNodes[1]-1):length(pathList[[i]])])
  }
  
  if(any(unlist(lapply(pathsWithBreaks, length) > 0)))
  {
    pathList = unlist(pathList, recursive = FALSE)
  }
  
  graphemesList = graphemePaths(allpaths, skel_graph0, finalBreaks)
  graphemes = graphemesList[[1]]
  
  graphemes = graphemes[unlist(lapply(graphemes, length)) > 5]
  cat("and done.\n")
  return(list(thin = indices, nodes = nodeList, breakPoints = finalBreaks, pathList = allPaths, graphemeList = graphemes))
}
