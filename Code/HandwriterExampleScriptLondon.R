devtools::install_github("CSAFE-ISU/handwriter")
library(handwriter)

data(london)

london = crop(london)
plotImage(london)

london_thin = thinImage(london)
plotImageThinned(london, london_thin)

london_nodes = getNodes(london_thin)
plotNodes(london, london_thin, london_nodes)
