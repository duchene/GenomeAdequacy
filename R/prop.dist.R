# This function calculates the topological distance between two trees as a proportion of the maximum possible distance for a tree of their size.

prop.dist <- function(t1, t2){
	  d <- dist.topo(t1, t2) / ((2*length(t1$tip.label)) - 3) 
}