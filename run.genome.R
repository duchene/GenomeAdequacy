# This function runs model adequacy assessment for genes in a file.


run.genome <- function(format = "phillip", model = "GTR+G", phymlPath){
	   require(phangorn)
	   
	   genes <- dir()
	   geneStats <- list
	   
	   for(i in 1:length(genes)){
	   	 geneStats[[i]] <- run.gene(genes[i], format = format, model = model, phymlPath = phymlPath)
	   }
	   
	   names(geneStats) <- genes

	   genomeStats <- matrix(NA, nrow = length(genes), ncol = 7)
	   elements <- seq(1, 13, by = 2)
	   for(i in 1:7){
	   	 genomeStats[i,] <- sapply(geneStats, function(x) x[[elements[i]]])
 	   }
	   rownames(genomeStats) <- genes
	   colnames(genomeStats) <- names(geneStats[[1]])[elements]

	   return(genomeStats)

}