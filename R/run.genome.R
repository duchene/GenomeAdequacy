# This function runs model adequacy assessment for genes in a file.


run.genome <- function(format = "phyllip", model = "GTR+G", phymlPath, Nsims = 100){
	   require(phangorn)
	   print(format)
	   print(grep(".+phy", dir(), value = T))
	   if(format == "phyllip"){
	   	   genes <- grep(".+phy", dir(), value = T)
		   print(genes)
	   } else if(format == "fasta"){
	     	   genes <- grep(".+[.]fasta", dir(), value = T)
	   }
	   geneStats <- list()
	   print(genes)
	   for(i in 1:length(genes)){
	   	 print(genes[i])
		 genes[i]
	   	 geneStats[[i]] <- run.gene(genes[i], format = format, model = model, phymlPath = phymlPath, Nsims = Nsims)
		 print(geneStats[[i]])
	   }
	   
	   names(geneStats) <- genes

	   genomeStats <- matrix(NA, nrow = length(genes), ncol = 14)
	   for(i in 1:14){
	   	 genomeStats[,i] <- sapply(geneStats, function(x) x[[i]])
 	   }
	   geneTrees <- sapply(geneStats, function(x) x[[15]])
	   names(geneTrees) <- genes
	   
	   if(model == "GTR+G"){
	   	    geneInferences <- list()
		    for(i in 1:length(geneStats)){
		    	  geneInferences[i] <- geneStats[[i]][16:18]
		    }
		    names(geneInferences) <- genes
	   }	   


	   rownames(genomeStats) <- genes
	   colnames(genomeStats) <- names(geneStats[[1]])[1:14]
	   results <- list(genome.results = genomeStats, empirical.trees = geneTrees, empirical.parameters)

	   return(genomeStats)

}