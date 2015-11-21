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

	   genomeStats <- matrix(NA, nrow = length(genes), ncol = 7)
	   elements <- seq(1, 13, by = 2)
	   for(i in 1:7){
	   	 genomeStats[i,] <- sapply(geneStats, function(x) x[[elements[i]]])
 	   }
	   rownames(genomeStats) <- genes
	   colnames(genomeStats) <- names(geneStats[[1]])[elements]

	   return(genomeStats)

}