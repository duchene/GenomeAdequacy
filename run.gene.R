# Runs get.test.statistics for a given gene and its simulations.

run.gene <- function(sdata, format = "phyllip", model = "GTR+G", phymlPath, Nsims = 100){
	 
	 # Get test statistics
	 print(format)
	 if(format == "phyllip"){
                  data <- read.dna(sdata)
         } else if(format == "fasta"){
                  data <- read.dna(sdata, format = "fasta")
         }
	 
	 empstats <- get.test.statistics(sdata, format = format, geneName = sdata, phymlPath = phymlPath, model = model)

	 # Simulate data sets.

	 l <- ncol(data)
	 sim <- list()
	 for(i in 1:Nsims){
	       if(model == "GTR+G"){
               	      rates = phangorn:::discrete.gamma(empstats$alphaParam, k = 4)
               	      rates <- rates + 0.001
               	      sim_dat_all <- lapply(rates, function(r) simSeq(empstats$outputTree, l = round(l/4, 0), Q = empstats$gtrMatrix, bf = empstats$piParams, rate = r))
               	      sim[[i]] <- as.DNAbin(c(sim_dat_all[[1]], sim_dat_all[[2]], sim_dat_all[[3]], sim_dat_all[[4]]))
	       } else if(model == "JC"){
	       	      sim[[i]] <- as.DNAbin(simSeq(empstats$outputTree, l = l))
	       }
	 
	 }
	 

	 # Get test statistics for simulations.

	 sim.stats <- list()
	 print("STARTED RUNNING SIMS")
	 for(i in 1:Nsims){	       
	       sim.stats[[i]] <- get.test.statistics(sim[[i]], format = "DNAbin", geneName = paste0(sdata, "_sim_", i), phymlPath = phymlPath, model = model)
	 }

	 # Get P-values for test statistics.
	 
	 multinoms <- sapply(sim.stats, function(x) x[[1]])
	 multinom.p <- length(which(multinoms < empstats[[1]])) / length(sim)
	 chisqs <- sapply(sim.stats, function(x) x[[2]])
	 chisq.p <- length(which(chisqs < empstats[[2]])) / length(sim)
	 homop <- sapply(sim.stats, function(x) x[[3]])
	 homo.p <- length(which(homop < empstats[[3]])) / length(sim)
	 meanbrsup <- sapply(sim.stats, function(x) x[[4]])
	 meanbrsu.p <- length(which(meanbrsup < empstats[[4]])) / length(sim)
	 CIbrsup <- sapply(sim.stats, function(x) x[[5]])
	 CIbrsu.p <- length(which(CIbrsup < empstats[[5]])) / length(sim)
	 deltas <- sapply(sim.stats, function(x) x[[6]])
	 delta.p <- length(which(deltas < empstats[[6]])) / length(sim)
	 trlens <- sapply(sim.stats, function(x) x[[7]])
	 trlen.p <- length(which(trlens < empstats[[7]])) / length(sim) 
	 
	 results <- list(empirical.multlik = empstats[[1]], multlik.p.value = multinom.p, empirical.chisq = empstats[[2]], chisq.p.value = chisq.p, empirical.homoplasy = empstats[[3]], homoplasy.p.value = homo.p, empirical.mean.branch.sup = empstats[[4]], mean.branch.sup.p.value = meanbrsu.p, empirical.CI.branch.sup = empstats[[5]], CI.branch.sup.p.value = CIbrsu.p, empirical.delta = empstats[[6]], delta.p.value = delta.p, empirical.tree.length = empstats[[7]], tree.length.p.value = trlen.p, empirical.tree = empstats$outputTree)

	 if(model == "GTR+G"){
                 results$gtrMatrix <- empstats$gtrMatrix
                 results$piParams <- empstats$piParams
                 results$alphaParam <- empstats$alphaParam
         }

	 return(results)

}