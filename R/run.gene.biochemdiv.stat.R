run.gene.biochemdiv.stat <- function(sdata, format = "phyllip", model = "GTR+G", phymlPath = "~/Desktop/Software/PhyML-3.1/PhyML-3.1_macOS-MountainLion", Nsims = 100, para = F, ncore = 1){

         # Get test statistics

         if(format == "phyllip"){
                  data <- read.dna(sdata)
         } else if(format == "fasta"){
                  data <- read.dna(sdata, format = "fasta")
         }

	 empstats <- get.test.statistics(sdata, format = format, geneName = sdata, phymlPath = phymlPath, model = model)
	 
         empdiv <- biochemdiv.stat(data)

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
	 divs <- sapply(sim, biochemdiv.stat)
	 divs.p <- length(which(divs < empdiv)) / length(divs)
	 res <- list(emp.div = empdiv, sim.divs = divs, divs.p.value = divs.p)
	 return(res)
}