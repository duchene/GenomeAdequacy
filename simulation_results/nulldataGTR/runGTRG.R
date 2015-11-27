library(ape)
library(phangorn)

for(f in dir('../R', pattern = '.+R$')){
      source(paste0('../R/', f))
}

args <- commandArgs(trailingOnly = TRUE)

fName <- args[1]

rT <- run.gene(fName, phymlPath = 'phyml', Nsims = 100, para = T, ncore = 10)
save(rT, file = paste0(fName, 'results_GTRG.Rdata'))
