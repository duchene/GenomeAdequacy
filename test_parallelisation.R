for(i in dir('R', pattern = '.+R$')){
    source(paste0('R/', i))
}


# Example to parallelise a gene run
t1 <- run.gene('1.phy', phymlPath = '~/Downloads/PhyML-3.1/PhyML-3.1_macOS-MountainLion', Nsims = 4, para = T, ncore = 5)

# Example to parallelise a genome run
g1 <- run.genome(phymlPath = '~/Downloads/PhyML-3.1/PhyML-3.1_macOS-MountainLion', Nsims = 5, paragene = T, ncore = 4)

