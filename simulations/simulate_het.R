

get.arbitrary.hetseqs <- function(ntax = 36, slen = 1000){
    require(phangorn)
    tr1 <- rtree(ntax - 26)
    tr1$edge.length <- rexp(length(tr1$edge.length))
    #tr1$edge.length <- rlnorm(length(tr1$edge.length), meanlog = -2, sd = 0.3)
    tr1$edge.length <- tr1$edge.length/sum(tr1$edge.length) * 1.5
    s1 <- get.arbitrary.seq(tr1, model = "gtr.g", l = slen)
    
    tr2 <- rtree(9)
    tr2$edge.length <- rexp(length(tr2$edge.length))
    #tr2$edge.length <- rlnorm(length(tr2$edge.length), meanlog = -2, sd = 0.3)
    tr2$edge.length <- tr2$edge.length/sum(tr2$edge.length) * 1.5
    s2 <- get.arbitrary.seq(tr2, model = "gtr.g", l = slen, rootseq =  as.character(as.DNAbin(s1)[1, ]))

    tr3 <- bind.tree(tr1, tr2, where = 1)
    tr3$tip.label <- paste0('t', 1:length(tr3$tip.label))

    names(s1) <- paste0('t', 1:length(s1))
    names(s2) <- paste0('t', length(s1):(length(s1)-1 + length(s2)))

    s3 <- rbind(as.DNAbin(s1)[-length(s1), ], as.DNAbin(s2))
        
    return(list(tr3, s3))
}
