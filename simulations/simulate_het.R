

get.arbitrary.hetseqs <- function(ntax = 36, slen = 1000){
    require(phangorn)
    tr1 <- rtree(ntax - 17)
    tr1$edge.length <- rexp(length(tr1$edge.length))
    #tr1$edge.length <- rlnorm(length(tr1$edge.length), meanlog = -2, sd = 0.3)
    tr1$edge.length <- tr1$edge.length/sum(tr1$edge.length) * 1.5
    s1 <- simSeq(tr1, l = slen, Q = c(1.34, 4.81, 0.93, 1.24, 5.56, 1), bf = c(0.1, 0.4, 0.4, 0.1))
    
    tr2 <- rtree(18)
    tr2$edge.length <- rexp(length(tr2$edge.length))
    #tr2$edge.length <- rlnorm(length(tr2$edge.length), meanlog = -2, sd = 0.3)
    tr2$edge.length <- tr2$edge.length/sum(tr2$edge.length) * 1.5
    s2 <- simSeq(tr2, l = slen, rootseq =  as.character(as.DNAbin(s1)[1, ]), Q = c(1.34, 4.81, 0.93, 1.24, 5.56, 1), bf = c(0.4, 0.1, 0.1, 0.4))

    tr3 <- bind.tree(tr1, tr2, where = 1)
    tr3$tip.label <- paste0('t', 1:length(tr3$tip.label))

    names(s1) <- paste0('t', 1:length(s1))
    names(s2) <- paste0('t', length(s1):(length(s1)-1 + length(s2)))

    s3 <- rbind(as.DNAbin(s1)[-length(s1), ], as.DNAbin(s2))
        
    return(list(tr3, s3))
}
