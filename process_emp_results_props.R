resfiles <- grep("[.]Rdata", dir(), value = T)
empres <- matrix(0, nrow = 1, ncol = 7)
colnames(empres) <- c("multinomial", "chisq", "homoplasy", "meanbrsup", "CIbrsup", "delta", "trlen")
rtind <- seq(2, 14, 2)

for(i in 1:length(resfiles)){
      load(resfiles[i])
      for(j in 1:7){
      	    # Note that the following line means that the output is in terms of failure rates.
            if(rT[[rtind[j]]] < 0.05 | rT[[rtind[j]]] > 0.95) empres[1, j] <- empres[1, j] + 1
      }
}

empres <- empres / length(resfiles)