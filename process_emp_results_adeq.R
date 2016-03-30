resfiles <- grep("[.]Rdata", dir(), value = T)
empres <- matrix(0, nrow = length(resfiles), ncol = 2)

colnames(empres) <- c("delta+trlen+chisq", "delta+trlen+chisq+multno+homopl")

for(i in 1:length(resfiles)){
      load(resfiles[i])
      empres[i, 1] <- 0
      delt.trlen.chisq.multno.homlp <- 0
      if((rT[[2]] > 0.05 && rT[[2]] < 0.95) && (rT[[6]] > 0.05 && rT[[6]] < 0.95) &&(rT[[7]] > 0.05 && rT[[7]] < 0.95)) empres[i, 1] <- 1
      if((empres[i, 1] == 1) && (rT[[1]] > 0.05 && rT[[1]] < 0.95) && (rT[[3]] > 0.05 && rT[[3]] < 0.95)) empres[i, 2] <- 1
}
