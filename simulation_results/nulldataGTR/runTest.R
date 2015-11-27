for(f in dir('../R', pattern = '.+R$')){
      source(paste0('../R/', f))
}

args <- commandArgs(trailingOnly = TRUE)

fName <- args[1]

print(fName)