
library(ape)

fasta_files <- dir(pattern = 'fasta$')

for(f in fasta_files){
      print(f)
      temp_f <- read.dna(f, format = 'fasta')
      write.dna(temp_f, file = gsub('fasta' , 'phy', f))
}