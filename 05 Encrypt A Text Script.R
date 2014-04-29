setwd("C:/Dropbox/267 Project")

source("00 Called Functions.R")
#filetoEncrypt<-'OliverTwist - firstChap.txt'

filetoEncrypt<- 'JaneEyre - firstChap.txt'
unencrypted<-readChar(filetoEncrypt, file.info(filetoEncrypt)$size)

AlphCode<-Alph
seq<-1:length(Alph)

#Oliver
#set.seed(101)

#Jane
set.seed(95)

Alphencoder<-sample(Alph)

decoder<-matrix(c(Alph,Alphencoder), nrow = length(Alph), ncol = 2)

end<-nchar(unencrypted)
decryptedVector<-vector(mode="character",length=end)

for(i in 1:end)
{
  first<-substr(unencrypted,i,i)
  xvar<-whichLetter(first,Alph)
  decryptedVector[i]<-decoder[xvar,2]  
}
encoded<-paste(decryptedVector, collapse = "")

#con<-"Oliver Encoded.txt"
con<-"JaneEyre Encoded.txt"

writeChar(encoded, con)

transCount<-createTranspositionMatrix(unencrypted, Alph, writeToFile=FALSE)
#transUnecrypted<-rowDivide(transCount, decoder==TRUE)
longunencrypted<- -sum((transCount)%*%t(logAlph))

longunencrypted

sum(transCount)


#> logunecrypt Oliver
#[1] 3542460
# logger[999999] = 3386948

#> longunencrypted Jane Eyre
#[1] 6116104