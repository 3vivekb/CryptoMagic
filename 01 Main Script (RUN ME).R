#Work of Vivek Bansal
#5/13/2013
#Math 267 Final Project
#Solving Substititon Codes using MCMC
#Professor Bee Leng Lee
#Algorithm based on Stephen Connor's Dissertation at Warwick 

#Coding style is Verbosity


setwd("C:/Dropbox/267 Project/To Submit")

library(MASS)

library(tseries)
#Used for read.matrix which makes something slightly different then coercing a data frame into a matrix.  
#Coercing seems to screw up my matrices...


source("00 Called Functions.R")

#This encrypted text is different
#filethatisEncrypted<-'trans4mind.txt'

filethatisEncrypted<-'Oliver Encoded.txt'
set.seed(175)

#filethatisEncrypted<-'JaneEyre Encoded.txt'
#set.seed(195)
encrypted<-readChar(filethatisEncrypted, file.info(filethatisEncrypted)$size)


#This set of functions below could take an hour to process - thus a shortcut to an already made matrix below
#reftextfilename <- 'edited war and peace.txt'
#warandpeace<-readChar(reftextfilename, file.info(reftextfilename)$size)
#reference<-warandpeace
#createTranspositionMatrix(reference, Alph, writeToFile=TRUE)


#Used with tseries - makes something slightly different, doesn't get screwed up matrices later.
#Gets transposition matrix from War and Peace, then row divides, and takes the log values 
#With -12 substitiution -infinity
transAlph<-read.matrix("transitionMatrixfull.csv", header = FALSE, sep = ",", skip = 0)
transAlph2<-rowDivide(transAlph, decoder=FALSE)
logAlph<-logAndReplace(transAlph2)

#Again Cheating
lettercount<-scan(file = "letterCount.csv", sep = ",")

#I don't know if I need to fix the letter count, but I will (to get rid of zero probabilities)
letterFreqProb<- fixLetterCount(lettercount)

########################################################################################################
#Step 1 Calculate the transition matrix of encrypted under the identity permutation (i.e. take alph = Alph)
countDecryptedPrior<-createTranspositionMatrix(encrypted, Alph, writeToFile=FALSE)

AlphCode<-Alph

###############################################
#0.5
#This section starts AlphCode under the intelligent starting point based on letter frequencies
letterCountEncrypted<-getLetterCount(encrypted, Alph)
rankEncrypted<-rank(letterCountEncrypted, ties.method="first")

rankMaster<-rank(lettercount, ties.method="first")

#if in the encrypted text I get the most of that letter, it should be the most used Alph letter.  Thus Rank 1 in the Alph text 
#(position 17)    should be assigned to rank 1 in the AlphCode text, where that is at position 1.  
j<-1
for(i in 1:length(Alph))
{
  
  for(j in 1:length(Alph)){
    if (rankEncrypted[j]==rankMaster[i]){
      AlphCode[j]<-Alph[rankMaster[i]]
    }
  }
  j<-1
}

#Sequence to randomly pick transpositions from.
seq<-1:length(Alph)

#Step 1.5 Calculate prior probability of first letter of T under the original permutation
priorfirstDecryptedLetter<-substr(encrypted,1,1)
whichPos<-whichLetter(priorfirstDecryptedLetter,Alph)
Btprior<-letterFreqProb[whichPos]
#######################################################################################################
#Iteration zone

iterations<-20000
printer<-0
alphsaver<-vector(mode="numeric",length=iterations)
logger<-vector(mode="numeric",length=iterations)

output<-FALSE

for(j in 1:iterations){

  #Step 2: Pick two numbers at random that are from the length of our Alph set.  
  swaps<-sample(seq,2)
  #Take those numbers and permute the Alph Decoder
  AlphCodeProp<-swapTheAlph(AlphCode,swaps[1],swaps[2])
  
  decoder<-matrix(c(Alph,AlphCodeProp), nrow = length(Alph), ncol = 2)
  
  
  #Every so often output Decrypted results to the user
  if(output==TRUE)
  {
  end<-nchar(encrypted)
  decryptedVector<-vector(mode="character",length=end)
  #Decrypt the text according to the key
  for(i in 1:end)
  {
    first<-substr(encrypted,i,i)
    xvar<-whichLetter(first,Alph)
    decryptedVector[i]<-decoder[xvar,2]  
  }
  decrypted<-paste(decryptedVector, collapse = "")
  print(decrypted)
  cat(j,"iterations", '\n')
  output<-FALSE
  }
  
  #Step three calculate the proposal transition matrix of the permutaton arising from the above transposition
  countDecryptedProposal <- countDecryptedPrior
  #Swap rows
  temp<- countDecryptedProposal[swaps[1],]
  countDecryptedProposal[swaps[1],]<- countDecryptedProposal[swaps[2],]
  countDecryptedProposal[swaps[2],]<-temp
  #Alph2
  #Swap columns
  temp<- countDecryptedProposal[,swaps[1]]
  countDecryptedProposal[,swaps[1]]<- countDecryptedProposal[,swaps[2]]
  countDecryptedProposal[,swaps[2]]<-temp
  
  #Step 4. Calculate the prior probability of the first letter of T (aka encoded text decrytped) under the new permutation
  firstEncryptedLetter<-substr(encrypted,1,1)
  whichPos<-whichLetter(firstEncryptedLetter,Alph)
  decodedLetter<-decoder[whichPos,2]
  whichPos2<-whichLetter(decodedLetter,Alph)
  Btprop<-letterFreqProb[whichPos2]
  
  #5. Calculate the acceptance probability
  kappa<- log(Btprop) - log(Btprior)
  alphaPart<-sum((countDecryptedProposal - countDecryptedPrior)%*%t(logAlph))
  kt<-kappa + alphaPart
  
  logalph<-min(0,kt)
  #Save Alphas and negative log likelihoods for graphing
  logger[j]<- sum((-countDecryptedPrior)%*%t(logAlph))
  
  
  alph<-exp(logalph)
  alphsaver[j]<-alph
  
  if(runif(1) < alph)
  {
    countDecryptedPrior<-countDecryptedProposal
    Btprior<-Btprop
    AlphCode<-AlphCodeProp  
  
  }
  #else
    #stuff stays the same
  
  
  printer<-printer+1
  if(printer==1000)
  {
    printer<-0
    output<-TRUE
  }  
}
#End of iterations

#Check for changes in the decoder
decoder[,1]==decoder[,2]

plot(alphsaver, xlim = c(0,2000), main = "Decoding Oliver Twist first 2000 iterations", ylab = "Alpha values", xlab = "Iteration Count")
plot(alphsaver, main = "Decoding Oliver Twist, 20000 iterations", ylab = "Alpha values", xlab = "Iteration Count")

plot(logger, main = "Decoding Oliver Twist, 20000 iterations", ylab = "NegativeLog-of Trans Matrices Statistic", xlab = "Iteration Count")
plot(logger, main = "Decoding Oliver Twist", ylab = "NegativeLog-of Trans Matrices Statistic", xlab = "Iteration Count")
#600 400
#seed = 175
plot(logger,xlim = c(0,2000), main = "Decoding Oliver Twist first 2000 iterations", ylab = "NegativeLog-of Trans Matrices Statistic", xlab = "Iteration Count")
