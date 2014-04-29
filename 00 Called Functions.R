#Used functions

Alph <- c("\n", "\r", "!", "*", "(", ")", "-", ".", ",", ";", 
          "?", "'", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
          "a", "A", "b", "B", "c", "C", "d", "D", "e", "E", "f", "F",
          "g", "G", "h", "H", "i", "I", "j", "J", "k", "K", "l", "L",
          "m", "M", "n", "N", "o", "O", "p", "P", "q", "Q", "r", "R",
          "s", "S", "t", "T", "u", "U", "v", "V", "w", "W", "x", "X",
          "y", "Y", "z", "Z", "¿")

#Which letter returns the numerical position in the Alph set of a given character.
whichLetter<- function(char = "¿", Alph) {
  k<-length(Alph)
  for(i in 1:length(Alph)){
    if(Alph[i] == char)
    {
      k<-i 
    }
  }
  k
}

#This function creates the transposition matrix for a given set of text.  
createTranspositionMatrix<- function(text,Alph, writeToFile = FALSE) {
  
  lettercounter<-rep(0,length(Alph))
  transAlph<-matrix(0,nrow=length(Alph),ncol=length(Alph))
  
  endminusone<-nchar(text)-1
  #ptm<- proc.time()
  #print(ptm)
  for(i in 1:endminusone)
  {
    
    first<-substr(text,i,i)
    second<-substr(text,i+1,i+1)
    xvar<-whichLetter(first,Alph)
    yvar<-whichLetter(second,Alph)
    transAlph[xvar,yvar]<-transAlph[xvar,yvar] + 1  
    lettercounter[xvar]<-lettercounter[xvar]+1
  }
  #counts the last letter, just for kicks
  first<-substr(text,endminusone+1,endminusone+1)
  xvar<-whichLetter(first,Alph)
  lettercounter[xvar]<-lettercounter[xvar]+1
  
  if(writeToFile)
  {
    write.matrix(transAlph, file = "transitionMatrixfull.csv", sep = ",")
    write(lettercounter, file = "letterCount.csv", sep = ",")
    #print(proc.time()-ptm)
  }
  else
  {
    transAlph
  }
}

#This function creates the transposition matrix for a given set of text.  
getLetterCount<- function(text,Alph) {
  
  lettercounter<-rep(0,length(Alph))
  
  end<-nchar(text)
  for(i in 1:end)
  {
    first<-substr(text,i,i)
    xvar<-whichLetter(first,Alph)
    lettercounter[xvar]<-lettercounter[xvar]+1
  }
  lettercounter
}

############################################################################3
#sum(letterfreq)

rowDivide<- function(transAlph, decoder=TRUE) {
  if(decoder==TRUE)
  {
    max<-sum(transAlph)
  }
  
for(i in 1:nrow(transAlph))
{
  k<-sum(transAlph[i,])
  if(decoder==TRUE)
  {
    if(k==0)
    {
      transAlph[i,]<-1/max
    }
    else
    {
      transAlph[i,]<-transAlph[i,]/k
    }
  }
  else{    
    transAlph[i,]<-transAlph[i,]/k
  }
}
transAlph
}

logAndReplace<- function(transAlph) {
  #browser()
  logAlph<-log(transAlph)
  
  #browser()
  replaceme<- function(x){
    if(is.infinite(x))
    {
      -12
    }
    else if(is.nan(x) == TRUE)
    {
      -12
    }
    else
      x
  }
  #logAlph[1,1] == -Inf
  #replace all zero entries that become -infinity with -12, as suggested byt he paper. 
  #mLogAlph<-matrix(lapply(logAlph, replaceme), ncol = ncol(logAlph))
  #mLogAlph<-sapply(logAlph, replaceme)
  
  mLogAlph<-matrix(sapply(logAlph, replaceme), ncol = ncol(logAlph))
  
  mLogAlph
}

#Adds 1 to zero entries, then divides by total to get probabilities.
fixLetterCount<- function(lettercount){
  
  for(i in 1:length(lettercount))
  {
    if(lettercount[i]==0)
    {
      lettercount[i]<-1
    }
    
  }
  letterfreq<-lettercount/sum(lettercount)
  
  letterfreq
}


swapTheAlph<-function(AlphSwapper, n, m)
{
  #  p<-sample(AlphSwapper,2)
  #  first<-whichLetter(p[1], AlphSwapper)
  #  second<-whichLetter(p[2], AlphSwapper)
  #  AlphSwapper[m]<-p[2]
  #  AlphSwapper[second]<-p[1]
  
  first<-AlphSwapper[n]
  second<-AlphSwapper[m]
  AlphSwapper[m]<-first
  AlphSwapper[n]<-second
  
  AlphSwapper
}

