mg_readCLSfile<-function(filename="NULL")
{
  if (regexpr(".cls$",filename)==-1)
  {
    stop("### class data should be .cls file! ###")
  }


  line1 <- scan(filename, nlines=1, what="character", quiet=TRUE)

  numberOfSamples <- as.integer(line1[1])
  numberOfClasses <- as.integer(line1[2])


  cls<-as.vector(as.matrix(read.delim(filename,header=F,sep=" ",skip=2)))


  if (is.na(as.numeric(cls[1])))
  {
    stop("### 3rd line of cls file should be numeric! ###")
  }

  if (min(as.numeric(cls))==0)
  {
    cls<-as.numeric(cls)+1
  }

  if(numberOfSamples!=length(cls)) {
    stop(paste("\nFound ", length(cls), "labels on line 3 of", filename,  "but expected", numberOfSamples, sep=' '))
  }

  if(numberOfClasses!=length(unique(cls))) {
    stop(paste("\nFound ", length(unique(cls)), "classes on line 3 of", filename,  "but expected", numberOfClasses, sep=' '))
  }

  return(cls)
}
