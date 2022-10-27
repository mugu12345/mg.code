mg_readGCTfile<-function(filename="NULL")
{
  if (regexpr(".gct$",filename)==-1)
  {
    stop("### input data should be .gct file! ###")
  }

  # line 2 dimensions
  dimensions <- scan(filename, what=list("integer", "integer"), nmax=1, skip=1, quiet=TRUE)
  rows <- dimensions[[1]]
  columns <- dimensions[[2]]

  cat("\ndimensions: ")
  print(dimensions)

  data<-read.delim(filename, header=T, sep="\t", skip=2, row.names=1, blank.lines.skip=T, comment.char="", as.is=T, quote="")

  data<-data[order(data[,1]),]

  data<-data[-1]

  if(ncol(data) != columns)
  {
    stop(paste("\nFound", ncol(data), "samples in", filename, "but expected", columns, "samples", sep=' ' ))

  }

  if(nrow(data) != rows)
  {
    stop(paste("\nFound", nrow(data), "features in", filename, "but expected", rows, "features", sep=' ' ))
  }

  return(data)
}
