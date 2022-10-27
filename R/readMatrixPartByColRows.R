readMatrixPartByColRows <- function(inpath, rows = 1, columns = -1, sep = "\t", comment.char = "#"){
  #rows = -1时, 读取所有行,comment.char跳过注释行，columns = -1读取所有列
  dfl <- list()
  if (grepl("gz$", inpath)){
    con <- gzfile(inpath, open = "rb")
  } else{
    con <- file(inpath, open = "r")
  }
  i <- 0
  all_dt=rbind()
  mx_col=1
  repeat{
    rec <- readLines(con, 1)
    if (length(rec) == 0) break
    i <- i + 1
    if (i > max(rows) & rows != -1) break
    if (grepl(comment.char, rec )) next
    if ( ! i %in% rows & rows != -1) next
    items <- strsplit(rec, split = sep, fixed = TRUE)[[1]]
    if ( columns == -1){
      select_cols <- items
    } else{
      if(mx_col<length(items)){
        mx_col=length(items)
      }
      select_cols <- items[columns]
    }
    #print(select_cols)
    all_dt=rbind(all_dt,select_cols)
  }
  close(con)
  if ( columns != -1){
    all_dt=all_dt[,which(columns%in%c(1:mx_col))]
  }
  return(crbind2DataFrame(all_dt))
}
