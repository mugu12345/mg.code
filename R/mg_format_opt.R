mg_format_opt=function(opt){
  for(i in names(opt)){
    if(length(intersect(grep('"$',opt[[i]]),grep('^"',opt[[i]])))>0){
      opt[[i]]=substr(opt[[i]],2,nchar(opt[[i]])-1)
    }
    if(i=='outfolder'){
      mg_file_del(paste0(opt$outfolder,'/run.log'))
    }
  }
  return(opt)
}
