mg_out_clsFile=function(group_vactor,out_path){#name,group
  gp=cbind(as.character(group_vactor),as.numeric(as.factor(group_vactor)))
  ugp=unique(gp)
  ugp=ugp[order(ugp[,2]),]
  writeMatrix(c(paste0(c(nrow(gp),nrow(ugp)),collapse = ' '),
                paste0(c('#',ugp[,1]),collapse = ' '),
                paste0(gp[,2],collapse = ' ')),outpath = out_path,row=F,header = F)
}
