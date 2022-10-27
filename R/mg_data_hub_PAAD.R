mg_data_hub_PAAD=function(dtype=c('mEXP','LNC')[1]){
  if(dtype=='mEXP'){
    fls=dir(paste0(MG_Grobal_baseFolder,'/data_hub/PAAD/'))
    fls=fls[grep('.gexp.dat.Rdata$',fls)]
    all_dat=list()
    for(fl in fls){
      load(file=paste0(MG_Grobal_baseFolder,'/data_hub/PAAD/',fl))
      print(paste0(fl,':',gexp.dat$INFO))
      all_dat=c(all_dat,list(gexp.dat))
    }
    names(all_dat)=gsub('.gexp.dat.Rdata$','',fls)
    return(all_dat)
  }
}
