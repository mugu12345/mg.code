getGEOExpDataByCel=function(gse_id){#GSE143477
  #if(is.null(sampleinfo)){
  #gse_id='GSE344'
  sampleinfo=getGEOSampleData(gse_id)
  #}
  sup=substr(gse_id,1,nchar(gse_id)-3)
  if(nchar(sup)<3){
    sup='GSE'
  }
  b_f=paste0(MG_Grobal_DBPath,'/geo/series/',sup,'nnn/',gse_id,'/suppl/')
  b_anno=paste0(MG_Grobal_DBPath,'/geo/GPL_anno/')
  tmp=MG_Grobal_TmpPath
  flist.all=c()
  for(fl in dir(b_f)){
    #fl=dir(b_f)[2]
    if(length(grep('_RAW',fl))>0){
      flist=untar(paste0(b_f,'/',fl),list=TRUE)
      flist.all=c(flist.all,flist)
      untar(paste0(b_f,'/',fl), files = flist, list = FALSE, exdir = tmp)
    }
  }
  gpls=unique(sampleinfo$Platform)
  rows=unique(sampleinfo$rows)
  #flist.all=flist.all[grep('CEL',toupper(flist.all))]
  flist.all=flist.all[grep('GSM',toupper(flist.all))]
  #flist.all=flist.all[grep('.GZ',toupper(flist.all))]
  flist.all=flist.all[c(grep('CEL.GZ$',toupper(flist.all)),grep('CEL$',toupper(flist.all)))]
  gsms=unlist(lapply(toupper(flist.all), function(x){
    regs=regexpr('GSM[0-9]+',x)
    st=regs[1]
    lt=attr(regs,'match.length')
    return(substr(x,st,st+lt-1))
  }))
  
  sampleinfo=sampleinfo[match(gsms,sampleinfo$Acc),]
  data_list=list()
  data_list_name=c()
  for(pf in unique(sampleinfo$Platform)){
    ind1=which(sampleinfo$Platform==pf)
    fls=flist.all[ind1]
    dt.all=cel2exp(paste0(tmp,'/',fls))
    dt=dt.all$Exp
    plat=dt.all$platform
    if(plat!='pd.hugene.1.0.st.v1'&plat!='pd.huex.1.0.st.v2'&plat!='pd.hta.2.0'){
      plat=pf  
    }
    colnames(dt)=sampleinfo$Acc[ind1]
    data_list=c(data_list,list(dt))
    data_list_name=c(data_list_name,paste0(plat,'_Data'))
  } 
  names(data_list)=data_list_name
  return(list(Exp=data_list,Sample=sampleinfo))    
}
