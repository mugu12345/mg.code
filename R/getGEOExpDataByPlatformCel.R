getGEOExpDataByPlatformCel=function(gse_id,platform,m_index=2){#GSE143477
  #if(is.null(sampleinfo)){
  #gse_id='GSE344'
  #gse_id='GSE22769'
  #platform='GPL10647'
  #gse_id='GSE9819'
  #platform='GPL6244'
  
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
  rows=unique(sampleinfo$rows)
  flist.all=flist.all[grep('\\.CEL',flist.all)]
  filemap=rbind()
  mutimap=rbind()
  for(sm in sampleinfo$Acc){
    t.ind=grep(sm,flist.all)
    if(length(t.ind)==1){
      filemap=rbind(filemap,c(flist.all[t.ind[1]],sm))
    }else if(length(t.ind)>1){
      for(i in t.ind){
        mutimap=rbind(mutimap,c(flist.all[t.ind[i]],sm))
      }
    }
  }
  if(!is.null(mutimap)){
    mutimap=mutimap[which(!mutimap[,2]%in%filemap[,2]),]
    mutimap=mutimap[which(!mutimap[,1]%in%filemap[,1]),]
    filemap=rbind(filemap,mutimap)
  }
  sampleinfo=sampleinfo[match(filemap[,2],sampleinfo$Acc),]
  flist.all=filemap[,1]
  data_list=list()
  data_list_name=c()
  pf=platform
  #print(sampleinfo)
  ind1=which(sampleinfo$Platform==pf)
  #print(ind1)
  if(length(ind1)>0){
  fls=flist.all[ind1]
  #fls=paste0(sampleinfo$Acc,'.CEL.gz')
  dt.all=cel2exp(paste0(tmp,'/',fls))
  #print(dt.all)
  dt=dt.all$Exp
  plat=dt.all$platform
  if(plat!='pd.hugene.1.0.st.v1'&plat!='pd.huex.1.0.st.v2'&plat!='pd.hta.2.0'){
    plat=pf  
  }
  #dim(dt)
  colnames(dt)=sampleinfo$Acc[ind1]
  #print(head(dt))
  print(plat)
  dt=exp_probe2symbol_v2(dt,GPL=plat,method = c('mean','median','max','min')[m_index])
  return(dt)    
  }else{
    return(NULL)
  }
}
