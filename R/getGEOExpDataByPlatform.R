getGEOExpDataByPlatform=function(gse_id='GSE63127',platform='GPL570',data_col=2,ann_col=NULL,m_index=2){#
  #if(is.null(sampleinfo)){
  #gse_id='GSE17705'
  if(data_col<2){
    data_col=2
  }
  sampleinfo=getGEOSampleData(gse_id)
  #head(sampleinfo)
  #}
  sup=substr(gse_id,1,nchar(gse_id)-3)
  if(nchar(sup)<3){
    sup='GSE'
  }
  b_f=paste0(MG_Grobal_DBPath,'/geo/series/',sup,'nnn/',gse_id,'/miniml/')
  b_anno=paste0(MG_Grobal_DBPath,'/geo/GPL_anno/')
  tmp=MG_Grobal_TmpPath
  flist.all=c()
  for(fl in dir(b_f)){
    if(length(grep('family.xml',fl))>0){
      flist=untar(paste0(b_f,'/',fl),list=TRUE)
      flist.all=c(flist.all,flist)
      print(paste0('untar data:',fl))
      untar(paste0(b_f,'/',fl), files = flist, list = FALSE, exdir = tmp)
    }
  }
  data_list=list()
  data_list_name=c()
  data_anno=list()
  #for(gpl in gpls){
  gpl=platform
  #print(paste0(tmp,'/',paste0(gpl,'-tbl-1.txt')))
  anno=readMatrix(paste0(tmp,'/',paste0(gpl,'-tbl-1.txt')),header = F,row=F)
  sampleinfo=sampleinfo[sampleinfo$Platform==gpl,]
  #sampleinfo$file
  #grep('GSM549660',flist.all)
  #length(flist.all)
  cmp=intersect(sampleinfo$file,flist.all)
  sampleinfo=sampleinfo[match(cmp,sampleinfo$file),]
  
  exps=flist.all[match(cmp,flist.all)]
  rows=unique(sampleinfo$rows)
  #data_anno=c(data_anno,list(anno))
      #gpl=gpls[1]
      #row=rows[1]
  #print(dim(anno))
  data_exp=cbind()
  #print(exps)
  for(f1 in exps){
    print(paste0('Press data for:',f1))
    dat=readMatrix(paste0(tmp,'/',f1),header = F,row=F)
    #print(head(dat))
    #print(data_col-)
    
    if(is.null(ncol(data_exp))){
      #print(dat[1:10,data_col])
      if(ncol(dat)<data_col){
        data_exp=cbind(data_exp,rep(NA,nrow(dat)))
      }else{
        data_exp=cbind(data_exp,dat[,data_col])
      }
      row.names(data_exp)=as.character(dat[,1])
    }else{
      if(ncol(dat)<data_col){
        data_exp=cbind(data_exp,rep(NA,nrow(dat)))
      }else{
        data_exp=cbind(data_exp,dat[match(row.names(data_exp),as.character(dat[,1])),data_col])
      }
    }
  }
  colnames(data_exp)=sampleinfo$Acc
  #print(dim(data_exp))
  print('data succ by extrat,converting')
  if(is.null(ann_col)){
    data_exp=exp_probe2symbol_v2(data_exp,GPL = gpl,method = c('mean','median','max','min')[m_index])
  }else if(ann_col>1){
    data_exp=exp_probe2symbol_v2(data_exp,anno = anno[,c(1,ann_col)],method = c('mean','median','max','min')[m_index])
  }
  #print(dim(data_exp))
  return(data_exp)    
}
