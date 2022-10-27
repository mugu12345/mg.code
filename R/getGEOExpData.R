getGEOExpData=function(gse_id='GSE140082'){#
  #if(is.null(sampleinfo)){
  #gse_id='GSE103229'
  #gse_id='GSE19417'
  sampleinfo=getGEOSampleData(gse_id)
  #}
  #sampleinfo[is.na(sampleinfo$file),]
  
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
      untar(paste0(b_f,'/',fl), files = flist, list = FALSE, exdir = tmp)
    }
  }
  exps=flist.all[match(sampleinfo$file,flist.all)]
  
    gpls=unique(sampleinfo$Platform)
    rows=unique(sampleinfo$rows)
    if(length(rows)>1){
      rows=names(which(table(sampleinfo$rows)>1))
    }
    
    #sampleinfo$Data_col1
    #sampleinfo$Data_col2
    #sampleinfo$Data_col3
    #sampleinfo$Data_col4
    
    data_list=list()
    data_list_name=c()
    data_anno=list()
    for(gpl in gpls){
      #gpl=gpls[1]
      anno=readMatrix(paste0(tmp,'/',paste0(gpl,'-tbl-1.txt')),header = F,row=F)
      data_anno=c(data_anno,list(anno))
      for(row in rows){
      #gpl=gpls[1]
      #row=rows[2]
      s.inds=which(sampleinfo$Platform==gpl&sampleinfo$rows==row)
      #sampleinfo[4,]
      if(length(s.inds)>0&as.numeric(row)>0){
      list_all=rbind()
      #dat0=NULL
      #indx.col0=NULL
      
      list_all=list()
      list_all_group=rbind()
      for(f1 in exps[s.inds]){
        #f1='GSM858115-tbl-1.txt'
        #f1='GSM552354-tbl-1.txt'
        #f1=exps[s.inds][1]
        dat=readMatrix(paste0(tmp,'/',f1),header = F,row=F)
        dat[,1]=as.character(dat[,1])
        dat=dat[order(dat[,1]),]
        #dim(dat)
        #print(ncol(dat))
        #head(dat)
        indx.dat=cbind()
        indx.col=c()
        
        #dat0=dat[,1]
        for(i in 2:ncol(dat)){
          if(class(dat[,i])=='numeric'){
            indx.dat=cbind(indx.dat,dat[,i])
            indx.col=c(indx.col,i)
          }else{
            nuers=as.numeric(as.character(dat[,i]))
            if(sum(is.na(nuers))<0.8*length(nuers)){
              indx.dat=cbind(indx.dat,nuers)
              indx.col=c(indx.col,i)
            }else if(sum(is.na(nuers))<0.5*length(nuers)&length(unique(dat[is.na(nuers),i]))<10){
              indx.dat=cbind(indx.dat,nuers)
              indx.col=c(indx.col,i)
            }
          }
        }
        #indx.col0=indx.col
        #head(indx.dat)
        list_all=c(list_all,list(list(data=indx.dat,column=indx.col,col=ncol(indx.dat),index=f1,row=dat[,1])))
        dt1=paste0(dat[,1],collapse = ',')
        dt2=paste0(indx.col,collapse = ',')
        ky=openssl::md5(paste0(dt1,dt2))
        list_all_group=rbind(list_all_group,c(ky,ncol(indx.dat)))
        #list_all=rbind(list_all,indx.dat)
      }
      
      if(is.null(list_all)){
        print('Not Found data')
      }
      chort=0
      for(ky in unique(list_all_group[,1])){
        #ky='a2bf241f15d8ac461f781cab72c51434'
        chort=chort+1
        k.inds=which(list_all_group[,1]==ky)
        #j=1
        dt.all=cbind()
        dt.col=c()
        dt.row=NULL
        for(i in k.inds){
          dat=list_all[[i]]
          dt.col=c(dt.col,dat$index)
          dt.all=cbind(dt.all,dat$data)
          if(is.null(dt.row)){
            dt.row=dat$row
          }
        }
        all.inds=rep(1:list_all_group[k.inds[1],2],length(k.inds))
        cnames=sampleinfo$Acc[match(dt.col,sampleinfo$file)]
        for(cl in unique(all.inds)){
          data=dt.all[,which(all.inds==cl)]
          colnames(data)=cnames
          row.names(data)=dt.row
          data_list=c(data_list,list(data))
          if(length(unique(list_all_group[,1]))==1){
            chrt=''
          }else{
            chrt=paste0('_c',chort,'_')
          }
          data_list_name=c(data_list_name,paste0(gpl,'_',row,chrt,'_Data_col',cl))
        }
      }
      
      }
      }
    }
    names(data_list)=data_list_name
    names(data_anno)=gpls
    #if(length(data_list)==1)
  return(list(Exp=data_list,Sample=sampleinfo,Anno=data_anno))    
}
