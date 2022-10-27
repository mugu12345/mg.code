merge_data_by_group=function(datExpr,anno=NULL,method=c('mean','median','max','min')[1],rm_muti=T){
  library(dplyr)

  anno=unique(anno[,1:2])
  #gp=table(anno[,1])
  anno=anno[which(!is.na(anno[,2])&!anno[,2]==''),]

  an.cmp=intersect(anno[,1],row.names(datExpr))
  if(rm_muti){
    if(ncol(datExpr)==1){
      test.data=cbind(datExpr[match(an.cmp,row.names(datExpr)),])
      row.names(test.data)=an.cmp
      colnames(test.data)=colnames(datExpr)
    }else{
      test.data=datExpr[match(an.cmp,row.names(datExpr)),]
    }
    test.data=crbind2DataFrame(test.data,full = T)
    #class(test.data[,1])
    #head(test.data)
    #sum(is.na(test.data[,1]))
    #head(datExpr)
    test.data$MG_SXR_Group=anno[match(an.cmp,anno[,1]),2]
  }else{
    anno=anno[anno[,1]%in%an.cmp,]
    if(ncol(datExpr)==1){
      test.data=crbind2DataFrame(cbind(datExpr[match(anno[,1],row.names(datExpr)),]),full = T)
      colnames(test.data)=colnames(datExpr)
      row.names(test.data)=anno[,1]
    }else{
      test.data=crbind2DataFrame(datExpr[match(anno[,1],row.names(datExpr)),],full = T)
    }
    test.data$MG_SXR_Group=anno[,2]
  }


  #which(test.data[,1]=='Inf'&test.data[,1]=='NaN'&test.data[,1]=='NA')

  vd.test.data=NULL
  if(method=='mean'){
    vd.test.data=test.data %>% dplyr::group_by(MG_SXR_Group) %>% dplyr::summarise_each(dplyr::funs(mean_rm_na),vals=c(colnames(test.data)[1:(ncol(test.data)-1)]))
  }else if(method=='median'){
    vd.test.data=test.data %>% dplyr::group_by(MG_SXR_Group) %>% dplyr::summarise_each(dplyr::funs(median_rm_na),vals=c(colnames(test.data)[1:(ncol(test.data)-1)]))
  }else if(method=='max'){
    vd.test.data=test.data %>% dplyr::group_by(MG_SXR_Group) %>% dplyr::summarise_each(dplyr::funs(max_rm_na),vals=c(colnames(test.data)[1:(ncol(test.data)-1)]))
  }else if(method=='min'){
    vd.test.data=test.data %>% dplyr::group_by(MG_SXR_Group) %>% dplyr::summarise_each(dplyr::funs(min_rm_na),vals=c(colnames(test.data)[1:(ncol(test.data)-1)]))
  }
  if(!is.null(vd.test.data)){
    vd.test.data=as.data.frame(vd.test.data)
    #print(head(vd.test.data))
    #print(row.names(vd.test.data))
    #print(sum(is.na(vd.test.data[,1])))
    #print(sum(vd.test.data[,1]==''))
    if(ncol(vd.test.data)==2){
      vd.test.data.1=cbind(vd.test.data[,2])
      row.names(vd.test.data.1)=as.character(vd.test.data[,1])
      vd.test.data=vd.test.data.1
      #head(vd.test.data)
    }else{
      row.names(vd.test.data)=as.character(vd.test.data[,1])
      vd.test.data=vd.test.data[,-1]
    }
    colnames(vd.test.data)=colnames(test.data)[1:(ncol(test.data)-1)]
  }
  return(vd.test.data)
  detach('package:dplyr')
}
