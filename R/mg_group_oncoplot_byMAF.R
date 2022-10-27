mg_group_oncoplot_byMAF=function(maf=NULL,mafPath=NULL,group=NULL,genes=NULL,top=NULL,annotationColor='npg',isTCGA =T){#group为两列，第一列为样本名称，第二列为分类
  library('maftools')
  if(is.null(maf)){
    if(is.null(mafPath)){
      maf=NULL
    }else{
      maf = read.maf(maf = mafPath,isTCGA =isTCGA )
    }
  }
  if(!is.null(group)&!is.null(maf)){
    cl.dt=as.data.frame(maf@clinical.data)
    ul=unique(nchar(as.character(group[,1])))
    if(length(ul)>1){
      gp=group[match(as.character(cl.dt[,1]),group[,1]),2]
    }else{
      gp=group[match(substr(as.character(cl.dt[,1]),1,ul),group[,1]),2]
    }
    cl.dt$Cluster=as.character(gp)
    colnames(cl.dt)=c(colnames(cl.dt)[1],colnames(group)[2])
    cl.dt=cl.dt[which(!is.na(cl.dt[,2])),]
    cl.dt[,2]=as.character(cl.dt[,2])
    data.table::setDT(cl.dt)
    maf@clinical.data=cl.dt
    if(is.null(top)){
      if(!is.null(genes)){
        top=length(genes)
      }else{
        top=15
      }
    }
    gp=unique(group[,2])
    cl=mg_get_ggsci_colN(annotationColor,length(gp))
    names(cl)=gp
    acl=list(A=cl)
    names(acl)=colnames(group)[2]
    oncoplot(maf = maf,top = top,genes = genes
             ,clinicalFeatures = colnames(group)[2]
             ,sortByAnnotation = TRUE
             ,annotationColor = acl)
  }
  return(maf)
}
