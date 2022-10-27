getTCGAMethyCpGByCode=function(code='COAD'){
  k450=NULL
  k27=NULL
  if(file.exists(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/Methy/',code,'_450k'))){
    k450=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/Methy/',code,'_450k'))
    nact=apply(k450, 1, function(x){return(sum(is.na(x)|x==0))})
    k450=k450[which(nact<0.5*ncol(k450)),]
    k450=impute::impute.knn(as.matrix(k450),rng.seed = 1234)$data
  }
  if(file.exists(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/Methy/',code,'_27k'))){
    k27=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/Methy/',code,'_27k'))
    nact=apply(k27, 1, function(x){return(sum(is.na(x)|x==0))})
    k27=k27[which(nact<0.5*ncol(k27)),]
    k27=impute::impute.knn(as.matrix(k27),rng.seed = 1234)$data
  }
  mg=NULL
  if(!is.null(k450)&!is.null(k27)){
    nsm=setdiff(colnames(k27),colnames(k450))
    if(length(nsm)>1){
      nk27=k27[,match(nsm,colnames(k27))]
      cpgs=intersect(row.names(nk27),row.names(k450))
      nk27=nk27[match(cpgs,row.names(nk27)),]
      k450=k450[match(cpgs,row.names(k450)),]
      nkdt=cbind(k450,nk27)
      nkdt=limma::removeBatchEffect(nkdt,batch = c(rep(1,ncol(k450)),rep(2,ncol(nk27))))
      mg=nkdt
    }
  }
  return(list(M27k=k27,M450k=k450,MMerge=mg))
}