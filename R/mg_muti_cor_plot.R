mg_muti_cor_plot=function(dat1,dat2=NULL,method=c('pearson','spearman','kendall')[1]){
  if(is.null(dat2)){
    cr=psych::corr.test(dat1,method = method)
  }else{
    cr=psych::corr.test(dat1,dat2,method = method)
  }
  return(cr)
}
