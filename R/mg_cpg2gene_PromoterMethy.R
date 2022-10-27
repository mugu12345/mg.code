mg_cpg2gene_PromoterMethy=function(cpg_beta,method=c('mean','median','max','min')[1]){
  cpg2gene=mg_CpgAnnoPromoters(row.names(cpg_beta))
  return(exp_probe2symbol_v2(cpg_beta[row.names(cpg_beta)%in%cpg2gene[,1],],anno = unique(cpg2gene[,c(1,5)])
                             ,rm_muti = F,method = method))
}
