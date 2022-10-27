mg_get_tcga_cancer_subtype=function(tcga_code='BRCA'){
  return(crbind2DataFrame(TCGAbiolinks::TCGAquery_subtype(tcga_code)))
}