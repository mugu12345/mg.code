mg_get_tcga_sample_subtype=function(tcga_samples=c('TCGA-60-2721')){
  tcga_samples=substr(tcga_samples,1,12)
  s.subtypes=TCGAbiolinks::TCGA_MolecularSubtype(tcga_samples)
  return(s.subtypes$subtypes)
}