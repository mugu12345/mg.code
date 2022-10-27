dotplot_batch=function(enrichmentORA,dbs=c('pathway_KEGG','pathway_Wikipathway','pathway_Reactome'
                                           ,'geneontology_Biological_Process','geneontology_Cellular_Component'
                                           ,'geneontology_Molecular_Function','disease_Disgenet','disease_OMIM'
                                           ,'drug_DrugBank','phenotype_Human_Phenotype_Ontology'),top=20,FDR=T
                       ,high_col='red',low_col='blue'){
  #library('ggpubr')
  library(ggplot2)
  #library(ggsci)
  #db='pathway_KEGG'
  plts=list()
  p.ct=0
  all.enrich=enrichmentORA
  dbs=intersect(dbs,enrichmentORA$DB)
  #windowsFonts(mgFont = windowsFont("Times New Roman"))
  for(db in dbs){
    tl=paste0('All ',db)
    pathway_data=all.enrich[all.enrich$DB==db,]
    if(nrow(pathway_data)>0){
      if(nrow(pathway_data)>top){
        pathway_data=pathway_data[order(pathway_data$FDR)[1:top],]
        tl=paste0('Top',top,' ',db)
      }
      desc=pathway_data$description
      ndesc=c()
      for(de in desc){
        if(nchar(de)>50&length(grep(' ',de))>0){
          de1=unlist(strsplit(de,' '))
          d2=paste0(de1[(ceiling(length(de1)/2)+1):length(de1)],collapse = ' ')
          if(nchar(d2)>50){
            d2=paste0(substr(d2,0,47),'...')
          }
          de2=paste0(paste0(de1[1:ceiling(length(de1)/2)],collapse = ' '),'\n'
                     ,d2)
          ndesc=c(ndesc,de2)
        }else{
          ndesc=c(ndesc,de)
        }
      }
      pathway_data$description=ndesc
      pathway_data$description <- factor(pathway_data$description
                                         ,levels=pathway_data$description[order(pathway_data$size,decreasing = T)], ordered=TRUE)
      pathway_data$pValue[pathway_data$pValue<1e-16]=1e-16
      pathway_data$FDR[pathway_data$FDR<1e-16]=1e-16

      bubble=ggplot(data = pathway_data, aes(x = enrichmentRatio, y = description))

      bubble=bubble+xlab('Enrichment Ratio')
      if(FDR){

        bubble=bubble+geom_point(aes(size = size,color = -log10(FDR))) +scale_color_gradient(low = low_col, high = high_col)
      }else{
        bubble=bubble+geom_point(aes(size = size,color = -log10(pValue))) +scale_color_gradient(low = low_col, high = high_col)
      }

      bubble=bubble+ggtitle(tl)
      bubble=bubble+ggsci::scale_fill_npg()+ggplot2::theme_bw()+theme(axis.title.y=element_blank()
                                                                      ,axis.text.y=element_text(family="Times",face="plain")
                                                                      ,axis.text.x=element_text(family="Times",face="plain")
                                                                      ,plot.title = element_text(hjust = 0.5,family="Times",face="plain")
                                                                      ,axis.title.x=element_text(family="Times",face="plain")
                                                                      ,legend.title = element_text(family="Times",face="plain")
                                                                      ,legend.text = element_text(family="Times",face="plain"))
      plts=c(plts,list(bubble))
      p.ct=p.ct+1
    }
  }
  if(p.ct>1){
    nc=2
    nr=ceiling(p.ct/2)
  }else{
    nc=p.ct
    nr=1
  }
  g1=ggpubr::ggarrange(plotlist=plts, ncol = nc, nrow = nr,labels = toupper(letters)[1:p.ct],align = "hv")
  #detach('package:ggpubr')
  #detach('package:ggplot2')
  #detach('package:ggsci')

  return(g1)
}
