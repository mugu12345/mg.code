immu_batch=function(exp,isTCGA=T,platform='illumina'){
  if(isTCGA){
    load(paste0(MG_Grobal_baseFolder,'/source/TCGA_immu.Rdata'))
    immu=TCGA_immu$Immu[match(colnames(exp),row.names(TCGA_immu$Immu)),]
    return(list(Immu=immu,Group=TCGA_immu$Group))
  }
  print('start in MCP:')
  a1=immu_MCPcounter(exp,isTCGA)
  print('start in EST:')
  a2=immu_estimate(exp,platform,isTCGA)
  print('start in ssGSEA:')
  a3=immu_ssgsea(exp)
  print('start in Timmer:')
  if(isTCGA){
    a4=immu_timer(exp)
  }else{
    a4=NULL
  }
  print('start in Meta:')
  a5=immu_meta(exp,isTCGA)
  print('start in ICG:')
  a6=immu_ICGs(exp)
  immu=list(MCPcounter=a1,estimate=a2,ssgsea=a3,timer=a4,meta=a5,icg=a6)
  exp.immu=cbind(immu$MCPcounter[match(colnames(exp),row.names(immu$MCPcounter)),],
                immu$estimate[match(colnames(exp),row.names(immu$estimate)),],
                immu$ssgsea[match(colnames(exp),row.names(immu$ssgsea)),],
                immu$timer[match(colnames(exp),row.names(immu$timer)),],
                immu$meta[match(colnames(exp),row.names(immu$meta)),],
                immu$icg[match(colnames(exp),row.names(immu$icg)),])
  group=c(rep('MCP',ncol(immu$MCPcounter))
          ,rep('EST',ncol(immu$estimate))
          ,rep('ssGSEA',ncol(immu$ssgsea))
          ,rep('Timer',ncol(immu$timer))
          ,rep('Meta',ncol(immu$meta))
          ,rep('ICG',ncol(immu$icg))
  )
  return(list(Immu=exp.immu,Group=group))
}