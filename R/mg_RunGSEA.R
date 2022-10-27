mg_RunGSEA=function(mod=c('exp_group','exp_gene','rank')[1],exp_Path=NULL, sample_group_path=NULL, outFolder=NULL,gene=NULL, column=NULL,lower=50,upper=50, gmt_Path=c("KEGG",'GO_BP','GO_CC','GO_MF','reactome','HALLMARK','TF')[1],plot_svg=FALSE,top=10,min=5,max=5000,outLog=T){

  if(is.null(exp_Path)|is.null(mod)|is.null(outFolder)|is.null(gmt_Path)){
    return(NULL)
  }
  if(plot_svg){
    svg='true'
  }else{
    svg='false'
  }
  if(gmt_Path=='KEGG'){
    gmt_Path=paste0('source/c2.cp.kegg.v7.0.symbols.gmt')
  }
  else if(gmt_Path=='GO_BP'){
    gmt_Path=paste0('source/c5.bp.v7.0.symbols.gmt')
  }
  else if(gmt_Path=='GO_CC'){
    gmt_Path=paste0('source/c5.cc.v7.0.symbols.gmt')
  }
  else if(gmt_Path=='GO_MF'){
    gmt_Path=paste0('source/c5.mf.v7.0.symbols.gmt')
  }
  else if(gmt_Path=='reactome'){
    gmt_Path=paste0('source/c2.cp.reactome.v7.0.symbols.gmt')
  }
  else if(gmt_Path=='HALLMARK'){
    gmt_Path=paste0('source/h.all.v7.0.symbols.gmt')
  }
  else if(gmt_Path=='TF'){
    gmt_Path=paste0('source/c3.tft.v7.0.symbols.gmt')
  }
  if(file.exists(paste0(getwd(),'/',outFolder))){
    outFolder=paste0(getwd(),'/',outFolder)
  }else if(!file.exists(outFolder)){
    dir.create(outFolder)
    if(file.exists(paste0(getwd(),'/',outFolder))){
      outFolder=paste0(getwd(),'/',outFolder)
    }
  }

  if(file.exists(paste0(getwd(),'/',gmt_Path))){
    gmt_Path=paste0(getwd(),'/',gmt_Path)
  }
  if(file.exists(paste0(getwd(),'/',exp_Path))){
    exp_Path=paste0(getwd(),'/',exp_Path)
  }

  command=NULL
  if(mod=='exp_group'){
    if(!is.null(exp_Path)&!is.null(sample_group_path)&!is.null(outFolder)){
      if(file.exists(paste0(getwd(),'/',sample_group_path))){
        sample_group_path=paste0(getwd(),'/',sample_group_path)
      }
      command=paste0('jre/GSEA/MG_GSEA.jar exp_group '
                     ,exp_Path,' ',sample_group_path,' ',outFolder,' ',gmt_Path,' ',svg,' ',top,' ',min,' ',max)
    }
  }else if(mod=='exp_gene'){
    if(!is.null(exp_Path)&!is.null(gene)&!is.null(outFolder)){
      command=paste0('jre/GSEA/MG_GSEA.jar exp_gene '
                     ,exp_Path,' ',gene,' ',outFolder,' ',gmt_Path,' ',svg,' ',top,' ',min,' ',max,' ',lower,' ',upper)
    }
  }else if(mod=='rank'){
    if(!is.null(exp_Path)&!is.null(column)&!is.null(outFolder)){
      command=paste0(MG_Grobal_baseFolder,'/jre/GSEA/MG_GSEA.jar rank '
                     ,exp_Path,' ',column-1,' ',outFolder,' ',gmt_Path,' ',svg,' ',top,' ',min,' ',max)
    }
  }

  if(!is.null(command)){
    if(MG_Grobal_System=='win'){
      command=paste0('jre/bin/java -jar ',command)
    }else{
      command=paste0('java -jar ',command)
    }
    print(paste0('RunGSEA CMD:',command))
    logs=system(command, intern = !outLog,
                ignore.stdout = FALSE, ignore.stderr = FALSE,
                wait = TRUE, input = NULL, show.output.on.console = TRUE,
                minimized = FALSE, invisible = TRUE)
    if(outLog){
      if(logs==0){
        print('Run GSEA succ')
      }else{
        print('Run GSEA error')
      }
    }else{
      print('Runed GSEA')
      print(logs)
      logs=logs[grep('######/',logs)]
      if(length(logs)==1){
        logs=unlist(strsplit(logs[1],'/'))
        if(length(logs)>1){
          return(logs[2:length(logs)])
        }
      }
    }
  }
  return(NULL)
}
