mg_RunGSEA_exp=function(exp_data=NULL, sample_group=NULL
                        , outFolder=NULL, gmt_Path=c("KEGG",'GO_BP','GO_CC','GO_MF','reactome','HALLMARK','TF')[1]
                        ,plot_svg=FALSE,top=10,min=5,max=5000,outLog=T){
  if(is.null(exp_data)|is.null(sample_group)|is.null(outFolder)){
    print('data not found!')
    return(0)
  }
  if(file.exists(paste0(getwd(),'/',outFolder))){
    outFolder=paste0(getwd(),'/',outFolder)
  }else if(!file.exists(outFolder)){
    dir.create(outFolder)
    if(file.exists(paste0(getwd(),'/',outFolder))){
      outFolder=paste0(getwd(),'/',outFolder)
    }
  }
  writeMatrix(exp_data,outpath = paste0(outFolder,'/exp.data.txt'))
  writeMatrix(data.frame(Sample=colnames(exp_data),Group=sample_group)
              ,outpath = paste0(outFolder,'/exp.data.sample.txt'),row=F)

  return(mg_RunGSEA(mod='exp_group',exp_Path=paste0(outFolder,'/exp.data.txt'), sample_group_path=paste0(outFolder,'/exp.data.sample.txt')
                    , outFolder=outFolder, gmt_Path=gmt_Path
                    ,plot_svg=plot_svg,top=top,min=min,max=max,outLog=outLog))

}
