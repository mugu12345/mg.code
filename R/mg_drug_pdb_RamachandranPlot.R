
mg_drug_pdb_RamachandranPlot=function(pdb_file){
  #Ramachandran plot（拉氏图）是由G. N. Ramachandran等人[1]于1963年开发的，用来描述蛋白质结构中氨基酸残基二面角ψ和φ是否在合理区域的一种可视化方法。同时也可以反映出该蛋白质的构象是否合理。
  #pdb_file='Z:/users/zhurf/work/P334_20210114_UCEC/4-Drug_anaylsys/CLEC14A.pdb'
  #pdb_file='/pub1/data/mg_projects/users/zhurf/work/P334_20210114_UCEC/4-Drug_anaylsys/CLEC14A_XXXXX.pdb'
  if(file.exists(paste0(getwd(),'/',pdb_file))){
    pdb_file=paste0(getwd(),'/',pdb_file)
  }
  if(!file.exists(pdb_file)){
    return(c('pdb not found'))
  }
  command=paste0('/pub1/data/mg_projects/users/zhurf/work/P334_20210114_UCEC/4-Drug_anaylsys/CLEC14A_XXXXX.pdb A  1.5 /pub1/data/mg_projects/users/zhurf/work/P334_20210114_UCEC/4-Drug_anaylsys/')
  if(MG_Grobal_System=='win'){
    if(substr(pdb_file,start = nchar(pdb_file)-3,stop = nchar(pdb_file))!='.pdb'){
      return(c('input must be .pdb file!'))
    }
    command=paste0(MG_Grobal_baseFolder,'/source/PROCHECK/Procheck_NT/pro_v2.bat ',substr(pdb_file,start = 1,nchar(pdb_file)-4),' A 1.5 '
                   ,MG_Grobal_baseFolder,'/source/PROCHECK/Procheck_NT')
  }else{
    command=paste0(MG_Grobal_baseFolder,'/source/PROCHECK/procheck/pdbsum_procheck_v2.scr '
                   ,MG_Grobal_baseFolder,'/source/PROCHECK/procheck ',pdb_file,' A 1.5 ',dirname(pdb_file))
  }
  print(paste0('RunGSEA CMD:',command))
  logs=system(command, intern = T,
              ignore.stdout = FALSE, ignore.stderr = FALSE,
              wait = TRUE, input = NULL, show.output.on.console = TRUE,
              minimized = FALSE, invisible = TRUE)
  return(logs)
}
