immu_CIBERSORT=function(exp_file=NULL,exp_data=NULL){

  if(is.null(exp_file)){
    if(is.null(exp_data)){
      print('data is null!')
      return(NULL)
    }else{
      exp_file=paste0(MG_Grobal_TmpPath,'/CIBERSORT_',digest::digest(as.character(tempfile())),'.txt')
      writeMatrix(exp_data,outpath = exp_file)
    }
  }
  if(file.exists(paste0(getwd(),'/',exp_file))){
    exp_file=paste0(getwd(),'/',exp_file)
  }
  #out_file=paste0(MG_Grobal_TmpPath,'/CIBERSORT_',digest::digest(as.character(tempfile())),'.txt')
  command=paste0(MG_Grobal_baseFolder,'/jre/CIBERSORT/CIBERSORT.jar',' -B ',MG_Grobal_baseFolder,'/jre/CIBERSORT/LM22.txt -M ',exp_file)

  if(!is.null(command)){
    if(MG_Grobal_System=='win'){
      command=paste0(MG_Grobal_baseFolder,'/jre/bin/java -jar ',command)
    }else{
      command=paste0('java -jar ',command)
    }
    print(paste0('RunCIBERSORT CMD:',command))
    Rserve::Rserve(args="--no-save")
    #command='Z:/projects/codes/jre/bin/java -jar Z:/projects/codes/jre/CIBERSORT/CIBERSORT.jar -B Z:/projects/codes/jre/CIBERSORT/LM22.txt -M Z:/global_tmp_data/ff0fe8f80e6420516e9e5d984f8bb0a9/CIBERSORT_f10b0167b9eb0111a422a4625270ab95.txt'
    #command='Z:/projects/codes/jre/bin/java -jar Z:/projects/codes/jre/CIBERSORT/CIBERSORT.jar -B Z:/projects/codes/jre/CIBERSORT/LM22.txt -M Z:/global_tmp_data/ff0fe8f80e6420516e9e5d984f8bb0a9/CIBERSORT_89d10e079add08b51889fb156e24550e.txt'
    logs=system(command, intern = T,
                ignore.stdout = FALSE, ignore.stderr = FALSE,
                wait = TRUE, input = NULL, show.output.on.console = TRUE,
                minimized = FALSE, invisible = TRUE)
    if(length(logs)>6){
      print(logs[1:6])
      logs=logs[7:length(logs)]
      if(length(grep('Exception in thread',logs[1]))>0){
        print(logs)
      }else{
        tme.data=rbind()
        for(l in 1:length(logs)){
          #print(logs[l])
          tme.data=rbind(tme.data,unlist(strsplit(logs[l],'\t')))
        }
        colnames(tme.data)=tme.data[1,]
        tme.data=tme.data[-1,]
        tme.data=crbind2DataFrame(tme.data)
        tme.data=tme.data[order(tme.data[,1]),]
        tme.data=tme.data[,-1]
        exp_data=readMatrix(exp_file)
        row.names(tme.data)=colnames(exp_data)
        return(tme.data)
      }
    }
    #if(logs==0){
    #out_file='Z:/global_tmp_data/ff0fe8f80e6420516e9e5d984f8bb0a9/CIBERSORT_a4110303330925f6480b1d3d94b7a100.txt'
    #  tme.data=read.csv(out_file,skip = 6,sep='\t')
    #dim(tme.data)
    #  exp_data=readMatrix(exp_file)
    # tme.data=tme.data[order(as.numeric(tme.data[,1])),]
    #  tme.data=tme.data[,-1]
    #  row.names(tme.data)=colnames(exp_data)
    #  return(tme.data)
    #}
  }
  return(NULL)
}
