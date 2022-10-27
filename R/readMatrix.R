readMatrix=function(inpath,row=T,header=T){
  #inpath='/pub1/data/tmp/Rtmp/d41d8cd98f00b204e9800998ecf8427e'
  if (grepl("gz$", inpath)){
    return(readMatrixByGZ(inpath,row,header))
  }
  if(MG_Grobal_System=='linux'&file.size(inpath)>0){
    #inpath='/pub1/data/user_data/15927487963/uploads/27.txt'
    command=paste0('/usr/bin/file ',inpath)
    logs=system(command, intern = T,
                ignore.stdout = FALSE, ignore.stderr = FALSE,
                wait = TRUE, input = NULL, show.output.on.console = TRUE,
                minimized = FALSE, invisible = TRUE)
    tx=toupper(gsub(': ','',gsub(inpath,'',logs)))
    if(length(grep(' TEXT',tx))==0&&length(grep(' SPEC',tx))>0){
      print('read data not is Text')
      return(NULL)
    }
  }
  dat=data.table::fread(inpath, sep = "\t",header = header,stringsAsFactors = F,check.names = F,na.strings="NA")
  if(nrow(dat)==0){
    dat=as.data.frame(dat,stringsAsFactors=F)
    return(dat)
  }
  if(row){
    #dat=read.csv(inpath,sep = '\t',stringsAsFactors = F,header = header,row.names = 1,check.names = F)
    #rownames=as.character(dat[,1])
    #print(rownames)
    dat=as.data.frame(dat,stringsAsFactors=F)
    row.names(dat)=dat[,1]
    if(ncol(dat)>2){
      dat=dat[,-1]
    }else if(ncol(dat)>1){
      rns=row.names(dat)
      cls=colnames(dat)
      dat=data.frame(dat[,2],stringsAsFactors = F)
      row.names(dat)=rns
      colnames(dat)=cls[2]
      #dat=as.data.frame()
    }
  }else{
    #dat=read.csv(inpath,sep = '\t',stringsAsFactors = F,check.names = F,header = header)
    dat=as.data.frame(dat,stringsAsFactors=F)
    #print(head(dat))
  }
  return(dat)
}
