getGEOSampleData=function(acc='GSE63127'){
  #acc='GSE8009'
  r <- httr::GET(paste0(MG_Grobal_baseURL,"/pubmed/getGeoSampleInfo?accession=",acc),add_headers=c('If-Match:*'))
  print(paste0(MG_Grobal_baseURL,"/pubmed/getGeoSampleInfo?accession=",acc))
  status=httr::http_status(r)
  print(paste0('statrus:',status))
  #print(httr)
  text=httr::content(r,'text',encoding = 'utf-8')
  #print(text)
  print('get texted,parse jsoning')
  text=jsonlite::fromJSON(text)
  print('parse jsoned')
  if(text$status!=200){
    return(NULL)
  }else{
    if(is.null(text$res$gsmInfomationList)){
      return(NA)
    }else{
      dt=text$res$gsmInfomationList
      orgm=NULL
      if(class(dt$organismJson)=='list'){
        orgm=as.character(dt$organismJson)
      }
      source=NULL
      if(class(dt$sourceJson)=='list'){
        source=as.character(dt$sourceJson)
      }
      
      ds=cbind()
      if(class(dt$dataSummaryJson)=='data.frame'){
        if(ncol(dt$dataSummaryJson)>0){
          nms=c()
          for(i in 1:ncol(dt$dataSummaryJson)){
            if(class(dt$dataSummaryJson[1,i])=='list'){
              tbs=rbind()
              for(j in 1:nrow(dt$dataSummaryJson)){
                #dim(dt$dataSummaryJson)
                #dt$dataSummaryJson[440,3][[1]]
                #head(dt$dataSummaryJson[,])
                if(is.null(dt$dataSummaryJson[j,i][[1]])){
                  tbs=rbind(tbs,rep('',ncol(tbs)))
                }else{
                  tbs=rbind(tbs,apply(dt$dataSummaryJson[j,i][[1]],1,function(x){
                    return(paste0(x,collapse = ','))
                  }))
                }
              }
              ds=cbind(ds,tbs)
              nms=c(nms,paste0('Data_col',1:ncol(tbs)))
            }else{
              st=as.character(dt$dataSummaryJson[,i])
              ds=cbind(ds,st)
              nms=c(nms,colnames(dt$dataSummaryJson)[i])
            }
          }
          colnames(ds)=nms    
        }
      }
      #dim(dt$characteristicsJson)
      charac=cbind()
      if(class(dt$characteristicsJson)=='data.frame'){
        for(i in 1:ncol(dt$characteristicsJson)){
          st=as.character(dt$characteristicsJson[,i])
          charac=cbind(charac,st)
        }
        colnames(charac)=colnames(dt$characteristicsJson)
      }
      dat=data.frame(Acc=dt$accession,
                Desc=gsub('\t',',',gsub('\n',',',dt$description)),
                Title=gsub('\t',',',gsub('\n',',',dt$title)),
                Type=dt$sampleType,
                Platform=dt$platformRef,
                Organism=orgm,Source=gsub('\t',',',gsub('\n',',',source)),stringsAsFactors = F)
      #head(dat)
      if(!is.null(ds)){
        dat=cbind(crbind2DataFrame(dat),ds)
      }
      if(!is.null(charac)){     
        dat=cbind(crbind2DataFrame(dat),charac)
      }
      dat=cbind(crbind2DataFrame(dat),DataProcessing=gsub('\t',',',gsub('\n',',',dt$dataProcessing)))
      return(crbind2DataFrame(dat))
    }
  }
}