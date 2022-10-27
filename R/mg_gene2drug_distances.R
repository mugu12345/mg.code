mg_gene2drug_distances=function(disease_genes,net_score=400){#方法学来源DOI:10.1093/bioinformatics/btaa563
  drug2gene<-mg_gene2drug()
  drug2gene=drug2gene[,c(1,2,6)]
  drug2gene=crbind2DataFrame(drug2gene)
  drug2gene=unique(drug2gene)
  colnames(drug2gene)=c('drugid','drugname','gene')

  gene_in_drug=unique(drug2gene[,3])
  gene_in_disease=disease_genes

  ppi.nodes=unique(c(gene_in_disease,gene_in_drug))
  ppi.all.nodes=mg_network_stringdb_extend_ppi(ppi.nodes,cut_score = net_score)

  library(igraph)

  PPIG=graph_from_data_frame(ppi.all.nodes,directed = FALSE)###network graph
  shortest_matrix=igraph::distances(PPIG,v=V(PPIG),to=V(PPIG))##所有最短路径
  shortest_path_dis=shortest_matrix[row.names(shortest_matrix)%in%gene_in_disease,]
  shortest_path_dis=na.omit(shortest_path_dis)
  degree_in_dis=igraph::degree(PPIG,v=V(PPIG),mode='all')####degree of disease relative genes
  print(paste0('input in netword gene count:',length(intersect(names(degree_in_dis),disease_genes))))

  tmp_cal_drug_distance=function(genenames){
    cmpg=intersect(colnames(shortest_path_dis),genenames)
    if(length(cmpg)==1){
      spd=t(t(shortest_path_dis[,match(cmpg,colnames(shortest_path_dis))]))
    }else{
      spd=shortest_path_dis[,match(cmpg,colnames(shortest_path_dis))]
    }
    spd=rbind(degree_in_dis[match(cmpg,names(degree_in_dis))],spd)

    distance_dgs=apply(spd, 2,function(x){
      distance_dg=min(x[-1])
      if(distance_dg==0){
        distance_dg=-log(x[1]+1)
      }
      return(distance_dg)
    })
    return(sum(distance_dgs)/ncol(spd))
  }
  ndd=drug2gene %>% dplyr::group_by(drugid) %>% dplyr::summarise_each(dplyr::funs(tmp_cal_drug_distance),vals=c('gene'))
  ndd=crbind2DataFrame(ndd)
  colnames(ndd)=c('drug_id','distances')
  print('cal drug distances succ')
  dtab=table(drug2gene[,1])
  ####random#####
  nodes=get.vertex.attribute(PPIG)
  nodes_t=nodes$name
  fname=digest::digest(paste0(sort(nodes_t),collapse = ','),algo = 'md5')
  fname=paste0(MG_Grobal_baseFolder,'/source/cache_file/DrugNet_',fname,'.fst')
  if(file.exists(fname)){
    rand.diss=readFST(fname,data.frame = T)
  }else{
    dtab.rand=t(apply(cbind(unique(dtab)),1, function(x){
      print(x)
      drug_degree=x
      round_dppi=rbind()
      for (j in 1:1000) {
        round_dppi=rbind(round_dppi,
                         cbind(drugid=rep(paste0('G',j),drug_degree)
                               ,gene=nodes_t[sample(length(nodes_t)
                                                    ,drug_degree,replace = F)]))

      }
      round_dppi.ndd=crbind2DataFrame(round_dppi) %>% dplyr::group_by(drugid) %>% dplyr::summarise_each(dplyr::funs(tmp_cal_drug_distance),vals=c('gene'))
      round_dppi.ndd=crbind2DataFrame(round_dppi.ndd)
      return(c(x,mean(round_dppi.ndd[,2]),sd(round_dppi.ndd[,2])))
    }))
    rand.diss=rbind()
    for(i in 1:nrow(dtab.rand)){
      dg=names(dtab)[which(dtab==dtab.rand[i,1])]
      rand.diss=rbind(rand.diss,cbind(dg,random_dis=rnorm(length(dg),mean = dtab.rand[i,2],sd=dtab.rand[i,3]
      ), Mean=dtab.rand[i,2],SD=dtab.rand[i,3]))
    }
    rand.diss=crbind2DataFrame(rand.diss)
    writeFST(rand.diss,outpath = fname)
  }

  ndd=cbind(ndd,rand.diss[match(ndd[,1],rand.diss[,1]),2:4])
  ndd$pvalue=pnorm(ndd[,2],ndd[,4],ndd[,5])
  ndd$FDR=p.adjust(ndd$pvalue)
  ndd$global_pvalue=pnorm(ndd[,2],mean(ndd[,3]),sd(ndd[,3]))
  ndd$global_FDR=p.adjust(ndd$global_pvalue)

  denplotdata=data.frame('Reference'=ndd[,3],'Drug'=ndd[,2])
  denplotdata=t(denplotdata)
  denplotdata=reshape2::melt(denplotdata)
  colnames(denplotdata)=c('Type','n','value')
  library(ggplot2)
  g1=ggplot(denplotdata,aes(value,fill=Type, color=Type)) +
    xlab("Distance") +
    geom_density(alpha = 0.6) +
    geom_rug() + theme_bw()
  ############cal dis 2 drug#############
  fname=digest::digest(paste0('net_score:',net_score),algo = 'md5')
  fname=paste0(MG_Grobal_baseFolder,'/source/cache_file/Drug2GeneDis_',fname,'.RData')
  if(file.exists(fname)){
    load(file = fname)
  }else{
    load(file=paste0(MG_Grobal_baseFolder,'/source/ppi/string.gene.link.RData'))
    string.gene.link=string.gene.link[string.gene.link[,3]>=net_score,]
    drug2gene.tmp=drug2gene[,c(1,3,2)]
    colnames(drug2gene.tmp)=colnames(string.gene.link)
    ppi.ex=rbind(string.gene.link,drug2gene.tmp)
    PPIG1=graph_from_data_frame(ppi.ex,directed = FALSE)###network graph
    all.gs=unique(c(ppi.all.nodes[,1],ppi.all.nodes[,2],drug2gene.tmp[,2]))
    PPIG1.shortest=igraph::distances(PPIG1,v=all.gs,to=unique(drug2gene.tmp[,1]))##所有最短路径
    PPIG1.shortest.drug=PPIG1.shortest[which(!row.names(PPIG1.shortest)%in%unique(drug2gene[,1]))
                                       ,colnames(PPIG1.shortest)%in%unique(drug2gene[,1])]
    save(PPIG1.shortest.drug,file = fname)
  }
  drug2gene=PPIG1.shortest.drug[row.names(PPIG1.shortest.drug)%in%disease_genes,]
  return(list(Data=ndd,Durg2Gene=drug2gene,plot=g1))
}
