immu_signatures_score=function(exp_data=NULL,isTCGA=F){
  #文献写作：我们根据该小组免疫肿瘤专家的意见，进行了广泛的文献检索，
  #并收集了160种免疫表达签名的集合，这些签名使用了被认为可靠且全面的各种资源。
  #在这些特征中，有83个是在研究癌症免疫应答的背景下得出的，其余77个具有免疫力。
  #已知与肿瘤组织免疫活性相关的83个特征包括从早期研究中收集的68个基因组（Wolf等，2014年），
  #来自所有TCGA基因表达数据集（免疫元基因吸引子）的计算分析得出的9个共表达特征，
  #（Cheng等，2013a， Cheng等人，2013b），
  #代表免疫结构的功能方向（或免疫排斥的ICR）的3个标记（Bedognetti等人，2016年， Galon等人，2013年
  #， Hendrickx等人，2017年），以及最近一项研究的3个特征，
  #这些特征表征了透明细胞肾细胞癌的免疫微环境（Senbabaoglu等人，2016年）。
  #77个更普通的签名包含45个签名的分数，分别代表来自两个来源的单个细胞类型（来自（Gentles等人，2015年）和25个来自（Bindea等人，2013年））
  #和32个分数，其中包括从ImmuneSigDB（Godec等人，2016年， Subramanian等，2005）（MSigDB的C7集合，博德学院）
  #。这些模式被确定为1888年免疫C7人类基因集的前32个主要组成部分，并被用作难以理解的大型复杂模型。来自的基因集Bindea等。（2013年）
  #， Senbabaoglu等。（2016年），并使用单样本基因集富集（ssGSEA）分析对MSigDB C7集合进行评分
  #（芭比等人，2009年），如在GSVA R软件包（Hänzelmann等，2013）。所有其他签名均使用相关引用中的方法评分。
  baseFolder='source'
  imm.genes=readMatrix(paste0(baseFolder,'/160_immu_signature_pmid_31433971_v2.txt'),row=F)
  table(unique(imm.genes[,c(1,3)])[,2])
  all.list=list()
  for(s in unique(imm.genes[,1])){
    inds=which(imm.genes[,1]==s)
    gs=GSEABase::GeneSet(setName=s, setIdentifier=paste0(s,"101"),geneIds=imm.genes[inds,2],GSEABase::SymbolIdentifier())
    all.list <- c(all.list, list(gs))
  }
  gsc <- GSEABase::GeneSetCollection(all.list)
  fl <- tempfile()
  GSEABase::toGmt(gsc, fl)
  c2immue=GSEABase::getGmt(fl)
  ssGSEA.immue <- GSVA::gsva(as.matrix(exp_data), c2immue,method='ssgsea',
                             min.sz=1, max.sz=5000, verbose=TRUE)
  ssGSEA.immue=t(ssGSEA.immue)
  if(isTCGA){
    rnames=gsub('\\.','-',row.names(ssGSEA.immue))
    row.names(ssGSEA.immue)=rnames
  }
  return(ssGSEA.immue)
  #all.gs=cbind()
  #gnames=c()
  #for(g in unique(imm.genes[,1])){
  #  gs=imm.genes[which(imm.genes[,1]==g),2]
  #  gs.score=ssGSEAScore_by_genes(exp_data,gs)
  #  all.gs=cbind(all.gs,gs.score[1,])
  #  gnames=c(gnames,g)
  #}
  #all.gs=crbind2DataFrame(all.gs)
  #colnames(all.gs)=gnames
  #row.names(all.gs)=colnames(exp_data)
  #return(all.gs)

}
