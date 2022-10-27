immu_Th1_IFNγScore=function(exp_data){
  #Th1/IFNγ gene signatures
  #计算每个患者的IFNγ评分，数据来源于PMC6548624,干扰素基因，获得性免疫相关，与免疫治疗相关
  genes=c('GBP4','GBP5','GBP1','STAT1','PTAFR','JAK2','JAK1','APBB2','PTPN1','CD38','OAS3','VCAM1','DGKI','CMAH','PIAS1','IL12RB2','CCL4','IRF3','HLA-DPA1','IFNG','CIITA','B2M','FCGR1A','GBP7','EGFL6','CTLA4','IFNGR1','CD44','HLA-DQA1','DPP4','SP100','IRF4','SGCB','FCGR1B','GBP6','OAS2','SUMO1','HLA-DQA2','DUSP5','ICAM1','IRF2','LTA','HLA-DPB1','LRRN3','SOCS3','HLA-DRB1','HLA-DRB5','IFNGR2','IRF1','APOD','CSF2','DOK5','MT2A','IRF9','NCAM1','CAMK2D','ATP9A','IRF7','OAS1','IRF8','GGT1','CD70','HLA-A','HLA-B','CAMK2B','ZBTB32','GBP2','HBEGF','IL22','CAMK2A','IRF6','SOCS1','BST2','HLA-C','OASL','PTPN6','HLA-F','HLA-G','IRF5','PML','SYNGR3','PTPN2','PRKCD','LRP8','BTG3')
  score=ssGSEAScore_by_genes(exp_data,genes)
  return(score[1,])
}
