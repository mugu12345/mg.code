
immu_AngiogenesisScore=function(exp_data){
  #血管生成相关的基因集，文献来源：PMC3743050
  angiogenesis=c(7010,1003,54567,51162,3791,54538,7450,7116,10085,5175,947,2022,11082,221395,6401,8842,7422,10266,8490,55109,3690,3685,3693,51129)
  angiogenesis.sym=c('TEK','CDH5','DLL4','EGFL7','KDR','ROBO4','VWF','TMSB4XP2','EDIL3','PECAM1','CD34','ENG','ESM1','ADGRF5','SELE','PROM1','VEGFA','RAMP2','RGS5','AGGF1','ITGB3','ITGAV','ITGB5','ANGPTL4')
  score=ssGSEAScore_by_genes(exp_data,angiogenesis.sym)
  return(score[1,])
}
