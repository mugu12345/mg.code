getGEOPlatformInfo=function(GPL='GPL570'){
    b_f=paste0(MG_Grobal_DBPath,'/geo/GPLs/',GPL,'-tbl-1.txt.gz')
    anno=readMatrixByGZ(b_f,header = F)
    return(anno)
}