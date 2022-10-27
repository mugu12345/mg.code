mg_CpgGenome38Anno=function(cpg_locs_hg38){
  library(TxDb.Hsapiens.UCSC.hg38.knownGene)
  library(ChIPpeakAnno)
  TxDb=TxDb.Hsapiens.UCSC.hg38.knownGene
  gr <- toGRanges(as.data.frame(cpg_locs_hg38), format="BED", header=FALSE)
  fiveUTRs <- unique(unlist(fiveUTRsByTranscript(TxDb)))
  promoters <- unique(promoters(TxDb, upstream = 2000, downstream = 500))
  exons=exons(TxDb,columns=c('exon_rank'))
  logal=unlist(lapply(exons$exon_rank,function(x){return((unlist(x)[1]==1))}))
  es1t=exons[which(logal)]
  otherExon=exons[which(!logal)]
  introns <- unique(unlist(intronsByTranscript(TxDb)))
  threeUTRs <- unique(unlist(threeUTRsByTranscript(TxDb)))
  annotation <- list(fiveUTRs, promoters, es1t, otherExon,
                     threeUTRs,introns)
  annotation <- lapply(annotation, function(.anno) {
    mcols(.anno) <- NULL
    .anno
  })
  precedence=c("fiveUTRs", 'Promoters','1stExon','otherExon','threeUTRs',"Introns")
  names(annotation)[1:6] <- precedence
  count=mg_annotattion_genome_count(gr,annotation,precedence=precedence,TxDb=TxDb)
  percentage=count$percentage
  names(percentage)=c(precedence,'Intergenic')
  return(percentage[c(2,1,3,4,6,5,7)])
}
