mg_CpgGenome38TSSAnno=function(cpg_locs_hg38){
  library(TxDb.Hsapiens.UCSC.hg38.knownGene)
  library(ChIPpeakAnno)
  TxDb=TxDb.Hsapiens.UCSC.hg38.knownGene
  gr <- toGRanges(as.data.frame(cpg_locs_hg38), format="BED", header=FALSE)
  transcripts <- unique(transcripts(TxDb, columns = NULL))

  TSS.up100=unique(flank(transcripts, width = 100, start = T,use.names = FALSE))
  TSS.up200=unique(flank(TSS.up100, width = 100, start = T,use.names = FALSE))
  TSS.up500=unique(flank(TSS.up200, width = 300, start = T,use.names = FALSE))
  TSS.up1000=unique(flank(TSS.up500, width = 500, start = T,use.names = FALSE))
  TSS.up1500=unique(flank(TSS.up1000, width = 500, start = T,use.names = FALSE))
  TSS.up2000=unique(flank(TSS.up1500, width = 500, start = T,use.names = FALSE))

  TSS.down100=unique(flank(TSS.up100, width = 100, start = F,use.names = FALSE))
  TSS.down200=unique(flank(TSS.down100, width = 100, start = F,use.names = FALSE))
  TSS.down500=unique(flank(TSS.down200, width = 300, start = F,use.names = FALSE))
  TSS.down1000=unique(flank(TSS.down500, width = 500, start = F,use.names = FALSE))
  TSS.down1500=unique(flank(TSS.down1000, width = 500, start = F,use.names = FALSE))
  TSS.down2000=unique(flank(TSS.down1500, width = 500, start = F,use.names = FALSE))

  annotation <- list(TSS.up2000,TSS.up1500,TSS.up1000,TSS.up500,TSS.up200, TSS.up100,
                     TSS.down100, TSS.down200, TSS.down500, TSS.down1000, TSS.down1500, TSS.down2000)
  annotation <- lapply(annotation, function(.anno) {
    mcols(.anno) <- NULL
    .anno
  })
  precedence=c("-2000", '-1500','-1000','-500','-200',"-100",'100','200','500','1000','1500','2000')
  names(annotation)[1:12] <- precedence
  count=mg_annotattion_genome_count(gr,annotation,precedence=precedence,TxDb=TxDb)
  percentage=count$percentage
  return(percentage[-13])
}
