mg_annotattion_genome_count=function (peaks.RD, genome_region,precedence = NULL, TxDb = TxDb.Hsapiens.UCSC.hg38.knownGene)
{
  library(ChIPpeakAnno)
  library(TxDb.Hsapiens.UCSC.hg38.knownGene)
  formatSeqnames=function (gr)
  {
    if (is(gr, "GRanges")) {
      seqlevels(gr)[grepl("^(\\d+|V?I{0,3}|IV|MT|M|X|Y)$",
                          seqlevels(gr))] <- paste("chr", seqlevels(gr)[grepl("^(\\d+|V?I{0,3}|IV|MT|M|X|Y)$",
                                                                              seqlevels(gr))], sep = "")
      seqlevels(gr)[seqlevels(gr) == "chrMT"] <- "chrM"
    }
    else {
      seqnames(gr)[grepl("^(\\d+|V?I{0,3}|IV|MT|M|X|Y)$",
                         seqnames(gr))] <- paste("chr", seqnames(gr)[grepl("^(\\d+|V?I{0,3}|IV|MT|M|X|Y)$",
                                                                           seqnames(gr))], sep = "")
      seqnames(gr)[seqnames(gr) == "chrMT"] <- "chrM"
    }
    gr
  }
  peaks.RD <- formatSeqnames(peaks.RD)
  peaks.RD <- unique(peaks.RD)
  annotation=genome_region
  annotation <- lapply(annotation, formatSeqnames)
  annotation <- GRangesList(annotation)
  newAnno <- c(unlist(annotation))

  newAnno.rd <- newAnno
  strand(newAnno.rd) <- "*"
  newAnno.rd <- reduce(trim(newAnno.rd))
  Intergenic.Region <- gaps(newAnno.rd, end = seqlengths(TxDb))
  Intergenic.Region <- Intergenic.Region[strand(Intergenic.Region) == "*"]

  mcols(peaks.RD) <- NULL
  #precedence=c(precedence,'exon1')
  if (!is.null(precedence)) {
    annotation <- annotation[unique(c(precedence, names(annotation)))]
  }
  names(Intergenic.Region) <- NULL
  annotation$Intergenic.Region <- Intergenic.Region
  anno.names <- names(annotation)
  ol.anno <- findOverlaps(peaks.RD, annotation, ignore.strand = ignore.strand)
  ol.anno.splited <- split(queryHits(ol.anno), anno.names[subjectHits(ol.anno)])
  jaccardIndex <- unlist(lapply(anno.names, function(.name) {
    union <- length(annotation[[.name]]) + length(peaks.RD) -
      length(unique(subjectHits(findOverlaps(peaks.RD,
                                             annotation[[.name]], ignore.strand = ignore.strand))))
    intersection <- length(ol.anno.splited[[.name]])
    intersection/union
  }))
  names(jaccardIndex) <- anno.names
  ol.anno <- as.data.frame(ol.anno)
  ol.anno.splited <- split(ol.anno, ol.anno[, 2])
  hasAnnoHits <- do.call(rbind, ol.anno.splited[names(ol.anno.splited) !=
                                                  as.character(length(annotation))])
  hasAnnoHits <- unique(hasAnnoHits[, 1])
  ol.anno <- ol.anno[!(ol.anno[, 2] == length(annotation) &
                         (ol.anno[, 1] %in% hasAnnoHits)), ]
  if (!is.null(precedence)) {
    ol.anno <- ol.anno[!duplicated(ol.anno[, 1]),
    ]
  }
  subjectHits <- anno.names[ol.anno[, 2]]
  counts <- table(subjectHits)
  percentage <- 100 * counts/length(peaks.RD)

  len <- length(anno.names) - length(percentage)
  if (len > 0) {
    tobeadd <- rep(0, len)
    names(tobeadd) <- anno.names[!anno.names %in% names(percentage)]
    percentage <- c(percentage, tobeadd)
  }
  percentage <- percentage[anno.names]
  return(list(percentage = percentage, jaccard = jaccardIndex))
}
