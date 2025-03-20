prepare.indel.df_tabversion <- function (indel.data, genomeSeq, genome.v)
{
  if (nrow(indel.data) > 0) {
    ref.length <- nchar(indel.data$REF)
    alt.length <- nchar(indel.data$ALT)
    indel.length <- abs(ref.length - alt.length)
    indel.type <- rep(NA, nrow(indel.data))
    indel.type[ref.length == 1 & alt.length > 1] <- "I"
    indel.type[ref.length > 1 & alt.length == 1] <- "D"
    indel.type[ref.length > 1 & alt.length > 1] <- "DI"
    indel.type[ref.length == 1 & alt.length == 1] <- "DI"
    change <- vector()
    if(sum(indel.type == "DI") >= 1){
    	change[indel.type == "DI"] <- substr(as.character(indel.data$REF)[indel.type == "DI"], 2, 1e+05)
    }
    if(sum(indel.type == "I") >= 1){
    		change[indel.type == "I"] <- substr(as.character(indel.data$ALT)[indel.type == "I"], 2, 1e+05)
    }
    if(sum(indel.type == "D") >= 1){
    			change[indel.type == "D"] <- substr(as.character(indel.data$REF),2, 1e+05)[indel.type == "D"]
    }
    min.position <- indel.data$position
    max.position <- indel.data$position + indel.length
    indel.chr <- as.character(indel.data$chr)
    extend5 = min.position - 2 * indel.length - 60
    extend3 = max.position + 2 * indel.length + 60

    sl5 <- GenomicRanges::GRanges(seqnames = indel.chr, IRanges::IRanges(extend5, min.position) )
    sl3 <- GenomicRanges::GRanges(seqnames = indel.chr, IRanges::IRanges(ifelse(indel.type == "I", min.position, max.position) +
                                                                           1, extend3) )

    slice5 <- as.character(BSgenome::getSeq(genomeSeq@single_sequences@twobitfile, sl5 ))
    slice3 <- as.character(BSgenome::getSeq(genomeSeq@single_sequences@twobitfile, sl3 ))
    indel.df <- data.frame(chr = as.character(indel.data$chr),
                           pos = indel.data$position, ref = as.character(indel.data$REF),
                           alt = as.character(indel.data$ALT), indel.type = indel.type,
                           change = change, slice3 = slice3, slice5 = slice5,
                           indel.length = indel.length)
  }
  else {
    indel.df <- data.frame()
  }
  indel.df
}


prepfunc <- function (indel.data, sampleID, genome.v = "hg19")
{
  if (genome.v == "hg19") {
    expected_chroms <- paste0("chr", c(seq(1:22), "X", "Y"))
    genomeSeq <- BSgenome.Hsapiens.UCSC.hg19::BSgenome.Hsapiens.UCSC.hg19
  }
  else if (genome.v == "hg38") {
    expected_chroms <- paste0("chr", c(seq(1:22), "X", "Y"))
    genomeSeq <- BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38
  }
  else if (genome.v == "mm10") {
    expected_chroms <- paste0("chr", c(seq(1:19), "X", "Y"))
    genomeSeq <- BSgenome.Mmusculus.UCSC.mm10::BSgenome.Mmusculus.UCSC.mm10
  }
  else if (genome.v == "canFam3") {
    expected_chroms <- paste0("chr", c(seq(1:38), "X"))
    genomeSeq <- BSgenome.Cfamiliaris.UCSC.canFam3::BSgenome.Cfamiliaris.UCSC.canFam3
  }
  vcf_seqnames <- unique(indel.data$chr)
  message("")
  indel.data <- indel.data[indel.data$chr %in% expected_chroms,,drop=F]
  if (nrow(indel.data) == 0) {
    message("[warning tabToIndelsClassification] no indels founds, nothing to process.")
    return(NULL)
  }
  indel.df <- indelsig.tools.lib:::prepare.indel.df_tabversion(indel.data, genomeSeq,
                                          genome.v)
  indels_classified <- signature.tools.lib:::mh(indel.df)
  indels_classified$indel.class <- "-"
  indels_classified$indel.class[indels_classified$classification ==
                                  "Microhomology-mediated"] <- "del.mhomology"
  indels_classified$indel.class[indels_classified$classification ==
                                  "Repeat-mediated"] <- "del.repeatmediated"
  indels_classified$indel.class[indels_classified$classification ==
                                  "None"] <- "del.other"
  indels_classified$indel.class[indels_classified$indel.type ==
                                  "I"] <- "insertion"
  indels_classified$indel.class[indels_classified$indel.type ==
                                  "DI"] <- "indel.complex"
  res <- list()
  message("")
  message("")
  return(cbind(indel.data[, !(colnames(indel.data) %in% colnames(indel.df)),
                          drop = F], indel.df))
}

