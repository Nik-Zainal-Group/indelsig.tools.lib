############################################
# Gene's full experimental results
############################################
source("./indel_common.R")

total_muts <- read.table("./denovo_indel.txt",sep = "\t", header = T, as.is = T) %>%
  select(Sample,chr, pos, REF, ALT) %>%
  rename(position=pos)

s <- indelsig.tools.lib::indel_classifier89(total_muts, "hg38")
s_highspecific <- indelsig.tools.lib::indel_highspecific(s)%>%
  rename(Chrom=chr, Pos=pos)%>%
  mutate(VariantID=paste0(Sample,"_",Chrom,"_",Pos,"_",REF,"_",ALT)) 

# Replicative
muts_ReplicStrand <- AddStrandInfo_intersect_indel(s_highspecific,"denovo_muts_bed.txt","../MCF7_RepliStrand.lagging38","../MCF7_RepliStrand.leading38","indels_ReplicativeStrand","total_muts_replictrand.txt")
StrandBias_indel(muts_ReplicStrand, "Sample","Strand",replitrand_length,12,20,6,"ReplicSB")
StrandBias_indel_lowr(muts_ReplicStrand, "Sample","Strand",replitrand_length_lowr,20,15,5,"ReplicSB_lowr")

StrandBias_OddsRatio_indel(muts_ReplicStrand, "Sample","Strand",replitrand_length,20,20,6,"ReplicSB_or")
StrandBias_OddsRatio_indel_lowr(muts_ReplicStrand, "Sample","Strand",replitrand_length,20,20,6,"ReplicSB_or_lowr")


sample_info <- read.table("./sample_info.txt", sep = "\t", header = T, as.is = T)
muts_ReplicStrand <- muts_ReplicStrand %>%
  left_join(sample_info, by="Sample")
StrandBias_indel_lowr(muts_ReplicStrand, "genotype","Strand",replitrand_length_lowr,15,20,5,"genotype_ReplicSB_lowr")
StrandBias_indel_lowr(muts_ReplicStrand, "edit","Strand",replitrand_length_lowr,15,20,5,"edit_ReplicSB_lowr")

