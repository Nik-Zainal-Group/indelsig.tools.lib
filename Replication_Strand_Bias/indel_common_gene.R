library(ggplot2)
library(plyr) 
library(reshape2)
library(gridExtra)
library(scales)
library(VennDiagram) # use for up to 3 sets
library(UpSetR) # use for more than 3 sets
library(Rtsne)
#library(factoextra)
library("VariantAnnotation")
library('GenomicFeatures')
library(BSgenome.Hsapiens.UCSC.hg38)
library(tidyverse)
library(indelsig.tools.lib)
library(stats)

#########################################
#
# Topography analysis
#
#########################################
replitrand_length <- read.table("./polynucleic_replication38.txt", sep = "\t", header = T, as.is = T)
replitrand_length_lowr <- read.table("./polynucleic_replication38_lowr.txt", sep = "\t", header = T, as.is = T)

# bed file is 0-based, half-closed-half-open 
Tab2Bed_38 <- function(muts, outputname){ # bed file is 0-based, half-closed-half-open 
  muts_bed <- muts[,c("Chrom","Pos","Pos","VariantID")]
  muts_bed[,2] <- muts_bed[,2]-1
  options(scipen = 100, digits = 4)
  write.table(muts_bed,outputname,sep="\t",col.names = F, row.names = F, quote = F)
  options(scipen=0)
}
AddStrandInfo_indel_38 <- function(mutfile1, mutfile2,muts_context,outputname){
  mut_strand1 <- read.table(mutfile1,sep = "\t",header = F,as.is = T)
  mut_strand2 <- read.table(mutfile2,sep = "\t",header = F,as.is = T)
  mut_strand <- rbind(mut_strand1,mut_strand2)
  mut_strand_short <- mut_strand[,c(4,8)]
  names(mut_strand_short) <- c("VariantID","Strand_original")
  
  muts_withstrandinfo <- merge(muts_context,mut_strand_short,by="VariantID",all.x=T)
  muts_withstrandinfo[is.na(muts_withstrandinfo)] <- "others"
  muts_withstrandinfo[(muts_withstrandinfo$Strand_original == "Leading" | muts_withstrandinfo$Strand_original == 1),]$Strand_original <- "leading_uts"
  muts_withstrandinfo[(muts_withstrandinfo$Strand_original == "Lagging" | muts_withstrandinfo$Strand_original == -1),]$Strand_original <- "lagging_ts"
  
  muts_withstrandinfo$Strand <- muts_withstrandinfo$Strand_original
  CTsubs_copy <- muts_withstrandinfo
  CTsubs <- muts_withstrandinfo
  
  CTsubs[(CTsubs$change_pyr!=CTsubs$change & CTsubs$Strand_original=="leading_uts"),]$Strand <- "lagging_ts"
  CTsubs[(CTsubs$change_pyr!=CTsubs$change & CTsubs$Strand_original=="lagging_ts"),]$Strand <- "leading_uts"
  
  
  write.table(CTsubs,outputname,sep="\t",col.names = T, row.names = F, quote = F)
  return(CTsubs)
}

AddRepliTimeInfo <- function(mutlist, mutBedfile,featureBedfile,intersectResultfile,outputfilename){
  Tab2Bed_38(mutlist,mutBedfile)
  intersectBed_command <- paste0("intersectBed -a ",mutBedfile," -b ",featureBedfile," -wo > ", intersectResultfile)
  
  #"nohup intersectBed -a denovo_muts_bed.txt -b /nfs/cancer_archive04/xz3/a_1242/00_common/MCF7.all.compact.bed -wo > subs_ReplicatingTime.txt &"
  try(system(intersectBed_command))
  
  subs_ReplicatingTime <- read.table(intersectResultfile,sep = "\t",header = F,as.is = T)
  subs_ReplicatingTime_short <- subs_ReplicatingTime[,c(4,9)]
  names(subs_ReplicatingTime_short) <- c("VariantID","ReplicatingTime")
  subs_ReplicatingTime_short$ReplicatingTime=abs(subs_ReplicatingTime_short$ReplicatingTime-11)
  denovo_muts_rt <- merge(mutlist,subs_ReplicatingTime_short,by="VariantID",all.x=T)
  denovo_muts_rt[is.na(denovo_muts_rt)] <- "others"
  write.table(denovo_muts_rt,outputfilename,sep = "\t", col.names = T, row.names = F, quote = F)
  
}
AddStrandInfo_intersect_indel <- function(mutlist, mutBedfile,featureBedfile_strand1,featureBedfile_strand2,intersectResultfile,outputfilename){
  Tab2Bed_38(mutlist,mutBedfile)
  intersectBed_command_1 <- paste0("intersectBed -a ",mutBedfile," -b ",featureBedfile_strand1," -wo > ", paste0(intersectResultfile,"_1.txt"))
  intersectBed_command_2 <- paste0("intersectBed -a ",mutBedfile," -b ",featureBedfile_strand2," -wo > ", paste0(intersectResultfile,"_2.txt"))
  
  # nohup intersectBed -a denovo_muts_control_mutagen_bed.txt -b ./00_data/TranscribStrand.uts.txt -wo > subs_control_mutagen_uts.txt &
  # nohup intersectBed -a denovo_muts_control_mutagen_bed.txt -b ./00_data/TranscribStrand.ts.txt -wo > subs_control_mutagen_ts.txt &
  try(system(intersectBed_command_1))
  try(system(intersectBed_command_2))
  
  AddStrandInfo_indel_38(paste0(intersectResultfile,"_1.txt"),paste0(intersectResultfile,"_2.txt"),mutlist,outputfilename)
}

Rintersect <- function(mutlist, mutBedfile,featureBedfile,intersectResultfile){
  Tab2Bed_38(mutlist,mutBedfile)
  intersectBed_command <- paste0("intersectBed -a ",mutBedfile," -b ",featureBedfile," -wo > ", paste0(intersectResultfile,".txt"))
  
  try(system(intersectBed_command))
}

# Includding simulation on replication timing distribtution of indel Sig
ReplicationTimingIndel_Sample <- function(muts_replitime,SampleCol,ReptimeCol, DoSimulation="TRUE", outputname){
  
  replitime_observed <- data.frame(table(muts_replitime[,SampleCol], muts_replitime[,ReptimeCol]))
  names(replitime_observed) <- c(SampleCol,"ReplicatingTime","observed_Freq")
  replitime_observed <- replitime_observed[replitime_observed[,ReptimeCol] != "others",]
  # simulate the expected distribution 
  if(DoSimulation){
    bootstrap_num <- 100
    muts_replitime[muts_replitime$Chrom=="23","Chrom"]="X"
    muts_replitime[muts_replitime$Chrom=="24","Chrom"]="Y"
    
    muts_replitime$seq_name <- paste0(muts_replitime$change.pyr,"_",muts_replitime$repcount)
    trinuc_catalogue <- dcast(data.frame(table(muts_replitime$seq_name,muts_replitime$Ko_gene)),Var1~Var2)
    names(trinuc_catalogue)[1] <- "seq_name"
    trinuc_replitime <- read.table("./polynucleic_replitime.txt", sep = "\t", header = T, as.is = T)
    
    # According to distribution of trinuc on replication timing regions (trinuc_replitime), bootstrap this distribution
    # bootstrapping method to evaluate the difference between expected distribution and observed one
    # bootstrapping is used to construct a population of expected distributions
    
    
    # loop for sample
    for(i in 4:dim(trinuc_catalogue)[2]){
      current_sample <- trinuc_catalogue[,c(1,i)]
      replitime_expected <- data.frame("ReplicatingTime"=c("1","2","3","4","5","6","7","8","9","10"))
      
      # loop for bootstrap numbers
      for(j in 1:bootstrap_num){
        bootstrap_replitime_all <- NULL
        
        # loop for trinucleotides
        for(k in 1:dim(current_sample)[1]){
          current_context <- as.character(current_sample[k,1])
          trinuc_replitime_current <- trinuc_replitime[trinuc_replitime$seq_name==current_context,]
          bootstrap_replitime=sample(trinuc_replitime_current$ReplicationTime, current_sample[k,2],replace = T, prob = trinuc_replitime_current$sum/sum(trinuc_replitime_current$sum))
          bootstrap_replitime_all <- c(bootstrap_replitime_all,bootstrap_replitime)
        } # k
        current_reptime <- data.frame(table(bootstrap_replitime_all))
        names(current_reptime) <- c("ReplicatingTime",paste0("bs",j))
        replitime_expected <- merge(replitime_expected,current_reptime,by="ReplicatingTime",all.x=T)
      } # j
      
      replitime_expected$expected_mean <- rowMeans(replitime_expected[,2:dim(replitime_expected)[2]])
      replitime_expected$expected_sd <- apply(replitime_expected[,2:dim(replitime_expected)[2]], 1, sd)
      replitime_observed_sample <- replitime_observed[replitime_observed[,SampleCol]==colnames(current_sample)[2],]
      
      # combine simulation (expected) and observed distributions together for each sample
      replitime_observed_expected <- merge(replitime_observed_sample,replitime_expected[,c("ReplicatingTime","expected_mean","expected_sd")],by="ReplicatingTime")
      replitime_observed_expected$ReplicatingTime <- as.numeric(as.character(replitime_observed_expected$ReplicatingTime))
      replitime_observed_expected <- replitime_observed_expected[order(replitime_observed_expected$ReplicatingTime),]
      # plot
      filename <- paste0(outputname,"_",colnames(current_sample)[2],"_simu.pdf")
      pdf(file=filename, onefile=TRUE,width=6,height=6, useDingbats=FALSE)
      p <- ggplot(data=replitime_observed_expected, aes(x=ReplicatingTime, y=observed_Freq))+ geom_bar(stat="identity",position="dodge",fill="#3cb44b")+xlab("Replication timing region")+ylab("Count")
      p <- p+geom_point(data=replitime_observed_expected,aes(x=ReplicatingTime,y=expected_mean),color="blue")+geom_line(data=replitime_observed_expected,aes(x=ReplicatingTime,y=expected_mean, group=1),color="blue")
      p <- p+geom_errorbar(data=replitime_observed_expected,aes(ymin=expected_mean-expected_sd,ymax=expected_mean+expected_sd),color="blue",position=position_dodge(.9),size=.2,width=0.5) #+scale_y_continuous(limits=c(0, 350),breaks=seq(0, 350, 100))
      p <- p+ggtitle(paste0(colnames(current_sample)[2]))
      p <- p+theme(axis.text.x=element_text(size=15,colour = "black"),
                   axis.text.y=element_text(size=15,colour = "black"),
                   axis.title.x = element_text(size=15),
                   axis.title.y = element_text(size=15),
                   plot.title = element_text(size=10),
                   panel.grid.minor.x=element_blank(),
                   panel.grid.major.x=element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.background = element_rect(fill = "white"),
                   panel.border = element_rect(colour = "black", fill=NA))
      print(p)
      dev.off()
      write.table(replitime_observed_expected,paste0(outputname,"_",colnames(current_sample)[2],"_simu.txt"),sep = "\t",col.names = T, row.names = F, quote = F)
      
    } # i
    
  }else{
    
    # loop for sample
    for(i in 2:dim(trinuc_catalogue)[2]){
      current_sample <- trinuc_catalogue[,c(1,i)]
      replitime_observed_sample <- replitime_observed[replitime_observed[,SampleCol]==colnames(current_sample)[2],]
      replitime_observed_sample$ReplicatingTime <- as.numeric(as.character(replitime_observed_sample$ReplicatingTime))
      replitime_observed_sample <- replitime_observed_sample[order(replitime_observed_sample$ReplicatingTime),]
      
      # plot
      filename <- paste0(outputname,"_",colnames(current_sample)[2],"_observed.pdf")
      pdf(file=filename, onefile=TRUE,width=6,height=6, useDingbats=FALSE)
      p <- ggplot(data=replitime_observed_sample, aes(x=ReplicatingTime, y=observed_Freq))+ geom_bar(stat="identity",position="dodge",fill="#3cb44b")+xlab("Replication timing region")+ylab("Count")
      p <- p+ggtitle(paste0(colnames(current_sample)[2]))
      p <- p+theme(axis.text.x=element_text(size=15,colour = "black"),
                   axis.text.y=element_text(size=15,colour = "black"),
                   axis.title.x = element_text(size=15),
                   axis.title.y = element_text(size=15),
                   plot.title = element_text(size=10),
                   panel.grid.minor.x=element_blank(),
                   panel.grid.major.x=element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.background = element_rect(fill = "white"),
                   panel.border = element_rect(colour = "black", fill=NA))
      print(p)
      dev.off()
      write.table(replitime_observed_sample,paste0(outputname,"_",colnames(current_sample)[2],"_observed.txt"),sep = "\t",col.names = T, row.names = F, quote = F)
      
      
    }
  }
  
}

plotCountbasis_average_sd_feature <- function(muts_basis,muttype_template,selectcolor,h,w,outputname){
  
  muts_basis[is.na(muts_basis)] <- 0
  mean_parentmuts <- sum(muts_basis[,2:dim(muts_basis)[2]])/(dim(muts_basis)[2]-1)
  
  muts_basis_melt <- melt(muts_basis,"targetfeature")
  names(muts_basis_melt) <- c("targetfeature","sample","count")
  muts_basis_melt_summary <- ddply(muts_basis_melt,c("targetfeature"),summarise, N=length(count),mean=mean(count),sd=sd(count),se=sd/sqrt(N),snr=abs(mean/sd))
  
  muts_basis_melt_summary$mean_perc <- muts_basis_melt_summary$mean/mean_parentmuts
  muts_basis_melt_summary$sd_perc <- muts_basis_melt_summary$sd/mean_parentmuts
  muts_basis_melt_summary$se_perc <- muts_basis_melt_summary$se/mean_parentmuts
  muts_basis_melt_summary$snr_perc <- muts_basis_melt_summary$mean/muts_basis_melt_summary$sd
  muts_basis_melt_summary <- muts_basis_melt_summary[order(muts_basis_melt_summary$targetfeature),]
  
  
  filename <- paste0(outputname, ".pdf")
  pdf(file=filename, onefile=TRUE,width=w,height=h, useDingbats=FALSE)
  p <- ggplot(data=muts_basis_melt_summary, aes(x=targetfeature, y=mean))+ geom_bar(stat="identity",position="dodge", width=.8,fill=selectcolor)+xlab("Mutation Types")+ylab("Count")
  p <- p+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),color="blue",position=position_dodge(.9),size=.2,width=0.5) #+scale_y_continuous(limits=c(0, 350),breaks=seq(0, 350, 100))
  p <- p+scale_x_discrete(limits = as.character(muts_basis_melt_summary$targetfeature))+ggtitle(outputname)
  p <- p+scale_fill_manual(values=mypalette)
  p <- p+theme(axis.text.x=element_text(size=10,colour = "black"),
               axis.text.y=element_text(size=10,colour = "black"),
               axis.title.x = element_text(size=15),
               axis.title.y = element_text(size=15),
               plot.title = element_text(size=10),
               panel.grid.minor.x=element_blank(),
               panel.grid.major.x=element_blank(),
               panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank(),
               panel.background = element_rect(fill = "white"),
               panel.border = element_rect(colour = "black", fill=NA))
  
  q <- ggplot(data=muts_basis_melt_summary, aes(x=targetfeature, y=mean_perc))+ geom_bar(stat="identity",position="dodge", width=.8,fill=selectcolor)+xlab("Mutation Types")+ylab("Percentage")
  q <- q+geom_errorbar(aes(ymin=mean_perc-sd_perc,ymax=mean_perc+sd_perc),color="blue",position=position_dodge(.9),size=.2,width=0.5) #+scale_y_continuous(limits=c(0, 350),breaks=seq(0, 350, 100))
  q <- q+scale_x_discrete(limits = as.character(muts_basis_melt_summary$targetfeature))+ggtitle(outputname)
  q <- q+scale_fill_manual(values=mypalette)
  q <- q+theme(axis.text.x=element_text(size=10,colour = "black"),
               axis.text.y=element_text(size=10,colour = "black"),
               axis.title.x = element_text(size=15),
               axis.title.y = element_text(size=15),
               plot.title = element_text(size=10),
               panel.grid.minor.x=element_blank(),
               panel.grid.major.x=element_blank(),
               panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank(),
               panel.background = element_rect(fill = "white"),
               panel.border = element_rect(colour = "black", fill=NA))
  
  grid.arrange(p,q,ncol=1)
  dev.off()
  write.table(muts_basis_melt_summary,paste0("targetfeature","_",outputname, "_basis.txt"),sep = "\t",row.names = F, col.names = T, quote = F)
  
  return(muts_basis_melt_summary)
  
  
  
  
}
# Plot Strand bias for repeat 1-9
StrandBias_indel <- function(denovo_muts_feature, SampleCol,feature,featurelength,h,w,colnum, outputname){
  
  #featurelength$all <-rowSums(featurelength[,2:5])
  
  # 1: uts/leading
  # -1: ts/lagging
  #featurelength_strand <- data.frame("Strand"=c("1","-1"),"C"=c(sum(featurelength[,"C"]), sum(featurelength[,"G"])),"T"=c(sum(featurelength[,"T"]), sum(featurelength[,"A"])))
  featurelength_strand <- dcast(featurelength[,c("seq_name","Strand_pyrimidine","sum")],seq_name~Strand_pyrimidine, value.var="sum")
  names(featurelength_strand) <- c("Ref","ts_lagging_wg","uts_leading_wg")
  denovo_muts_feature <- denovo_muts_feature[denovo_muts_feature[,feature] != "others",]
  denovo_muts_feature$Mutation <- paste0(denovo_muts_feature$type_2,denovo_muts_feature$original_reps)
  denovo_muts_feature <- denovo_muts_feature %>%
    filter(indel.length==1) %>%
    filter(original_reps>1)
  denovo_muts_dis <- data.frame(table(denovo_muts_feature[,SampleCol],denovo_muts_feature[,feature], denovo_muts_feature[,"Mutation"]))
  names(denovo_muts_dis) <- c("Sample",feature,"Mutation", "Freq")
  
  gtc <- denovo_muts_dis
  gtc$targetfeature <- gtc[,feature]
  
  filename=paste0(outputname,".pdf")
  pdf(file=filename, onefile=TRUE,height=h,width = w) 
  d1 <- ggplot(gtc,aes(x=Mutation,y=Freq,fill=targetfeature))+geom_bar(stat="identity",position="dodge")
  d1 <- d1+ylab("Count")+theme(axis.title.x = element_text(size=15),
                               axis.title.y = element_text(size=15),
                               plot.title = element_text(size=10),
                               axis.text.x=element_text(angle=90, vjust=0.5,colour = "black"),
                               axis.text.y=element_text(colour = "black"),
                               panel.grid.minor.x=element_blank(),
                               panel.grid.major.x=element_blank(),
                               panel.grid.major.y = element_blank(),
                               panel.grid.minor.y = element_blank(),
                               panel.background = element_rect(fill = "white"),
                               panel.border = element_rect(colour = "black", fill=NA))
  d1 <- d1+facet_wrap(~Sample,ncol=colnum,scales="free")
  print(d1)
  dev.off()
  
  gtc_dcast <- dcast(gtc,Sample+Mutation~Strand, value.var="Freq")
  gtc_dcast$Ref <- paste0(substr(gtc_dcast$Mutation,5,5),"_",substr(gtc_dcast$Mutation,7,7)) 
  gtc_dcast <- merge(gtc_dcast, featurelength_strand, by="Ref", all.x=T)
  gtc_dcast$chisq_pvalue <- 1
  for(i in 1:dim(gtc_dcast)[1]){
    if(gtc_dcast[i,"leading_uts"]+gtc_dcast[i,"lagging_ts"]>0){
      gtc_dcast[i,"chisq_pvalue"] <- chisq.test(c(gtc_dcast[i,"leading_uts"], gtc_dcast[i,"lagging_ts"]), p=c(gtc_dcast[i,"uts_leading_wg"], gtc_dcast[i,"ts_lagging_wg"]),rescale.p = TRUE)$p.value
      
    }
    
  }
  gtc_dcast$P_adjust_chisq <- p.adjust(gtc_dcast$chisq_pvalue,method = "BH")
  gtc_dcast$flag_chisq <- ""
  gtc_dcast[gtc_dcast$P_adjust_chisq<=0.05,"flag_chisq"] <- "*"
  gtc_dcast[gtc_dcast$P_adjust_chisq<=0.01,"flag_chisq"] <- "**"
  gtc_dcast[gtc_dcast$P_adjust_chisq<=0.001,"flag_chisq"] <- "***"
  
  # binomial test
  gtc_dcast$binom_pvalue <- 1
  for(i in 1:dim(gtc_dcast)[1]){
    if(gtc_dcast[i,"leading_uts"]+gtc_dcast[i,"lagging_ts"]>0){
      gtc_dcast[i,"binom_pvalue"] <- binom.test(c(gtc_dcast[i,"leading_uts"], gtc_dcast[i,"lagging_ts"]), p=gtc_dcast[i,"uts_leading_wg"]/(gtc_dcast[i,"uts_leading_wg"]+ gtc_dcast[i,"ts_lagging_wg"]))$p.value
      
    }
    
  }
  gtc_dcast$P_adjust_binom <- p.adjust(gtc_dcast$binom_pvalue,method = "BH")
  gtc_dcast$flag_binom <- ""
  gtc_dcast[gtc_dcast$P_adjust_binom<=0.05,"flag_binom"] <- "*"
  gtc_dcast[gtc_dcast$P_adjust_binom<=0.01,"flag_binom"] <- "**"
  gtc_dcast[gtc_dcast$P_adjust_binom<=0.001,"flag_binom"] <- "***"
  
  # FET test
  if(FALSE){
  gtc_dcast$fet_pvalue <- 1
  for(i in 1:dim(gtc_dcast)[1]){
    if(gtc_dcast[i,"leading_uts"]+gtc_dcast[i,"lagging_ts"]>0){
      gtc_dcast[i,"fet_pvalue"] <- fisher.test(matrix(gtc_dcast[i,"leading_uts"], gtc_dcast[i,"lagging_ts"], gtc_dcast[i,"uts_leading_wg"], gtc_dcast[i,"ts_lagging_wg"]))$p.value
      
    }
    
  }
  gtc_dcast$P_adjust_fet <- p.adjust(gtc_dcast$fet_pvalue,method = "BH")
  gtc_dcast$flag_fet <- ""
  gtc_dcast[gtc_dcast$P_adjust_fet<=0.05,"flag_fet"] <- "*"
  gtc_dcast[gtc_dcast$P_adjust_fet<=0.01,"flag_fet"] <- "**"
  gtc_dcast[gtc_dcast$P_adjust_fet<=0.001,"flag_fet"] <- "***"
  }
  
  
  write.table(gtc_dcast,paste0(outputname, "_pvalue_adjust.txt"),sep = "\t",col.names = T, row.names = F, quote = F)
  
  
}
StrandBias_OddsRatio_indel <- function(denovo_muts_feature, SampleCol,feature,featurelength,h,w,colnum, outputname){
  
  #featurelength$all <-rowSums(featurelength[,2:5])
  
  # 1: uts/leading
  # -1: ts/lagging
  #featurelength_strand <- data.frame("Strand"=c("1","-1"),"C"=c(sum(featurelength[,"C"]), sum(featurelength[,"G"])),"T"=c(sum(featurelength[,"T"]), sum(featurelength[,"A"])))
  #  featurelength_strand <- dcast(featurelength[,c("seq_name","Strand_pyrimidine","sum")],seq_name~Strand_pyrimidine, value.var="sum")
  #  names(featurelength_strand) <- c("Ref","ts_lagging_wg","uts_leading_wg")
  #  denovo_muts_feature <- denovo_muts_feature[denovo_muts_feature[,feature] != "others",]
  #  denovo_muts_feature$Mutation <- paste0(denovo_muts_feature$type_2,denovo_muts_feature$original_reps)
  #  denovo_muts_dis <- data.frame(table(denovo_muts_feature[,SampleCol],denovo_muts_feature[,feature], denovo_muts_feature[,"Mutation"]))
  #  names(denovo_muts_dis) <- c("Sample",feature,"Mutation", "Freq")
  
  featurelength_strand <- dcast(featurelength[,c("seq_name","Strand_pyrimidine","sum")],seq_name~Strand_pyrimidine, value.var="sum")
  names(featurelength_strand) <- c("Ref","ts_lagging_wg","uts_leading_wg")
  denovo_muts_feature <- denovo_muts_feature[denovo_muts_feature[,feature] != "others",]
  denovo_muts_feature$Mutation <- paste0(denovo_muts_feature$type_2,denovo_muts_feature$original_reps)
  denovo_muts_feature <- denovo_muts_feature %>%
    filter(indel.length==1) %>%
    filter(original_reps>1)
  denovo_muts_dis <- data.frame(table(denovo_muts_feature[,SampleCol],denovo_muts_feature[,feature], denovo_muts_feature[,"Mutation"]))
  names(denovo_muts_dis) <- c("Sample",feature,"Mutation", "Freq")
  
  gtc <- denovo_muts_dis
  gtc$targetfeature <- gtc[,feature]
  
  gtc_dcast <- dcast(gtc,Sample+Mutation~Strand, value.var="Freq")
  gtc_dcast$Ref <- paste0(substr(gtc_dcast$Mutation,5,5),"_",substr(gtc_dcast$Mutation,7,7)) 
  gtc_dcast <- merge(gtc_dcast, featurelength_strand, by="Ref", all.x=T)
  gtc_dcast$oddsratio <- 1
  gtc_dcast$lowerconfint <- 1
  gtc_dcast$higherconfint <- 1
  for(i in 1:dim(gtc_dcast)[1]){
    if(gtc_dcast[i,"leading_uts"]*gtc_dcast[i,"lagging_ts"]>0){
      
      M <- matrix(c(gtc_dcast[i,"lagging_ts"], gtc_dcast[i,"leading_uts"], (gtc_dcast[i,"ts_lagging_wg"]-gtc_dcast[i,"lagging_ts"]), (gtc_dcast[i,"uts_leading_wg"]-gtc_dcast[i,"leading_uts"])), ncol = 2)
      b <- data.frame(t(calcOddsRatio(M)))
      names(b) <- c("oddsratio","lowerconfint","higherconfint")
      gtc_dcast[i,"oddsratio"] <- b$oddsratio
      gtc_dcast[i,"lowerconfint"] <- b$lowerconfint
      gtc_dcast[i,"higherconfint"] <- b$higherconfint
  #    gtc_dcast[i,"chisq_pvalue"] <- chisq.test(c(gtc_dcast[i,"leading_uts"], gtc_dcast[i,"lagging_ts"]), p=c(gtc_dcast[i,"uts_leading_wg"], gtc_dcast[i,"ts_lagging_wg"]),rescale.p = TRUE)$p.value
      
    }
    
  }
  gtc_dcast$flag <- ""
  gtc_dcast[gtc_dcast$lowerconfint>1 | gtc_dcast$higherconfint<1,"flag"] <- "*"
  write.table(gtc_dcast,paste0(outputname, "_oddsratio.txt"),sep = "\t",col.names = T, row.names = F, quote = F)
  
  filename=paste0(outputname,"_oddsratio.pdf")
  pdf(file=filename, onefile=TRUE,height=h,width = w) 
  d1 <- ggplot(gtc_dcast,aes(x=oddsratio,y=Mutation))+geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") 
  d1 <- d1 + geom_errorbarh(aes(xmax = higherconfint, xmin = lowerconfint), size = .5, height = .2, color = "gray50")
  d1 <- d1 + geom_point(size = 3.5, color = "orange") +ylab("") +xlab("Odds Ratio")
#  d1 <- d1 + coord_trans(x = scales:::exp_trans(10)) + scale_x_continuous(breaks = log10(seq(0.1, 2.5, 0.1)), labels = seq(0.1, 2.5, 0.1),
    #                   limits = log10(c(0.09,2.5)))
  d1 <- d1+theme(axis.title.x = element_text(size=15),
                               axis.title.y = element_text(size=15),
                               plot.title = element_text(size=10),
                               axis.text.x=element_text(angle=90, vjust=0.5,colour = "black"),
                               axis.text.y=element_text(colour = "black"),
                               panel.grid.minor.x=element_blank(),
                               panel.grid.major.x=element_blank(),
                               panel.grid.major.y = element_blank(),
                               panel.background = element_rect(fill = "white"),
                               panel.border = element_rect(colour = "black", fill=NA))
  d1 <- d1+facet_wrap(~Sample,ncol=colnum,scales="free")
  print(d1)
  dev.off()
  
  
}
AddFeatureInfo <- function(mutlist, mutBedfile,featureBedfile,intersectResultfile,outputfilename){
  Tab2Bed_38(mutlist,mutBedfile)
  intersectBed_command <- paste0("intersectBed -a ",mutBedfile," -b ",featureBedfile," -wo > ", intersectResultfile)
  
  #"nohup intersectBed -a denovo_muts_bed.txt -b /nfs/cancer_archive04/xz3/a_1242/00_common/MCF7.all.compact.bed -wo > subs_ReplicatingTime.txt &"
  try(system(intersectBed_command))
  
  subs_Feature <- read.table(intersectResultfile,sep = "\t",header = F,as.is = T)
  subs_Feature_short <- subs_Feature[,c(4,8)]
  names(subs_Feature_short) <- c("VariantID","Feature")
  denovo_muts_rt <- merge(mutlist,subs_Feature_short,by="VariantID",all.x=T)
  denovo_muts_rt[is.na(denovo_muts_rt)] <- "NonRegulatory"
  write.table(denovo_muts_rt,outputfilename,sep = "\t", col.names = T, row.names = F, quote = F)
  
}
Feature_Sample_indel <- function(muts_feature,SampleCol,FeatureCol,trinuc_feature, FeatureName,outputname){
  
  feature_observed <- data.frame(table(muts_feature[,SampleCol], muts_feature[,FeatureCol]))
  names(feature_observed) <- c(SampleCol,"Feature","observed_Freq")
  # simulate the expected distribution 
  
  bootstrap_num <- 100
  muts_feature[muts_feature$Chrom=="23","Chrom"]="X"
  muts_feature[muts_feature$Chrom=="24","Chrom"]="Y"
  
  muts_feature$seq_name <- paste0(muts_feature$change.pyr,"_",muts_feature$repcount)
  trinuc_catalogue <- dcast(data.frame(table(muts_feature$seq_name,muts_feature$Ko_gene)),Var1~Var2)
  names(trinuc_catalogue)[1] <- "seq_name"
  
  #trinuc_catalogue <- Gen32Catalogue(muts_feature,SampleCol)
  # trinuc_feature <- read.table(TrinucFeatureFile, sep = "\t", header = F, as.is = T)
  # names(TrinucFeature) <- c("trinuc_pyrimidine","Feature","sum")
  # According to distribution of trinuc on replication timing regions (trinuc_replitime), bootstrap this distribution
  # bootstrapping method to evaluate the difference between expected distribution and observed one
  # bootstrapping is used to construct a population of expected distributions
  
  # loop for sample
  for(i in 2:dim(trinuc_catalogue)[2]){
    current_sample <- trinuc_catalogue[,c(1,i)]
    feature_expected <- data.frame(table(muts_feature[,FeatureCol]))
    names(feature_expected) <- c("Feature","Observed")
    # loop for bootstrap numbers
    for(j in 1:bootstrap_num){
      bootstrap_Feature_all <- NULL
      
      # loop for trinucleotides
      for(k in 1:dim(current_sample)[1]){
        current_context <- as.character(current_sample[k,1])
        trinuc_feature_current <- trinuc_feature[trinuc_feature$seq_name==current_context,]
        bootstrap_Feature=sample(trinuc_feature_current$Feature, current_sample[k,2],replace = T, prob = trinuc_feature_current$sum/sum(trinuc_feature_current$sum))
        bootstrap_Feature_all <- c(bootstrap_Feature_all,bootstrap_Feature)
      } # k
      current_feature <- data.frame(table(bootstrap_Feature_all))
      names(current_feature) <- c("Feature",paste0("bs",j))
      feature_expected <- merge(feature_expected,current_feature,by="Feature",all.x=T)
    } # j
    feature_expected <- feature_expected[,-2]
    feature_expected$expected_mean <- rowMeans(feature_expected[,2:dim(feature_expected)[2]])
    feature_expected$expected_sd <- apply(feature_expected[,2:dim(feature_expected)[2]], 1, sd)
    feature_expected_sample <- feature_observed[feature_observed[,SampleCol]==colnames(current_sample)[2],]
    
    # combine simulation (expected) and observed distributions together for each sample
    feature_observed_expected <- merge(feature_expected_sample,feature_expected[,c("Feature","expected_mean","expected_sd")],by="Feature")
    # plot
    filename <- paste0(outputname,"_",colnames(current_sample)[2],"_simu.pdf")
    pdf(file=filename, onefile=TRUE,width=6,height=6, useDingbats=FALSE)
    p <- ggplot(data=feature_observed_expected, aes(x=Feature, y=observed_Freq))+ geom_bar(stat="identity",position="dodge",fill="#3cb44b")+xlab("Feature")+ylab("Count")
    p <- p+geom_point(data=feature_observed_expected,aes(x=Feature,y=expected_mean),color="blue")
    p <- p+geom_errorbar(data=feature_observed_expected,aes(ymin=expected_mean-expected_sd,ymax=expected_mean+expected_sd),color="blue",position=position_dodge(.9),size=.2,width=0.5) #+scale_y_continuous(limits=c(0, 350),breaks=seq(0, 350, 100))
    p <- p+ggtitle(paste0(colnames(current_sample)[2]))
    p <- p+theme(axis.text.x=element_text(size=15,colour = "black"),
                 axis.text.y=element_text(size=15,colour = "black"),
                 axis.title.x = element_text(size=15),
                 axis.title.y = element_text(size=15),
                 plot.title = element_text(size=10),
                 panel.grid.minor.x=element_blank(),
                 panel.grid.major.x=element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.background = element_rect(fill = "white"),
                 panel.border = element_rect(colour = "black", fill=NA))
    print(p)
    dev.off()
    
    feature_observed_expected$chisq_pvalue <- chisq.test(c(feature_observed_expected[1,"observed_Freq"], feature_observed_expected[2,"observed_Freq"]), p=c(feature_observed_expected[1,"expected_mean"], feature_observed_expected[2,"expected_mean"]),rescale.p = TRUE)$p.value
    feature_observed_expected$binom_pvalue <- binom.test(feature_observed_expected[feature_observed_expected$Feature==FeatureName,"observed_Freq"],sum(feature_observed_expected$observed_Freq),p=feature_observed_expected[feature_observed_expected$Feature==FeatureName,"expected_mean"]/feature_observed_expected[feature_observed_expected$Feature=="NonRegulatory","expected_mean"])$p.value
    
    write.table(feature_observed_expected,paste0(outputname,"_",colnames(current_sample)[2],"_simu.txt"),sep = "\t",col.names = T, row.names = F, quote = F)
    
  } # i
  
  
  
  
  
  
  
  
  
  
}

calcOddsRatio <- function(mymatrix,alpha=0.05,referencerow=2,quiet=FALSE)
{
  numrow <- nrow(mymatrix)
  myrownames <- rownames(mymatrix)
  
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    DiseaseUnexposed <- mymatrix[referencerow,1]
    ControlUnexposed <- mymatrix[referencerow,2]
    if (i != referencerow)
    {
      DiseaseExposed <- mymatrix[i,1]
      ControlExposed <- mymatrix[i,2]
      
      totExposed <- DiseaseExposed + ControlExposed
      totUnexposed <- DiseaseUnexposed + ControlUnexposed
      
      probDiseaseGivenExposed <- DiseaseExposed/totExposed
      probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
      probControlGivenExposed <- ControlExposed/totExposed
      probControlGivenUnexposed <- ControlUnexposed/totUnexposed
      
      # calculate the odds ratio
      oddsRatio <- (probDiseaseGivenExposed*probControlGivenUnexposed)/
        (probControlGivenExposed*probDiseaseGivenUnexposed)
      if (quiet == FALSE)
      {
        print(paste("category =", rowname, ", odds ratio = ",oddsRatio))
      }
      
      # calculate a confidence interval
      confidenceLevel <- (1 - alpha)*100
      sigma <- sqrt((1/DiseaseExposed)+(1/ControlExposed)+
                      (1/DiseaseUnexposed)+(1/ControlUnexposed))
      # sigma is the standard error of our estimate of the log of the odds ratio
      z <- qnorm(1-(alpha/2))
      lowervalue <- oddsRatio * exp(-z * sigma)
      uppervalue <- oddsRatio * exp( z * sigma)
      if (quiet == FALSE)
      {
        print(paste("category =", rowname, ", ", confidenceLevel,
                    "% confidence interval = [",lowervalue,",",uppervalue,"]"))
        return(c(oddsRatio,lowervalue,uppervalue))
      }
    }
  }
  if (quiet == TRUE && numrow == 2) # If there are just two treatments (exposed/nonexposed)
  {
    return(oddsRatio)
  }
}


#  Strand bias at low resolution for single base indels (length==1)
StrandBias_indel_lowr <- function(denovo_muts_feature, SampleCol,feature,featurelength,h,w,colnum, outputname){
  
  #featurelength$all <-rowSums(featurelength[,2:5])
  
  # 1: uts/leading
  # -1: ts/lagging
  #featurelength_strand <- data.frame("Strand"=c("1","-1"),"C"=c(sum(featurelength[,"C"]), sum(featurelength[,"G"])),"T"=c(sum(featurelength[,"T"]), sum(featurelength[,"A"])))
  featurelength_strand <- dcast(featurelength[,c("seq_name","Strand_pyrimidine","sum")],seq_name~Strand_pyrimidine, value.var="sum")
  names(featurelength_strand) <- c("Ref","ts_lagging_wg","uts_leading_wg")
  denovo_muts_feature <- denovo_muts_feature[denovo_muts_feature[,feature] != "others",]
  denovo_muts_feature <- denovo_muts_feature %>%
    filter(indel.length==1) %>%
    mutate(rep_level=case_when(
      original_reps>=2 & original_reps<=4 ~ "234",
      original_reps>=5 & original_reps<=7 ~ "567",
      original_reps>=8 ~ "89",
      TRUE ~ "other"
    )) %>%
    filter(rep_level!="other") 
  denovo_muts_feature$Mutation <- paste0(denovo_muts_feature$type_2,denovo_muts_feature$rep_level)
  
 # denovo_muts_feature$Mutation <- paste0(substr(denovo_muts_feature$type_2,5,5),"_",denovo_muts_feature$rep_level)
  denovo_muts_dis <- data.frame(table(denovo_muts_feature[,SampleCol],denovo_muts_feature[,feature], denovo_muts_feature[,"Mutation"]))
  names(denovo_muts_dis) <- c("Sample",feature,"Mutation", "Freq")
  
  gtc <- denovo_muts_dis
  gtc$targetfeature <- gtc[,feature]
  
  filename=paste0(outputname,".pdf")
  pdf(file=filename, onefile=TRUE,height=h,width = w) 
  d1 <- ggplot(gtc,aes(x=Mutation,y=Freq,fill=targetfeature))+geom_bar(stat="identity",position="dodge")
  d1 <- d1+ylab("Count")+theme(axis.title.x = element_text(size=15),
                               axis.title.y = element_text(size=15),
                               plot.title = element_text(size=10),
                               axis.text.x=element_text(angle=90, vjust=0.5,colour = "black"),
                               axis.text.y=element_text(colour = "black"),
                               panel.grid.minor.x=element_blank(),
                               panel.grid.major.x=element_blank(),
                               panel.grid.major.y = element_blank(),
                               panel.grid.minor.y = element_blank(),
                               panel.background = element_rect(fill = "white"),
                               panel.border = element_rect(colour = "black", fill=NA))
  d1 <- d1+facet_wrap(~Sample,ncol=colnum,scales="free")
  print(d1)
  dev.off()
  
  gtc_dcast <- dcast(gtc,Sample+Mutation~Strand, value.var="Freq")
  gtc_dcast$Ref <- paste0(substr(gtc_dcast$Mutation,5,5),"_",sub(".*\\]","",gtc_dcast$Mutation))

  gtc_dcast <- merge(gtc_dcast, featurelength_strand, by="Ref", all.x=T)
  gtc_dcast$chisq_pvalue <- 1
  for(i in 1:dim(gtc_dcast)[1]){
    if(gtc_dcast[i,"leading_uts"]+gtc_dcast[i,"lagging_ts"]>0){
      gtc_dcast[i,"chisq_pvalue"] <- chisq.test(c(gtc_dcast[i,"leading_uts"], gtc_dcast[i,"lagging_ts"]), p=c(gtc_dcast[i,"uts_leading_wg"], gtc_dcast[i,"ts_lagging_wg"]),rescale.p = TRUE)$p.value
      
    }
    
  }
  gtc_dcast$P_adjust_chisq <- p.adjust(gtc_dcast$chisq_pvalue,method = "BH")
  gtc_dcast$flag_chisq <- ""
  gtc_dcast[gtc_dcast$P_adjust_chisq<=0.05,"flag_chisq"] <- "*"
  gtc_dcast[gtc_dcast$P_adjust_chisq<=0.01,"flag_chisq"] <- "**"
  gtc_dcast[gtc_dcast$P_adjust_chisq<=0.001,"flag_chisq"] <- "***"
  
  # binomial test
  gtc_dcast$binom_pvalue <- 1
  for(i in 1:dim(gtc_dcast)[1]){
    if(gtc_dcast[i,"leading_uts"]+gtc_dcast[i,"lagging_ts"]>0){
      gtc_dcast[i,"binom_pvalue"] <- binom.test(c(gtc_dcast[i,"leading_uts"], gtc_dcast[i,"lagging_ts"]), p=gtc_dcast[i,"uts_leading_wg"]/(gtc_dcast[i,"uts_leading_wg"]+ gtc_dcast[i,"ts_lagging_wg"]))$p.value
      
    }
    
  }
  gtc_dcast$P_adjust_binom <- p.adjust(gtc_dcast$binom_pvalue,method = "BH")
  gtc_dcast$flag_binom <- ""
  gtc_dcast[gtc_dcast$P_adjust_binom<=0.05,"flag_binom"] <- "*"
  gtc_dcast[gtc_dcast$P_adjust_binom<=0.01,"flag_binom"] <- "**"
  gtc_dcast[gtc_dcast$P_adjust_binom<=0.001,"flag_binom"] <- "***"
  
  # FET test
  if(FALSE){
  gtc_dcast$fet_pvalue <- 1
  for(i in 1:dim(gtc_dcast)[1]){
    if(gtc_dcast[i,"leading_uts"]+gtc_dcast[i,"lagging_ts"]>0){
      gtc_dcast[i,"fet_pvalue"] <- fisher.test(matrix(gtc_dcast[i,"leading_uts"], gtc_dcast[i,"lagging_ts"], gtc_dcast[i,"uts_leading_wg"], gtc_dcast[i,"ts_lagging_wg"]))$p.value
      
    }
    
  }
  gtc_dcast$P_adjust_fet <- p.adjust(gtc_dcast$fet_pvalue,method = "BH")
  gtc_dcast$flag_fet <- ""
  gtc_dcast[gtc_dcast$P_adjust_fet<=0.05,"flag_fet"] <- "*"
  gtc_dcast[gtc_dcast$P_adjust_fet<=0.01,"flag_fet"] <- "**"
  gtc_dcast[gtc_dcast$P_adjust_fet<=0.001,"flag_fet"] <- "***"
  }
  
  write.table(gtc_dcast,paste0(outputname, "_pvalue_adjust.txt"),sep = "\t",col.names = T, row.names = F, quote = F)
  
  
}
StrandBias_OddsRatio_indel_lowr <- function(denovo_muts_feature, SampleCol,feature,featurelength,h,w,colnum, outputname){
  
  featurelength_strand <- dcast(featurelength[,c("seq_name","Strand_pyrimidine","sum")],seq_name~Strand_pyrimidine, value.var="sum")
  names(featurelength_strand) <- c("Ref","ts_lagging_wg","uts_leading_wg")
  denovo_muts_feature <- denovo_muts_feature[denovo_muts_feature[,feature] != "others",]
  denovo_muts_feature <- denovo_muts_feature %>%
    filter(indel.length==1) %>%
    mutate(rep_level=case_when(
      original_reps>=2 & original_reps<=4 ~ "234",
      original_reps>=5 & original_reps<=7 ~ "567",
      original_reps>=8 ~ "89",
      TRUE ~ "other"
    )) %>%
    filter(rep_level!="other") 
  denovo_muts_feature$Mutation <- paste0(denovo_muts_feature$type_2,denovo_muts_feature$rep_level)
  
  # denovo_muts_feature$Mutation <- paste0(substr(denovo_muts_feature$type_2,5,5),"_",denovo_muts_feature$rep_level)
  denovo_muts_dis <- data.frame(table(denovo_muts_feature[,SampleCol],denovo_muts_feature[,feature], denovo_muts_feature[,"Mutation"]))
  names(denovo_muts_dis) <- c("Sample",feature,"Mutation", "Freq")
  
  gtc <- denovo_muts_dis
  gtc$targetfeature <- gtc[,feature]
  
  gtc_dcast <- dcast(gtc,Sample+Mutation~Strand, value.var="Freq")
  gtc_dcast$Ref <- paste0(substr(gtc_dcast$Mutation,5,5),"_",substr(gtc_dcast$Mutation,7,7)) 
  gtc_dcast <- merge(gtc_dcast, featurelength_strand, by="Ref", all.x=T)
  gtc_dcast$oddsratio <- 1
  gtc_dcast$lowerconfint <- 1
  gtc_dcast$higherconfint <- 1
  for(i in 1:dim(gtc_dcast)[1]){
    if(gtc_dcast[i,"leading_uts"]*gtc_dcast[i,"lagging_ts"]>0){
      
      M <- matrix(c(gtc_dcast[i,"lagging_ts"], gtc_dcast[i,"leading_uts"], (gtc_dcast[i,"ts_lagging_wg"]-gtc_dcast[i,"lagging_ts"]), (gtc_dcast[i,"uts_leading_wg"]-gtc_dcast[i,"leading_uts"])), ncol = 2)
      b <- data.frame(t(calcOddsRatio(M)))
      names(b) <- c("oddsratio","lowerconfint","higherconfint")
      gtc_dcast[i,"oddsratio"] <- b$oddsratio
      gtc_dcast[i,"lowerconfint"] <- b$lowerconfint
      gtc_dcast[i,"higherconfint"] <- b$higherconfint
      #    gtc_dcast[i,"chisq_pvalue"] <- chisq.test(c(gtc_dcast[i,"leading_uts"], gtc_dcast[i,"lagging_ts"]), p=c(gtc_dcast[i,"uts_leading_wg"], gtc_dcast[i,"ts_lagging_wg"]),rescale.p = TRUE)$p.value
      
    }
    
  }
  gtc_dcast$flag <- ""
  gtc_dcast[gtc_dcast$lowerconfint>1 | gtc_dcast$higherconfint<1,"flag"] <- "*"
  write.table(gtc_dcast,paste0(outputname, "_oddsratio.txt"),sep = "\t",col.names = T, row.names = F, quote = F)
  
  filename=paste0(outputname,"_oddsratio.pdf")
  pdf(file=filename, onefile=TRUE,height=h,width = w) 
  d1 <- ggplot(gtc_dcast,aes(x=oddsratio,y=Mutation))+geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") 
  d1 <- d1 + geom_errorbarh(aes(xmax = higherconfint, xmin = lowerconfint), size = .5, height = .2, color = "gray50")
  d1 <- d1 + geom_point(size = 3.5, color = "orange") +ylab("") +xlab("Odds Ratio")
  #  d1 <- d1 + coord_trans(x = scales:::exp_trans(10)) + scale_x_continuous(breaks = log10(seq(0.1, 2.5, 0.1)), labels = seq(0.1, 2.5, 0.1),
  #                   limits = log10(c(0.09,2.5)))
  d1 <- d1+theme(axis.title.x = element_text(size=15),
                 axis.title.y = element_text(size=15),
                 plot.title = element_text(size=10),
                 axis.text.x=element_text(angle=90, vjust=0.5,colour = "black"),
                 axis.text.y=element_text(colour = "black"),
                 panel.grid.minor.x=element_blank(),
                 panel.grid.major.x=element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.background = element_rect(fill = "white"),
                 panel.border = element_rect(colour = "black", fill=NA))
  d1 <- d1+facet_wrap(~Sample,ncol=colnum,scales="free")
  print(d1)
  dev.off()
  
  
}

