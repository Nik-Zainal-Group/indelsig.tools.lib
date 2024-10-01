

indel_template_type_3 <- data.frame("IndelType"=c("[+C]NonRep","[+C]ShortRep_leq4","[+C]LongRep_g4","[+T]NonRep","[+T]ShortRep_leq4","[+T]LongRep_g4","Ins_NonRep","Ins_nMer_ShortRep_leq4","Ins_nMer_LongRep_g4",
                                                  "[-C]NonRep","[-C]ShortRep_leq4","[-C]LongRep_g4","[-T]NonRep","[-T]ShortRep_leq4","[-T]LongRep_g4","Del_NonRep","Del_nMer_ShortRep_leq4","Del_nMer_LongRep_g4","Del_Spaced_short_leq5","Del_Spaced_long_g5",
                                                  "Complex"),
                                    "Indel"=c("[+C]","[+C]","[+C]","[+T]","[+T]","[+T]","Ins_nMer","Ins_nMer","Ins_NonRep",
                                              "[-C]","[-C]","[-C]","[-T]","[-T]","[-T]","Del_nMer","Del_nMer","Del_NonRep","Del_Spaced","Del_Spaced",
                                              "Complex")
)



indel_template_type_4 <- read.table("./indel_template_type_4.txt", sep = "\t",header = T, as.is = T)
usethis::use_data(indel_template_type_4,overwrite = TRUE)

indel_type_4_figurelabel <- read.table("./indel_type_4_figurelabel.txt", sep = "\t",header = T, as.is = T)
usethis::use_data(indel_type_4_figurelabel,overwrite = TRUE)


indel_template_type_4_full <- read.table("./indel_template_type_4_full.txt", sep = "\t",header = T, as.is = T)
usethis::use_data(indel_template_type_4_full,overwrite = TRUE)


indel_template_type_4_full_figurelabel <- read.table("./indel_template_type_4_full_figurelabel.txt", sep = "\t",header = T, as.is = T)
usethis::use_data(indel_template_type_4_full_figurelabel,overwrite = TRUE)
