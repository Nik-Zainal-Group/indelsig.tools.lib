ch <- list()

ch[['type_3']] <- list(
  channels=c("[-C]LongRep_g4","[-C]NonRep","[-C]ShortRep_leq4",
             "[-T]LongRep_g4", "[-T]NonRep","[-T]ShortRep_leq4",
             "[+C]LongRep_g4","[+C]NonRep", "[+C]ShortRep_leq4",
             "[+T]LongRep_g4", "[+T]NonRep","[+T]ShortRep_leq4",
             "Complex",
             "Del_nMer_LongRep_g4","Del_nMer_ShortRep_leq4",
             "Del_NonRep",
             "Del_Spaced_long_g5","Del_Spaced_short_leq5",
             "Ins_nMer_LongRep_g4","Ins_nMer_ShortRep_leq4","Ins_NonRep"),
  types=c('[C]','[T]','[Com]','[nMer]','[NonRep]','[Spaced]')
  )

ch[['type_3']]$assigned_types <- ch[['type_3']]$types[c(1,1,1,2,2,2,1,1,1,2,2,2,3,
                                                            4,4,5,6,6,4,4,5)]





ch[['type_4']] <- list(
  channels=c(
              "[-C]Rep=1|A","[-C]Rep=1|G","[-C]Rep=1|T",
              "[-C]Rep=2|A","[-C]Rep=2|G","[-C]Rep=2|T",
              "[-C]Rep=3|A","[-C]Rep=3|G","[-C]Rep=3|T",

              "[-C]Rep_45|A","[-C]Rep_45|G","[-C]Rep_45|T",
              "[-C]Rep_6",


             "[+C]Rep_456","[+C]Rep_789","[+C]Rep_leq3",
             "A|[+C]Rep=0|A","A|[+C]Rep=0|T","T|[+C]Rep=0|T",


             "A|[-T]Rep_leq4|A","A|[-T]Rep_leq4|C","A|[-T]Rep_leq4|G",
             "A|[-T]Rep_567|A","A|[-T]Rep_567|C","A|[-T]Rep_567|G",
             "A|[-T]Rep_89|A","A|[-T]Rep_89|C","A|[-T]Rep_89|G",

             "A|[+T]Rep_leq4|A","A|[+T]Rep_leq4|C","A|[+T]Rep_leq4|G",
             "A|[+T]Rep_567|A","A|[+T]Rep_567|C","A|[+T]Rep_567|G",
             "A|[+T]Rep_89|A","A|[+T]Rep_89|C","A|[+T]Rep_89|G",

             "C|[-T]Rep_leq4|A","C|[-T]Rep_leq4|C","C|[-T]Rep_leq4|G",
             "C|[-T]Rep_567|A","C|[-T]Rep_567|C","C|[-T]Rep_567|G",
             "C|[-T]Rep_89|A","C|[-T]Rep_89|C","C|[-T]Rep_89|G",

             "C|[+T]Rep_leq4|A","C|[+T]Rep_leq4|C","C|[+T]Rep_leq4|G",
             "C|[+T]Rep_567|A","C|[+T]Rep_567|C","C|[+T]Rep_567|G",
             "C|[+T]Rep_89|A","C|[+T]Rep_89|C","C|[+T]Rep_89|G",


             "G|[-T]Rep_leq4|A","G|[-T]Rep_leq4|C","G|[-T]Rep_leq4|G",
             "G|[-T]Rep_567|A","G|[-T]Rep_567|C","G|[-T]Rep_567|G",
             "G|[-T]Rep_89|A","G|[-T]Rep_89|C","G|[-T]Rep_89|G",


             "G|[+T]Rep_leq4|A","G|[+T]Rep_leq4|C","G|[+T]Rep_leq4|G",
             "G|[+T]Rep_567|A","G|[+T]Rep_567|C","G|[+T]Rep_567|G",
             "G|[+T]Rep_89|A","G|[+T]Rep_89|C","G|[+T]Rep_89|G",


             "Del_nMer_R2","Del_nMer_R3","Del_nMer_R4","Del_nMer_R5",
             "Del_NonRep_L2","Del_NonRep_L3","Del_NonRep_L4","Del_NonRep_L5",
             "Del_Spaced_short_leq5_mh1","Del_Spaced_short_leq5_mh2","Del_Spaced_short_leq5_mh3","Del_Spaced_short_leq5_mh4",
             "Del_Spaced_long_g5_mh1","Del_Spaced_long_g5_mh2","Del_Spaced_long_g5_mh3","Del_Spaced_long_g5_mh4","Del_Spaced_long_g5_mh5","Del_Spaced_long_g5_mh6",


             "Ins_nMer_R_23","Ins_nMer_R4",
             "Ins_NonRep_L2","Ins_NonRep_L3","Ins_NonRep_L4","Ins_NonRep_L5",

             "Complex"),

  types=c('[C]RepShort','[C]RepMed','[C]RepLong','[C]Rep=0',
          '[T]RepShort|0','[T]RepMed','[T]RepLong',
          '[nMer]','[NonRep]','[Spaced_long]','[Spaced_short]','[Com]')

)

ch[['type_4']]$assigned_types=ch[['type_4']]$types[c(
  rep(1,9),

  rep(2,3),
  3,

  2,3,1,
  rep(4,3),

  rep(5,3),
  rep(6,3),
  rep(7,3),

  rep(5,3),
  rep(6,3),
  rep(7,3),

  rep(5,3),
  rep(6,3),
  rep(7,3),

  rep(5,3),
  rep(6,3),
  rep(7,3),

  rep(5,3),
  rep(6,3),
  rep(7,3),

  rep(5,3),
  rep(6,3),
  rep(7,3),

  rep(8,4),
  rep(9,4),
  rep(10,4),
  rep(11,6),

  rep(8,2),
  rep(9,4),
  12

)]

ch_type4_reorder <- unlist(lapply(ch$type_4$types,function(type){
                                v <- sort(ch$type_4$channels[ch$type_4$assigned_types==type])
                                names(v) <- rep(type,length(v))
                                return(v)
                              }))
ch[['type_4']]$channels <- unname(ch_type4_reorder)
ch[['type_4']]$assigned_types <- names(ch_type4_reorder)


ch[['type_4_m5']] <- list(
  channels = c(
    "A|[+C]Rep=0|A","A|[+C]Rep=0|T",
    "[+C]Rep_leq3","[+C]Rep_456","[+C]Rep_789",

    "A|[+T]Rep_leq4|A","A|[+T]Rep_leq4|C","A|[+T]Rep_leq4|G",
    "C|[+T]Rep_leq4|A","C|[+T]Rep_leq4|C","C|[+T]Rep_leq4|G",
    "G|[+T]Rep_leq4|A","G|[+T]Rep_leq4|C","G|[+T]Rep_leq4|G",

    "A|[+T]Rep_567|A","A|[+T]Rep_567|C","A|[+T]Rep_567|G",
    "C|[+T]Rep_567|A","C|[+T]Rep_567|C","C|[+T]Rep_567|G",
    "G|[+T]Rep_567|A","G|[+T]Rep_567|C","G|[+T]Rep_567|G",

    "A|[+T]Rep_89|A","A|[+T]Rep_89|C","A|[+T]Rep_89|G",
    "C|[+T]Rep_89|A","C|[+T]Rep_89|C","C|[+T]Rep_89|G",
    "G|[+T]Rep_89|A","G|[+T]Rep_89|C","G|[+T]Rep_89|G",


    "Ins_NonRep_R0_L234","Ins_NonRep_R0_L5","Ins_NonRep_R1_L234","Ins_NonRep_R1_L5",
    "Ins_nMer_R234","Ins_nMer_R5",

    "[-C]Rep=1|A","[-C]Rep=1|T",
    "[-C]Rep=2|A","[-C]Rep=2|T",
    "[-C]Rep=3|A","[-C]Rep=3|T",
    "[-C]Rep_45|A","[-C]Rep_45|T",
    "[-C]Rep_leq5|G",
    "[-C]Rep_6",

    "A|[-T]Rep_leq4|A","A|[-T]Rep_leq4|C","A|[-T]Rep_leq4|G",
    "C|[-T]Rep_leq4|A","C|[-T]Rep_leq4|C","C|[-T]Rep_leq4|G",
    "G|[-T]Rep_leq4|A","G|[-T]Rep_leq4|C","G|[-T]Rep_leq4|G",

    "A|[-T]Rep_567|A","A|[-T]Rep_567|C","A|[-T]Rep_567|G",
    "C|[-T]Rep_567|A","C|[-T]Rep_567|C","C|[-T]Rep_567|G",
    "G|[-T]Rep_567|A","G|[-T]Rep_567|C","G|[-T]Rep_567|G",

    "A|[-T]Rep_89|A","A|[-T]Rep_89|C","A|[-T]Rep_89|G",
    "C|[-T]Rep_89|A","C|[-T]Rep_89|C","C|[-T]Rep_89|G",
    "G|[-T]Rep_89|A","G|[-T]Rep_89|C","G|[-T]Rep_89|G",

    "Del_NonRep_L234","Del_NonRep_L5",
    "Del_nMer_U12_R234","Del_nMer_U12_R5","Del_nMer_U3_R2","Del_nMer_U3_R3",

    "Del_Spaced_short_leq5_mh1","Del_Spaced_short_leq5_mh2","Del_Spaced_short_leq5_mh3",
    "Del_Spaced_long_g5_mh1","Del_Spaced_long_g5_mh2","Del_Spaced_long_g5_mh3","Del_Spaced_long_g5_mh4",
    "Complex"),
  types=c('[-C]','[-T]','[+C]','[+T]',
          'Del_nMer','Del_NonRep','Del_Spaced','Ins_nMer',
          'Ins_NonRep','[Com]')
)

ch[['type_4_m5']]$assigned_types <- ch[['type_4_m5']]$types[c(
                                                            rep(3,5),
                                                            rep(4,27),
                                                            rep(9,4),
                                                            rep(8,2),
                                                            rep(1,10),
                                                            rep(2,27),
                                                            rep(6,2),
                                                            rep(5,4),
                                                            rep(7,7),
                                                            rep(10,1)
                                                          )]


## Final channel set used in manuscript
ch[['ch2']] <- list(channels= c( "A[Ins(C):R0]A"       ,   "A[Ins(C):R0]T"     ,     "Ins(C):R(0,3)"    ,      "Ins(C):R(4,6)"     ,     "Ins(C):R(7,9)"  ,
                                 "A[Ins(T):R(0,4)]A"   ,   "A[Ins(T):R(0,4)]C"  ,    "A[Ins(T):R(0,4)]G"  ,    "C[Ins(T):R(0,4)]A"  ,    "C[Ins(T):R(0,4)]C" ,
                                 "C[Ins(T):R(0,4)]G"  ,    "G[Ins(T):R(0,4)]A"  ,    "G[Ins(T):R(0,4)]C"  ,    "G[Ins(T):R(0,4)]G"  ,    "A[Ins(T):R(5,7)]A" ,
                                 "A[Ins(T):R(5,7)]C"  ,    "A[Ins(T):R(5,7)]G"  ,    "C[Ins(T):R(5,7)]A"  ,    "C[Ins(T):R(5,7)]C"  ,    "C[Ins(T):R(5,7)]G" ,
                                 "G[Ins(T):R(5,7)]A"  ,    "G[Ins(T):R(5,7)]C"  ,    "G[Ins(T):R(5,7)]G"   ,   "A[Ins(T):R(8,9)]A"   ,   "A[Ins(T):R(8,9)]C" ,
                                 "A[Ins(T):R(8,9)]G"  ,    "C[Ins(T):R(8,9)]A"  ,    "C[Ins(T):R(8,9)]C"  ,    "C[Ins(T):R(8,9)]G"  ,    "G[Ins(T):R(8,9)]A" ,
                                 "G[Ins(T):R(8,9)]C"  ,    "G[Ins(T):R(8,9)]G"  ,    "Ins(2,4):R0"       ,     "Ins(5,):R0"      ,       "Ins(2,4):R1"     ,
                                 "Ins(5,):R1"         ,    "Ins(2,):R(2,4)"   ,      "Ins(2,):R(5,9)"    ,     "[Del(C):R1]A"     ,      "[Del(C):R1]T"    ,
                                 "[Del(C):R2]A"      ,     "[Del(C):R2]T"       ,    "[Del(C):R3]A"      ,     "[Del(C):R3]T"      ,     "[Del(C):R(4,5)]A" ,
                                 "[Del(C):R(4,5)]T"   ,    "[Del(C):R(1,5)]G"   ,    "Del(C):R(6,9)"     ,     "A[Del(T):R(1,4)]A"  ,    "A[Del(T):R(1,4)]C" ,
                                 "A[Del(T):R(1,4)]G"  ,    "C[Del(T):R(1,4)]A"  ,    "C[Del(T):R(1,4)]C"   ,   "C[Del(T):R(1,4)]G"  ,    "G[Del(T):R(1,4)]A"  ,
                                 "G[Del(T):R(1,4)]C"  ,    "G[Del(T):R(1,4)]G"  ,    "A[Del(T):R(5,7)]A"  ,    "A[Del(T):R(5,7)]C"  ,    "A[Del(T):R(5,7)]G"  ,
                                 "C[Del(T):R(5,7)]A"  ,    "C[Del(T):R(5,7)]C"  ,    "C[Del(T):R(5,7)]G"  ,    "G[Del(T):R(5,7)]A"  ,    "G[Del(T):R(5,7)]C"  ,
                                 "G[Del(T):R(5,7)]G" ,     "A[Del(T):R(8,9)]A"  ,    "A[Del(T):R(8,9)]C"  ,    "A[Del(T):R(8,9)]G"   ,   "C[Del(T):R(8,9)]A"  ,
                                 "C[Del(T):R(8,9)]C"  ,    "C[Del(T):R(8,9)]G"  ,    "G[Del(T):R(8,9)]A"  ,    "G[Del(T):R(8,9)]C"   ,   "G[Del(T):R(8,9)]G"  ,
                                 "Del(2,4):R1"       ,     "Del(5,):R1"        ,     "Del(2,8):U(1,2):R(2,4)", "Del(2,):U(1,2):R(5,9)",  "Del(3,):U(3,):R2"  ,
                                 "Del(3,):U(3,):R(3,9)" ,  "Del(2,5):M1"      ,      "Del(3,5):M2"       ,     "Del(4,5):M(3,4)"    ,    "Del(6,):M1"     ,
                                 "Del(6,):M2"         ,    "Del(6,):M3"      ,       "Del(6,):M(4,)"     ,     "Complex"  ),
                    type=c("+1bp C",'+1bp T','+≥2bp','-1bp C','-1bp T','-≥2bp','-Mh','Complex')

                    )


ch[['ch2']]$channels_per_type<-c(5,27,6,10,27,6,7,1)
names(ch[['ch2']]$channels_per_type)<-ch[['ch2']]$type

ch[['ch2']]$assigned_type <- rep(ch[['ch2']][['type']],ch[['ch2']]$channels_per_type)


ch[['ch2']][['palette']] <- c(rgb(102,153,204,maxColorValue = 255),
                              rgb(238,204,101,maxColorValue = 255),
                              rgb(238,153,170,maxColorValue = 255),

                              rgb(0,68,136,maxColorValue = 255),
                              rgb(153,120,0,maxColorValue = 255),
                              rgb(238,51,119,maxColorValue = 255),
                              rgb(118,43,131,maxColorValue = 255),

                              rgb(0,0,0,maxColorValue = 255))

ch[['ch2']][['assigned_palette']] <-rep(ch[['ch2']][['palette']],ch[['ch2']]$channels_per_type)

ch[['ch2']][['type_color']] <-c('black','black','black','white','white','white','white','white')
ch[['ch2']][['type_alias']] <- c("1bp C",'1bp T','>=2bp','1bp C','1bp T','>=2bp','Mh','X')


##### CH2 + SBS channels

ch[['ch2_sbs']] <- list(
  channels=c('A[C>A]A','A[C>A]C','A[C>A]G','A[C>A]T','C[C>A]A','C[C>A]C','C[C>A]G','C[C>A]T','G[C>A]A','G[C>A]C','G[C>A]G','G[C>A]T','T[C>A]A','T[C>A]C','T[C>A]G','T[C>A]T','A[C>G]A','A[C>G]C','A[C>G]G','A[C>G]T','C[C>G]A','C[C>G]C','C[C>G]G','C[C>G]T','G[C>G]A','G[C>G]C','G[C>G]G','G[C>G]T','T[C>G]A','T[C>G]C','T[C>G]G','T[C>G]T','A[C>T]A','A[C>T]C','A[C>T]G','A[C>T]T','C[C>T]A','C[C>T]C','C[C>T]G','C[C>T]T','G[C>T]A','G[C>T]C','G[C>T]G','G[C>T]T','T[C>T]A','T[C>T]C','T[C>T]G','T[C>T]T','A[T>A]A','A[T>A]C','A[T>A]G','A[T>A]T','C[T>A]A','C[T>A]C','C[T>A]G','C[T>A]T','G[T>A]A','G[T>A]C','G[T>A]G','G[T>A]T','T[T>A]A','T[T>A]C','T[T>A]G','T[T>A]T','A[T>C]A','A[T>C]C','A[T>C]G','A[T>C]T','C[T>C]A','C[T>C]C','C[T>C]G','C[T>C]T','G[T>C]A','G[T>C]C','G[T>C]G','G[T>C]T','T[T>C]A','T[T>C]C','T[T>C]G','T[T>C]T','A[T>G]A','A[T>G]C','A[T>G]G','A[T>G]T','C[T>G]A','C[T>G]C','C[T>G]G','C[T>G]T','G[T>G]A','G[T>G]C','G[T>G]G','G[T>G]T','T[T>G]A','T[T>G]C','T[T>G]G','T[T>G]T','A[Ins(C):R0]A','A[Ins(C):R0]T','Ins(C):R(0,3)','Ins(C):R(4,6)','Ins(C):R(7,9)','A[Ins(T):R(0,4)]A','A[Ins(T):R(0,4)]C','A[Ins(T):R(0,4)]G','C[Ins(T):R(0,4)]A','C[Ins(T):R(0,4)]C','C[Ins(T):R(0,4)]G','G[Ins(T):R(0,4)]A','G[Ins(T):R(0,4)]C','G[Ins(T):R(0,4)]G','A[Ins(T):R(5,7)]A','A[Ins(T):R(5,7)]C','A[Ins(T):R(5,7)]G','C[Ins(T):R(5,7)]A','C[Ins(T):R(5,7)]C','C[Ins(T):R(5,7)]G','G[Ins(T):R(5,7)]A','G[Ins(T):R(5,7)]C','G[Ins(T):R(5,7)]G','A[Ins(T):R(8,9)]A','A[Ins(T):R(8,9)]C','A[Ins(T):R(8,9)]G','C[Ins(T):R(8,9)]A','C[Ins(T):R(8,9)]C','C[Ins(T):R(8,9)]G','G[Ins(T):R(8,9)]A','G[Ins(T):R(8,9)]C','G[Ins(T):R(8,9)]G','Ins(2,4):R0','Ins(5,):R0','Ins(2,4):R1','Ins(5,):R1','Ins(2,):R(2,4)','Ins(2,):R(5,9)','[Del(C):R1]A','[Del(C):R1]T','[Del(C):R2]A','[Del(C):R2]T','[Del(C):R3]A','[Del(C):R3]T','[Del(C):R(4,5)]A','[Del(C):R(4,5)]T','[Del(C):R(1,5)]G','Del(C):R(6,9)','A[Del(T):R(1,4)]A','A[Del(T):R(1,4)]C','A[Del(T):R(1,4)]G','C[Del(T):R(1,4)]A','C[Del(T):R(1,4)]C','C[Del(T):R(1,4)]G','G[Del(T):R(1,4)]A','G[Del(T):R(1,4)]C','G[Del(T):R(1,4)]G','A[Del(T):R(5,7)]A','A[Del(T):R(5,7)]C','A[Del(T):R(5,7)]G','C[Del(T):R(5,7)]A','C[Del(T):R(5,7)]C','C[Del(T):R(5,7)]G','G[Del(T):R(5,7)]A','G[Del(T):R(5,7)]C','G[Del(T):R(5,7)]G','A[Del(T):R(8,9)]A','A[Del(T):R(8,9)]C','A[Del(T):R(8,9)]G','C[Del(T):R(8,9)]A','C[Del(T):R(8,9)]C','C[Del(T):R(8,9)]G','G[Del(T):R(8,9)]A','G[Del(T):R(8,9)]C','G[Del(T):R(8,9)]G','Del(2,4):R1','Del(5,):R1','Del(2,8):U(1,2):R(2,4)','Del(2,):U(1,2):R(5,9)','Del(3,):U(3,):R2','Del(3,):U(3,):R(3,9)','Del(2,5):M1','Del(3,5):M2','Del(4,5):M(3,4)','Del(6,):M1','Del(6,):M2','Del(6,):M3','Del(6,):M(4,)','Complex'),
  type=c(
    'C>A','C>G','C>T','T>A','T>C','T>G',
    "+1bp C",'+1bp T','+≥2bp','-1bp C','-1bp T','-≥2bp','-Mh','Complex')
)




## Ch2 SBS channels
ch[['ch2_sbs']]$channels_per_type<-c(
  16,16,16,16,16,16,
  5,27,6,10,27,6,7,1)

names(ch[['ch2_sbs']]$channels_per_type)<-ch[['ch2_sbs']]$type


ch[['ch2_sbs']]$assigned_type <- rep(ch[['ch2_sbs']][['type']],ch[['ch2_sbs']]$channels_per_type)


ch[['ch2_sbs']][['palette']] <- c(


                              rgb(3,195,239,maxColorValue = 255),
                              rgb(0,0,0,maxColorValue = 255),
                              rgb(231,48,41,maxColorValue = 255),
                              rgb(201,200,200,maxColorValue = 255),
                              rgb(169,212,108,maxColorValue = 255),
                              rgb(238,205,204,maxColorValue = 255),


                              rgb(102,153,204,maxColorValue = 255),
                              rgb(238,204,101,maxColorValue = 255),
                              rgb(238,153,170,maxColorValue = 255),

                              rgb(0,68,136,maxColorValue = 255),
                              rgb(153,120,0,maxColorValue = 255),
                              rgb(238,51,119,maxColorValue = 255),
                              rgb(118,43,131,maxColorValue = 255),

                              rgb(0,0,0,maxColorValue = 255))

ch[['ch2_sbs']][['assigned_palette']] <-rep(ch[['ch2_sbs']][['palette']],ch[['ch2_sbs']]$channels_per_type)

ch[['ch2_sbs']][['type_color']] <-c(
  'white','white','white','black','black','black',
  'black','black','black','white','white','white','white','white')

ch[['ch2_sbs']][['type_alias']] <- c(
  'C>A','C>G','C>T','T>A','T>C','T>G',
  "1bp C",'1bp T','>=2bp','1bp C','1bp T','>=2bp','Mh','X')




#usethis::use_data(ch,overwrite = T)

#' Channel sets for indel signatures.
#' @docType data
#' @usage data(ch)
#' @format  An object of the type list
#' @examples
#' data(ch)
#"ch"
