#' Plot indel profile in a extended channel bar plot for single sample
#' Show all x labels
#' @param muts_basis A indel catalogue of a single sample
#' @param text_size Size of text
#' @param plot_title Title of the plot
#' @return A extended channel indel profile plot
#' @export
gen_plot_catalougefull_single<- function(muts_basis,text_size,plot_title){
  #mean_parentmuts <- sum(muts_basis[,1:(dim(muts_basis)[2]-1)])/(dim(muts_basis)[2]-1)


  indel_template_type_4_full_figurelabel <- structure(list(IndelType = c("A[Ins(C):R0]A", "A[Ins(C):R0]G", 
"A[Ins(C):R0]T", "A[Ins(C):R1]A", "A[Ins(C):R1]G", "A[Ins(C):R1]T", 
"A[Ins(C):R2]A", "A[Ins(C):R2]G", "A[Ins(C):R2]T", "A[Ins(C):R3]A", 
"A[Ins(C):R3]G", "A[Ins(C):R3]T", "A[Ins(C):R4]A", "A[Ins(C):R4]G", 
"A[Ins(C):R4]T", "A[Ins(C):R5]A", "A[Ins(C):R5]G", "A[Ins(C):R5]T", 
"A[Ins(C):R6]A", "A[Ins(C):R6]G", "A[Ins(C):R6]T", "A[Ins(C):R7]A", 
"A[Ins(C):R7]G", "A[Ins(C):R7]T", "A[Ins(C):R8]A", "A[Ins(C):R8]G", 
"A[Ins(C):R8]T", "A[Ins(C):R9]A", "A[Ins(C):R9]G", "A[Ins(C):R9]T", 
"G[Ins(C):R0]A", "G[Ins(C):R0]G", "G[Ins(C):R0]T", "G[Ins(C):R1]A", 
"G[Ins(C):R1]G", "G[Ins(C):R1]T", "G[Ins(C):R2]A", "G[Ins(C):R2]G", 
"G[Ins(C):R2]T", "G[Ins(C):R3]A", "G[Ins(C):R3]G", "G[Ins(C):R3]T", 
"G[Ins(C):R4]A", "G[Ins(C):R4]G", "G[Ins(C):R4]T", "G[Ins(C):R5]A", 
"G[Ins(C):R5]G", "G[Ins(C):R5]T", "G[Ins(C):R6]A", "G[Ins(C):R6]G", 
"G[Ins(C):R6]T", "G[Ins(C):R7]A", "G[Ins(C):R7]G", "G[Ins(C):R7]T", 
"G[Ins(C):R8]A", "G[Ins(C):R8]G", "G[Ins(C):R8]T", "G[Ins(C):R9]A", 
"G[Ins(C):R9]G", "G[Ins(C):R9]T", "T[Ins(C):R0]A", "T[Ins(C):R0]G", 
"T[Ins(C):R0]T", "T[Ins(C):R1]A", "T[Ins(C):R1]G", "T[Ins(C):R1]T", 
"T[Ins(C):R2]A", "T[Ins(C):R2]G", "T[Ins(C):R2]T", "T[Ins(C):R3]A", 
"T[Ins(C):R3]G", "T[Ins(C):R3]T", "T[Ins(C):R4]A", "T[Ins(C):R4]G", 
"T[Ins(C):R4]T", "T[Ins(C):R5]A", "T[Ins(C):R5]G", "T[Ins(C):R5]T", 
"T[Ins(C):R6]A", "T[Ins(C):R6]G", "T[Ins(C):R6]T", "T[Ins(C):R7]A", 
"T[Ins(C):R7]G", "T[Ins(C):R7]T", "T[Ins(C):R8]A", "T[Ins(C):R8]G", 
"T[Ins(C):R8]T", "T[Ins(C):R9]A", "T[Ins(C):R9]G", "T[Ins(C):R9]T", 
"A[Ins(T):R0]A", "A[Ins(T):R0]C", "A[Ins(T):R0]G", "A[Ins(T):R1]A", 
"A[Ins(T):R1]C", "A[Ins(T):R1]G", "A[Ins(T):R2]A", "A[Ins(T):R2]C", 
"A[Ins(T):R2]G", "A[Ins(T):R3]A", "A[Ins(T):R3]C", "A[Ins(T):R3]G", 
"A[Ins(T):R4]A", "A[Ins(T):R4]C", "A[Ins(T):R4]G", "A[Ins(T):R5]A", 
"A[Ins(T):R5]C", "A[Ins(T):R5]G", "A[Ins(T):R6]A", "A[Ins(T):R6]C", 
"A[Ins(T):R6]G", "A[Ins(T):R7]A", "A[Ins(T):R7]C", "A[Ins(T):R7]G", 
"A[Ins(T):R8]A", "A[Ins(T):R8]C", "A[Ins(T):R8]G", "A[Ins(T):R9]A", 
"A[Ins(T):R9]C", "A[Ins(T):R9]G", "C[Ins(T):R0]A", "C[Ins(T):R0]C", 
"C[Ins(T):R0]G", "C[Ins(T):R1]A", "C[Ins(T):R1]C", "C[Ins(T):R1]G", 
"C[Ins(T):R2]A", "C[Ins(T):R2]C", "C[Ins(T):R2]G", "C[Ins(T):R3]A", 
"C[Ins(T):R3]C", "C[Ins(T):R3]G", "C[Ins(T):R4]A", "C[Ins(T):R4]C", 
"C[Ins(T):R4]G", "C[Ins(T):R5]A", "C[Ins(T):R5]C", "C[Ins(T):R5]G", 
"C[Ins(T):R6]A", "C[Ins(T):R6]C", "C[Ins(T):R6]G", "C[Ins(T):R7]A", 
"C[Ins(T):R7]C", "C[Ins(T):R7]G", "C[Ins(T):R8]A", "C[Ins(T):R8]C", 
"C[Ins(T):R8]G", "C[Ins(T):R9]A", "C[Ins(T):R9]C", "C[Ins(T):R9]G", 
"G[Ins(T):R0]A", "G[Ins(T):R0]C", "G[Ins(T):R0]G", "G[Ins(T):R1]A", 
"G[Ins(T):R1]C", "G[Ins(T):R1]G", "G[Ins(T):R2]A", "G[Ins(T):R2]C", 
"G[Ins(T):R2]G", "G[Ins(T):R3]A", "G[Ins(T):R3]C", "G[Ins(T):R3]G", 
"G[Ins(T):R4]A", "G[Ins(T):R4]C", "G[Ins(T):R4]G", "G[Ins(T):R5]A", 
"G[Ins(T):R5]C", "G[Ins(T):R5]G", "G[Ins(T):R6]A", "G[Ins(T):R6]C", 
"G[Ins(T):R6]G", "G[Ins(T):R7]A", "G[Ins(T):R7]C", "G[Ins(T):R7]G", 
"G[Ins(T):R8]A", "G[Ins(T):R8]C", "G[Ins(T):R8]G", "G[Ins(T):R9]A", 
"G[Ins(T):R9]C", "G[Ins(T):R9]G", "Ins(2,4):M", "Ins(5,):M", 
"Ins2:U1:R0", "Ins3:U1:R0", "Ins4:U1:R0", "Ins2:U2:R0", "Ins3:U3:R0", 
"Ins4:U2:R0", "Ins4:U4:R0", "Ins(5,):R0", "Ins2:U1:R1", "Ins2:U1:R2", 
"Ins2:U1:R3", "Ins2:U1:R4", "Ins2:U1:R(5,9)", "Ins2:U2:R1", "Ins2:U2:R2", 
"Ins2:U2:R3", "Ins2:U2:R4", "Ins2:U2:R(5,9)", "Ins3:U1:R1", "Ins3:U1:R2", 
"Ins3:U1:R3", "Ins3:U1:R4", "Ins3:U1:R(5,9)", "Ins3:U3:R1", "Ins3:U3:R2", 
"Ins3:U3:R3", "Ins3:U3:R4", "Ins3:U3:R(5,9)", "Ins4:U1:R1", "Ins4:U1:R2", 
"Ins4:U1:R3", "Ins4:U1:R4", "Ins4:U1:R(5,9)", "Ins4:U2:R1", "Ins4:U2:R2", 
"Ins4:U2:R3", "Ins4:U2:R4", "Ins4:U2:R(5,9)", "Ins4:U4:R1", "Ins4:U4:R2", 
"Ins4:U4:R3", "Ins4:U4:R4", "Ins4:U4:R(5,9)", "Ins(5,):U1:R1", 
"Ins(5,):U1:R2", "Ins(5,):U1:R3", "Ins(5,):U1:R4", "Ins(5,):U1:R(5,9)", 
"Ins(5,):U2:R1", "Ins(5,):U2:R2", "Ins(5,):U2:R3", "Ins(5,):U2:R4", 
"Ins(5,):U2:R(5,9)", "Ins(5,):U(3,):R1", "Ins(5,):U(3,):R2", 
"Ins(5,):U(3,):R3", "Ins(5,):U(3,):R4", "Ins(5,):U(3,):R(5,9)", 
"A[Del(C):R1]A", "A[Del(C):R1]G", "A[Del(C):R1]T", "A[Del(C):R2]A", 
"A[Del(C):R2]G", "A[Del(C):R2]T", "A[Del(C):R3]A", "A[Del(C):R3]G", 
"A[Del(C):R3]T", "A[Del(C):R4]A", "A[Del(C):R4]G", "A[Del(C):R4]T", 
"A[Del(C):R5]A", "A[Del(C):R5]G", "A[Del(C):R5]T", "A[Del(C):R6]A", 
"A[Del(C):R6]G", "A[Del(C):R6]T", "A[Del(C):R7]A", "A[Del(C):R7]G", 
"A[Del(C):R7]T", "A[Del(C):R8]A", "A[Del(C):R8]G", "A[Del(C):R8]T", 
"A[Del(C):R9]A", "A[Del(C):R9]G", "A[Del(C):R9]T", "G[Del(C):R1]A", 
"G[Del(C):R1]G", "G[Del(C):R1]T", "G[Del(C):R2]A", "G[Del(C):R2]G", 
"G[Del(C):R2]T", "G[Del(C):R3]A", "G[Del(C):R3]G", "G[Del(C):R3]T", 
"G[Del(C):R4]A", "G[Del(C):R4]G", "G[Del(C):R4]T", "G[Del(C):R5]A", 
"G[Del(C):R5]G", "G[Del(C):R5]T", "G[Del(C):R6]A", "G[Del(C):R6]G", 
"G[Del(C):R6]T", "G[Del(C):R7]A", "G[Del(C):R7]G", "G[Del(C):R7]T", 
"G[Del(C):R8]A", "G[Del(C):R8]G", "G[Del(C):R8]T", "G[Del(C):R9]A", 
"G[Del(C):R9]G", "G[Del(C):R9]T", "T[Del(C):R1]A", "T[Del(C):R1]G", 
"T[Del(C):R1]T", "T[Del(C):R2]A", "T[Del(C):R2]G", "T[Del(C):R2]T", 
"T[Del(C):R3]A", "T[Del(C):R3]G", "T[Del(C):R3]T", "T[Del(C):R4]A", 
"T[Del(C):R4]G", "T[Del(C):R4]T", "T[Del(C):R5]A", "T[Del(C):R5]G", 
"T[Del(C):R5]T", "T[Del(C):R6]A", "T[Del(C):R6]G", "T[Del(C):R6]T", 
"T[Del(C):R7]A", "T[Del(C):R7]G", "T[Del(C):R7]T", "T[Del(C):R8]A", 
"T[Del(C):R8]G", "T[Del(C):R8]T", "T[Del(C):R9]A", "T[Del(C):R9]G", 
"T[Del(C):R9]T", "A[Del(T):R1]A", "A[Del(T):R1]C", "A[Del(T):R1]G", 
"A[Del(T):R2]A", "A[Del(T):R2]C", "A[Del(T):R2]G", "A[Del(T):R3]A", 
"A[Del(T):R3]C", "A[Del(T):R3]G", "A[Del(T):R4]A", "A[Del(T):R4]C", 
"A[Del(T):R4]G", "A[Del(T):R5]A", "A[Del(T):R5]C", "A[Del(T):R5]G", 
"A[Del(T):R6]A", "A[Del(T):R6]C", "A[Del(T):R6]G", "A[Del(T):R7]A", 
"A[Del(T):R7]C", "A[Del(T):R7]G", "A[Del(T):R8]A", "A[Del(T):R8]C", 
"A[Del(T):R8]G", "A[Del(T):R9]A", "A[Del(T):R9]C", "A[Del(T):R9]G", 
"C[Del(T):R1]A", "C[Del(T):R1]C", "C[Del(T):R1]G", "C[Del(T):R2]A", 
"C[Del(T):R2]C", "C[Del(T):R2]G", "C[Del(T):R3]A", "C[Del(T):R3]C", 
"C[Del(T):R3]G", "C[Del(T):R4]A", "C[Del(T):R4]C", "C[Del(T):R4]G", 
"C[Del(T):R5]A", "C[Del(T):R5]C", "C[Del(T):R5]G", "C[Del(T):R6]A", 
"C[Del(T):R6]C", "C[Del(T):R6]G", "C[Del(T):R7]A", "C[Del(T):R7]C", 
"C[Del(T):R7]G", "C[Del(T):R8]A", "C[Del(T):R8]C", "C[Del(T):R8]G", 
"C[Del(T):R9]A", "C[Del(T):R9]C", "C[Del(T):R9]G", "G[Del(T):R1]A", 
"G[Del(T):R1]C", "G[Del(T):R1]G", "G[Del(T):R2]A", "G[Del(T):R2]C", 
"G[Del(T):R2]G", "G[Del(T):R3]A", "G[Del(T):R3]C", "G[Del(T):R3]G", 
"G[Del(T):R4]A", "G[Del(T):R4]C", "G[Del(T):R4]G", "G[Del(T):R5]A", 
"G[Del(T):R5]C", "G[Del(T):R5]G", "G[Del(T):R6]A", "G[Del(T):R6]C", 
"G[Del(T):R6]G", "G[Del(T):R7]A", "G[Del(T):R7]C", "G[Del(T):R7]G", 
"G[Del(T):R8]A", "G[Del(T):R8]C", "G[Del(T):R8]G", "G[Del(T):R9]A", 
"G[Del(T):R9]C", "G[Del(T):R9]G", "Del2:U1:R1", "Del3:U1:R1", 
"Del4:U1:R1", "Del5:U1:R1", "Del6:U1:R1", "Del7:U1:R1", "Del8:U1:R1", 
"Del9:U1:R1", "Del(10,):U1:R1", "Del2:U(2,):R1", "Del3:U(2,):R1", 
"Del4:U(2,):R1", "Del5:U(2,):R1", "Del6:U(2,):R1", "Del7:U(2,):R1", 
"Del8:U(2,):R1", "Del9:U(2,):R1", "Del(10,):U(2,):R1", "Del2:U1:R3", 
"Del2:U1:R4", "Del2:U1:R(5,9)", "Del2:U2:R2", "Del2:U2:R3", "Del2:U2:R4", 
"Del2:U2:R(5,9)", "Del3:U1:R4", "Del3:U1:R(5,9)", "Del3:U3:R2", 
"Del3:U3:R3", "Del3:U3:R4", "Del3:U3:R(5,9)", "Del4:U1:R(5,9)", 
"Del4:U2:R3", "Del4:U2:R4", "Del4:U2:R(5,9)", "Del4:U4:R2", "Del4:U4:R3", 
"Del4:U4:R4", "Del4:U4:R(5,9)", "Del5:U1:R(6,9)", "Del5:U5:R2", 
"Del5:U5:R3", "Del5:U5:R4", "Del5:U5:R(5,9)", "Del(6,):U1:R(7,9)", 
"Del(6,):U2:R(4,9)", "Del(6,):U3:R(3,9)", "Del(6,):U(4,):R(2,9)", 
"Del2:M1", "Del3:M1", "Del3:M2", "Del4:M1", "Del4:M2", "Del4:M3", 
"Del5:M1", "Del5:M2", "Del5:M3", "Del5:M4", "Del6:M1", "Del6:M2", 
"Del6:M3", "Del6:M4", "Del6:M5", "Del(7,):M1", "Del(7,):M2", 
"Del(7,):M3", "Del(7,):M4", "Del(7,):M5", "Del(7,):M(6,)", "Complex(0,1)", 
"Complex(2,5)", "Complex(6,10)", "Complex(11,20)", "Complex(21,)"
), Indel = c("Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Complex", "Complex", "Complex", 
"Complex", "Complex"), Indel3 = c("Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Complex", "Complex", "Complex", "Complex", "Complex"), Figlabel = c("A[Ins(C):R0]A", 
"A[Ins(C):R0]G", "A[Ins(C):R0]T", "A[Ins(C):R1]A", "A[Ins(C):R1]G", 
"A[Ins(C):R1]T", "A[Ins(C):R2]A", "A[Ins(C):R2]G", "A[Ins(C):R2]T", 
"A[Ins(C):R3]A", "A[Ins(C):R3]G", "A[Ins(C):R3]T", "A[Ins(C):R4]A", 
"A[Ins(C):R4]G", "A[Ins(C):R4]T", "A[Ins(C):R5]A", "A[Ins(C):R5]G", 
"A[Ins(C):R5]T", "A[Ins(C):R6]A", "A[Ins(C):R6]G", "A[Ins(C):R6]T", 
"A[Ins(C):R7]A", "A[Ins(C):R7]G", "A[Ins(C):R7]T", "A[Ins(C):R8]A", 
"A[Ins(C):R8]G", "A[Ins(C):R8]T", "A[Ins(C):R9]A", "A[Ins(C):R9]G", 
"A[Ins(C):R9]T", "G[Ins(C):R0]A", "G[Ins(C):R0]G", "G[Ins(C):R0]T", 
"G[Ins(C):R1]A", "G[Ins(C):R1]G", "G[Ins(C):R1]T", "G[Ins(C):R2]A", 
"G[Ins(C):R2]G", "G[Ins(C):R2]T", "G[Ins(C):R3]A", "G[Ins(C):R3]G", 
"G[Ins(C):R3]T", "G[Ins(C):R4]A", "G[Ins(C):R4]G", "G[Ins(C):R4]T", 
"G[Ins(C):R5]A", "G[Ins(C):R5]G", "G[Ins(C):R5]T", "G[Ins(C):R6]A", 
"G[Ins(C):R6]G", "G[Ins(C):R6]T", "G[Ins(C):R7]A", "G[Ins(C):R7]G", 
"G[Ins(C):R7]T", "G[Ins(C):R8]A", "G[Ins(C):R8]G", "G[Ins(C):R8]T", 
"G[Ins(C):R9]A", "G[Ins(C):R9]G", "G[Ins(C):R9]T", "T[Ins(C):R0]A", 
"T[Ins(C):R0]G", "T[Ins(C):R0]T", "T[Ins(C):R1]A", "T[Ins(C):R1]G", 
"T[Ins(C):R1]T", "T[Ins(C):R2]A", "T[Ins(C):R2]G", "T[Ins(C):R2]T", 
"T[Ins(C):R3]A", "T[Ins(C):R3]G", "T[Ins(C):R3]T", "T[Ins(C):R4]A", 
"T[Ins(C):R4]G", "T[Ins(C):R4]T", "T[Ins(C):R5]A", "T[Ins(C):R5]G", 
"T[Ins(C):R5]T", "T[Ins(C):R6]A", "T[Ins(C):R6]G", "T[Ins(C):R6]T", 
"T[Ins(C):R7]A", "T[Ins(C):R7]G", "T[Ins(C):R7]T", "T[Ins(C):R8]A", 
"T[Ins(C):R8]G", "T[Ins(C):R8]T", "T[Ins(C):R9]A", "T[Ins(C):R9]G", 
"T[Ins(C):R9]T", "A[Ins(T):R0]A", "A[Ins(T):R0]C", "A[Ins(T):R0]G", 
"A[Ins(T):R1]A", "A[Ins(T):R1]C", "A[Ins(T):R1]G", "A[Ins(T):R2]A", 
"A[Ins(T):R2]C", "A[Ins(T):R2]G", "A[Ins(T):R3]A", "A[Ins(T):R3]C", 
"A[Ins(T):R3]G", "A[Ins(T):R4]A", "A[Ins(T):R4]C", "A[Ins(T):R4]G", 
"A[Ins(T):R5]A", "A[Ins(T):R5]C", "A[Ins(T):R5]G", "A[Ins(T):R6]A", 
"A[Ins(T):R6]C", "A[Ins(T):R6]G", "A[Ins(T):R7]A", "A[Ins(T):R7]C", 
"A[Ins(T):R7]G", "A[Ins(T):R8]A", "A[Ins(T):R8]C", "A[Ins(T):R8]G", 
"A[Ins(T):R9]A", "A[Ins(T):R9]C", "A[Ins(T):R9]G", "C[Ins(T):R0]A", 
"C[Ins(T):R0]C", "C[Ins(T):R0]G", "C[Ins(T):R1]A", "C[Ins(T):R1]C", 
"C[Ins(T):R1]G", "C[Ins(T):R2]A", "C[Ins(T):R2]C", "C[Ins(T):R2]G", 
"C[Ins(T):R3]A", "C[Ins(T):R3]C", "C[Ins(T):R3]G", "C[Ins(T):R4]A", 
"C[Ins(T):R4]C", "C[Ins(T):R4]G", "C[Ins(T):R5]A", "C[Ins(T):R5]C", 
"C[Ins(T):R5]G", "C[Ins(T):R6]A", "C[Ins(T):R6]C", "C[Ins(T):R6]G", 
"C[Ins(T):R7]A", "C[Ins(T):R7]C", "C[Ins(T):R7]G", "C[Ins(T):R8]A", 
"C[Ins(T):R8]C", "C[Ins(T):R8]G", "C[Ins(T):R9]A", "C[Ins(T):R9]C", 
"C[Ins(T):R9]G", "G[Ins(T):R0]A", "G[Ins(T):R0]C", "G[Ins(T):R0]G", 
"G[Ins(T):R1]A", "G[Ins(T):R1]C", "G[Ins(T):R1]G", "G[Ins(T):R2]A", 
"G[Ins(T):R2]C", "G[Ins(T):R2]G", "G[Ins(T):R3]A", "G[Ins(T):R3]C", 
"G[Ins(T):R3]G", "G[Ins(T):R4]A", "G[Ins(T):R4]C", "G[Ins(T):R4]G", 
"G[Ins(T):R5]A", "G[Ins(T):R5]C", "G[Ins(T):R5]G", "G[Ins(T):R6]A", 
"G[Ins(T):R6]C", "G[Ins(T):R6]G", "G[Ins(T):R7]A", "G[Ins(T):R7]C", 
"G[Ins(T):R7]G", "G[Ins(T):R8]A", "G[Ins(T):R8]C", "G[Ins(T):R8]G", 
"G[Ins(T):R9]A", "G[Ins(T):R9]C", "G[Ins(T):R9]G", "Ins(2,4):M", 
"Ins(5,):M", "Ins2:U1:R0", "Ins3:U1:R0", "Ins4:U1:R0", "Ins2:U2:R0", 
"Ins3:U3:R0", "Ins4:U2:R0", "Ins4:U4:R0", "Ins(5,):R0", "Ins2:U1:R1", 
"Ins2:U1:R2", "Ins2:U1:R3", "Ins2:U1:R4", "Ins2:U1:R(5,9)", "Ins2:U2:R1", 
"Ins2:U2:R2", "Ins2:U2:R3", "Ins2:U2:R4", "Ins2:U2:R(5,9)", "Ins3:U1:R1", 
"Ins3:U1:R2", "Ins3:U1:R3", "Ins3:U1:R4", "Ins3:U1:R(5,9)", "Ins3:U3:R1", 
"Ins3:U3:R2", "Ins3:U3:R3", "Ins3:U3:R4", "Ins3:U3:R(5,9)", "Ins4:U1:R1", 
"Ins4:U1:R2", "Ins4:U1:R3", "Ins4:U1:R4", "Ins4:U1:R(5,9)", "Ins4:U2:R1", 
"Ins4:U2:R2", "Ins4:U2:R3", "Ins4:U2:R4", "Ins4:U2:R(5,9)", "Ins4:U4:R1", 
"Ins4:U4:R2", "Ins4:U4:R3", "Ins4:U4:R4", "Ins4:U4:R(5,9)", "Ins(5,):U1:R1", 
"Ins(5,):U1:R2", "Ins(5,):U1:R3", "Ins(5,):U1:R4", "Ins(5,):U1:R(5,9)", 
"Ins(5,):U2:R1", "Ins(5,):U2:R2", "Ins(5,):U2:R3", "Ins(5,):U2:R4", 
"Ins(5,):U2:R(5,9)", "Ins(5,):U(3,):R1", "Ins(5,):U(3,):R2", 
"Ins(5,):U(3,):R3", "Ins(5,):U(3,):R4", "Ins(5,):U(3,):R(5,9)", 
"A[Del(C):R1]A", "A[Del(C):R1]G", "A[Del(C):R1]T", "A[Del(C):R2]A", 
"A[Del(C):R2]G", "A[Del(C):R2]T", "A[Del(C):R3]A", "A[Del(C):R3]G", 
"A[Del(C):R3]T", "A[Del(C):R4]A", "A[Del(C):R4]G", "A[Del(C):R4]T", 
"A[Del(C):R5]A", "A[Del(C):R5]G", "A[Del(C):R5]T", "A[Del(C):R6]A", 
"A[Del(C):R6]G", "A[Del(C):R6]T", "A[Del(C):R7]A", "A[Del(C):R7]G", 
"A[Del(C):R7]T", "A[Del(C):R8]A", "A[Del(C):R8]G", "A[Del(C):R8]T", 
"A[Del(C):R9]A", "A[Del(C):R9]G", "A[Del(C):R9]T", "G[Del(C):R1]A", 
"G[Del(C):R1]G", "G[Del(C):R1]T", "G[Del(C):R2]A", "G[Del(C):R2]G", 
"G[Del(C):R2]T", "G[Del(C):R3]A", "G[Del(C):R3]G", "G[Del(C):R3]T", 
"G[Del(C):R4]A", "G[Del(C):R4]G", "G[Del(C):R4]T", "G[Del(C):R5]A", 
"G[Del(C):R5]G", "G[Del(C):R5]T", "G[Del(C):R6]A", "G[Del(C):R6]G", 
"G[Del(C):R6]T", "G[Del(C):R7]A", "G[Del(C):R7]G", "G[Del(C):R7]T", 
"G[Del(C):R8]A", "G[Del(C):R8]G", "G[Del(C):R8]T", "G[Del(C):R9]A", 
"G[Del(C):R9]G", "G[Del(C):R9]T", "T[Del(C):R1]A", "T[Del(C):R1]G", 
"T[Del(C):R1]T", "T[Del(C):R2]A", "T[Del(C):R2]G", "T[Del(C):R2]T", 
"T[Del(C):R3]A", "T[Del(C):R3]G", "T[Del(C):R3]T", "T[Del(C):R4]A", 
"T[Del(C):R4]G", "T[Del(C):R4]T", "T[Del(C):R5]A", "T[Del(C):R5]G", 
"T[Del(C):R5]T", "T[Del(C):R6]A", "T[Del(C):R6]G", "T[Del(C):R6]T", 
"T[Del(C):R7]A", "T[Del(C):R7]G", "T[Del(C):R7]T", "T[Del(C):R8]A", 
"T[Del(C):R8]G", "T[Del(C):R8]T", "T[Del(C):R9]A", "T[Del(C):R9]G", 
"T[Del(C):R9]T", "A[Del(T):R1]A", "A[Del(T):R1]C", "A[Del(T):R1]G", 
"A[Del(T):R2]A", "A[Del(T):R2]C", "A[Del(T):R2]G", "A[Del(T):R3]A", 
"A[Del(T):R3]C", "A[Del(T):R3]G", "A[Del(T):R4]A", "A[Del(T):R4]C", 
"A[Del(T):R4]G", "A[Del(T):R5]A", "A[Del(T):R5]C", "A[Del(T):R5]G", 
"A[Del(T):R6]A", "A[Del(T):R6]C", "A[Del(T):R6]G", "A[Del(T):R7]A", 
"A[Del(T):R7]C", "A[Del(T):R7]G", "A[Del(T):R8]A", "A[Del(T):R8]C", 
"A[Del(T):R8]G", "A[Del(T):R9]A", "A[Del(T):R9]C", "A[Del(T):R9]G", 
"C[Del(T):R1]A", "C[Del(T):R1]C", "C[Del(T):R1]G", "C[Del(T):R2]A", 
"C[Del(T):R2]C", "C[Del(T):R2]G", "C[Del(T):R3]A", "C[Del(T):R3]C", 
"C[Del(T):R3]G", "C[Del(T):R4]A", "C[Del(T):R4]C", "C[Del(T):R4]G", 
"C[Del(T):R5]A", "C[Del(T):R5]C", "C[Del(T):R5]G", "C[Del(T):R6]A", 
"C[Del(T):R6]C", "C[Del(T):R6]G", "C[Del(T):R7]A", "C[Del(T):R7]C", 
"C[Del(T):R7]G", "C[Del(T):R8]A", "C[Del(T):R8]C", "C[Del(T):R8]G", 
"C[Del(T):R9]A", "C[Del(T):R9]C", "C[Del(T):R9]G", "G[Del(T):R1]A", 
"G[Del(T):R1]C", "G[Del(T):R1]G", "G[Del(T):R2]A", "G[Del(T):R2]C", 
"G[Del(T):R2]G", "G[Del(T):R3]A", "G[Del(T):R3]C", "G[Del(T):R3]G", 
"G[Del(T):R4]A", "G[Del(T):R4]C", "G[Del(T):R4]G", "G[Del(T):R5]A", 
"G[Del(T):R5]C", "G[Del(T):R5]G", "G[Del(T):R6]A", "G[Del(T):R6]C", 
"G[Del(T):R6]G", "G[Del(T):R7]A", "G[Del(T):R7]C", "G[Del(T):R7]G", 
"G[Del(T):R8]A", "G[Del(T):R8]C", "G[Del(T):R8]G", "G[Del(T):R9]A", 
"G[Del(T):R9]C", "G[Del(T):R9]G", "Del2:U1:R1", "Del3:U1:R1", 
"Del4:U1:R1", "Del5:U1:R1", "Del6:U1:R1", "Del7:U1:R1", "Del8:U1:R1", 
"Del9:U1:R1", "Del(10,):U1:R1", "Del2:U(2,):R1", "Del3:U(2,):R1", 
"Del4:U(2,):R1", "Del5:U(2,):R1", "Del6:U(2,):R1", "Del7:U(2,):R1", 
"Del8:U(2,):R1", "Del9:U(2,):R1", "Del(10,):U(2,):R1", "Del2:U1:R3", 
"Del2:U1:R4", "Del2:U1:R(5,9)", "Del2:U2:R2", "Del2:U2:R3", "Del2:U2:R4", 
"Del2:U2:R(5,9)", "Del3:U1:R4", "Del3:U1:R(5,9)", "Del3:U3:R2", 
"Del3:U3:R3", "Del3:U3:R4", "Del3:U3:R(5,9)", "Del4:U1:R(5,9)", 
"Del4:U2:R3", "Del4:U2:R4", "Del4:U2:R(5,9)", "Del4:U4:R2", "Del4:U4:R3", 
"Del4:U4:R4", "Del4:U4:R(5,9)", "Del5:U1:R(6,9)", "Del5:U5:R2", 
"Del5:U5:R3", "Del5:U5:R4", "Del5:U5:R(5,9)", "Del(6,):U1:R(7,9)", 
"Del(6,):U2:R(4,9)", "Del(6,):U3:R(3,9)", "Del(6,):U(4,):R(2,9)", 
"Del2:M1", "Del3:M1", "Del3:M2", "Del4:M1", "Del4:M2", "Del4:M3", 
"Del5:M1", "Del5:M2", "Del5:M3", "Del5:M4", "Del6:M1", "Del6:M2", 
"Del6:M3", "Del6:M4", "Del6:M5", "Del(7,):M1", "Del(7,):M2", 
"Del(7,):M3", "Del(7,):M4", "Del(7,):M5", "Del(7,):M(6,)", "Complex(0,1)", 
"Complex(2,5)", "Complex(6,10)", "Complex(11,20)", "Complex(21,)"
)), class = "data.frame", row.names = c(NA, -476L))


  muts_basis_melt <- reshape2::melt(muts_basis,"IndelType")

  muts_basis_melt <- merge(indel_template_type_4_full_figurelabel, muts_basis_melt,by="IndelType",all.x=T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType","Indel","Indel3","Figlabel","Sample","freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)

  indel_mypalette_fill <- c("#000000", # FEABB9 Complex
                            "#762A83", # Del(2,):M(1,)
                            "#EE3377", # Del(2,):R(0,9) #EE6677
                            "#004488", # Del(C)
                            "#997700", # Del(T)
                            "#EE99AA", #"#668D3C"  Ins(2,)
                            "#6699CC", #"#007996"  Ins(C)
                            "#EECC66") # Ins(T)

  indel_positions <- indel_template_type_4_full_figurelabel$IndelType
  indel_positions_labels <- indel_template_type_4_full_figurelabel$Figlabel

  # color blocks for indel bases

  entry <- table(indel_template_type_4_full_figurelabel$Indel)
  order_entry <- c("Ins(C)", "Ins(T)", "Ins(2,):R(0,9)", "Del(C)", "Del(T)", "Del(2,):R(1,9)", "Del(2,):M(1,)", "Complex")
  entry <- entry[order_entry]
  blocks <- data.frame(Type=unique(indel_template_type_4_full_figurelabel$Indel),
                       fill=indel_mypalette_fill,
                       xmin=c(0,cumsum(entry)[-length(entry)])+0.5,
                       xmax=cumsum(entry)+0.5)
  blocks$ymin <- max(muts_basis_melt$freq)*1.08#
  blocks$ymax <- max(muts_basis_melt$freq)*1.2
  blocks$labels <-c("1bp C", "1bp T", ">=2bp", "1bp C", "1bp T", ">=2bp", "Mh", "X")
  blocks$cl <-c("black", "black", "black", "white", "white", "white", "white",  "white")

  # grey blocks for insertion or deletion or complex
  indel_mypalette_fill3 <- c("#000000", # FEABB9 Complex
                             "#888888", # Deletion
                             "#DDDDDD") # Insertion

  entry3 <- table(indel_template_type_4_full_figurelabel$Indel3)
  order_entry3 <- c("Insertion", "Deletion", "Complex")
  entry3 <- entry3[order_entry3]
  blocks3 <- data.frame(Type=unique(indel_template_type_4_full_figurelabel$Indel3),
                        fill=indel_mypalette_fill3,
                        xmin=c(0,cumsum(entry3)[-length(entry3)])+0.5,
                        xmax=cumsum(entry3)+0.5)
  blocks3$ymin <- max(muts_basis_melt$freq)*1.2#
  blocks3$ymax <- max(muts_basis_melt$freq)*1.32
  blocks3$labels <-c("Insertion", "Deletion", "X")
  blocks3$cl <-c("black", "white",  "white")

  indel_mypalette_fill_all <- c("#000000", # FEABB9 Complex

                                "#762A83", # Del(2,):M(1,)
                                "#EE3377", # Del(2,):R(0,9) #EE6677
                                "#004488", # Del(C)
                                "#997700", # Del(T)
                                "#888888", # Deletion

                                "#EE99AA", #"#668D3C"  Ins(2,)
                                "#6699CC", #"#007996"  Ins(C)
                                "#EECC66", # Ins(T)
                                "#DDDDDD") # Insertion


  p <- ggplot2::ggplot(data=muts_basis_melt, ggplot2::aes(x=IndelType, y=freq,fill=Indel))+ ggplot2::geom_bar(stat="identity",position="dodge", width=.7)+ggplot2::xlab("Indel Types")+ggplot2::ylab("Count")
  #  p <- p+scale_y_continuous(limits=c(0,40),breaks=(seq(0,40,10)))
  p <- p+ggplot2::scale_x_discrete(limits = indel_positions,labels = indel_positions_labels)+ ggplot2::ggtitle(plot_title)
  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill_all)+ggplot2::coord_cartesian(ylim=c(0,unique(blocks3$ymax)), expand = FALSE)
  p <- p+ggplot2::theme_classic()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, size=5,colour = "black",hjust=1),
                                                 axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                                                 #    axis.line.y=element_blank(),
                                                 legend.position = "none",
                                                 axis.title.x = ggplot2::element_text(size=15),
                                                 axis.title.y = ggplot2::element_text(size=15))

  ## Add the overhead blocks for insertion or deletion or complex
  p <- p+ggplot2::geom_rect(data = blocks3, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    # geom_text(data=blocks,aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels),size=text_size,inherit.aes = F,colour="white")
    ggplot2::geom_text(data=blocks3,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)+ggplot2::scale_colour_manual(values=c("black", "white"))

  ## Add the overhead blocks for indel bases
  p <- p+ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    ggplot2::geom_text(data=blocks,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)

  return(p)


  # return(muts_basis)
}

#' Plot indel profile in a extended channel bar plot for single sample
#' Not show X labels
#' @param muts_basis A indel catalogue of a single sample
#' @param text_size Size of text
#' @param plot_title Title of the plot
#' @return A extended channel indel profile plot
#' @export
gen_plot_catalougefull_single_noXlabel<- function(muts_basis,text_size,plot_title){
  #mean_parentmuts <- sum(muts_basis[,1:(dim(muts_basis)[2]-1)])/(dim(muts_basis)[2]-1)
  indel_template_type_4_full_figurelabel <- structure(list(IndelType = c("A[Ins(C):R0]A", "A[Ins(C):R0]G", 
"A[Ins(C):R0]T", "A[Ins(C):R1]A", "A[Ins(C):R1]G", "A[Ins(C):R1]T", 
"A[Ins(C):R2]A", "A[Ins(C):R2]G", "A[Ins(C):R2]T", "A[Ins(C):R3]A", 
"A[Ins(C):R3]G", "A[Ins(C):R3]T", "A[Ins(C):R4]A", "A[Ins(C):R4]G", 
"A[Ins(C):R4]T", "A[Ins(C):R5]A", "A[Ins(C):R5]G", "A[Ins(C):R5]T", 
"A[Ins(C):R6]A", "A[Ins(C):R6]G", "A[Ins(C):R6]T", "A[Ins(C):R7]A", 
"A[Ins(C):R7]G", "A[Ins(C):R7]T", "A[Ins(C):R8]A", "A[Ins(C):R8]G", 
"A[Ins(C):R8]T", "A[Ins(C):R9]A", "A[Ins(C):R9]G", "A[Ins(C):R9]T", 
"G[Ins(C):R0]A", "G[Ins(C):R0]G", "G[Ins(C):R0]T", "G[Ins(C):R1]A", 
"G[Ins(C):R1]G", "G[Ins(C):R1]T", "G[Ins(C):R2]A", "G[Ins(C):R2]G", 
"G[Ins(C):R2]T", "G[Ins(C):R3]A", "G[Ins(C):R3]G", "G[Ins(C):R3]T", 
"G[Ins(C):R4]A", "G[Ins(C):R4]G", "G[Ins(C):R4]T", "G[Ins(C):R5]A", 
"G[Ins(C):R5]G", "G[Ins(C):R5]T", "G[Ins(C):R6]A", "G[Ins(C):R6]G", 
"G[Ins(C):R6]T", "G[Ins(C):R7]A", "G[Ins(C):R7]G", "G[Ins(C):R7]T", 
"G[Ins(C):R8]A", "G[Ins(C):R8]G", "G[Ins(C):R8]T", "G[Ins(C):R9]A", 
"G[Ins(C):R9]G", "G[Ins(C):R9]T", "T[Ins(C):R0]A", "T[Ins(C):R0]G", 
"T[Ins(C):R0]T", "T[Ins(C):R1]A", "T[Ins(C):R1]G", "T[Ins(C):R1]T", 
"T[Ins(C):R2]A", "T[Ins(C):R2]G", "T[Ins(C):R2]T", "T[Ins(C):R3]A", 
"T[Ins(C):R3]G", "T[Ins(C):R3]T", "T[Ins(C):R4]A", "T[Ins(C):R4]G", 
"T[Ins(C):R4]T", "T[Ins(C):R5]A", "T[Ins(C):R5]G", "T[Ins(C):R5]T", 
"T[Ins(C):R6]A", "T[Ins(C):R6]G", "T[Ins(C):R6]T", "T[Ins(C):R7]A", 
"T[Ins(C):R7]G", "T[Ins(C):R7]T", "T[Ins(C):R8]A", "T[Ins(C):R8]G", 
"T[Ins(C):R8]T", "T[Ins(C):R9]A", "T[Ins(C):R9]G", "T[Ins(C):R9]T", 
"A[Ins(T):R0]A", "A[Ins(T):R0]C", "A[Ins(T):R0]G", "A[Ins(T):R1]A", 
"A[Ins(T):R1]C", "A[Ins(T):R1]G", "A[Ins(T):R2]A", "A[Ins(T):R2]C", 
"A[Ins(T):R2]G", "A[Ins(T):R3]A", "A[Ins(T):R3]C", "A[Ins(T):R3]G", 
"A[Ins(T):R4]A", "A[Ins(T):R4]C", "A[Ins(T):R4]G", "A[Ins(T):R5]A", 
"A[Ins(T):R5]C", "A[Ins(T):R5]G", "A[Ins(T):R6]A", "A[Ins(T):R6]C", 
"A[Ins(T):R6]G", "A[Ins(T):R7]A", "A[Ins(T):R7]C", "A[Ins(T):R7]G", 
"A[Ins(T):R8]A", "A[Ins(T):R8]C", "A[Ins(T):R8]G", "A[Ins(T):R9]A", 
"A[Ins(T):R9]C", "A[Ins(T):R9]G", "C[Ins(T):R0]A", "C[Ins(T):R0]C", 
"C[Ins(T):R0]G", "C[Ins(T):R1]A", "C[Ins(T):R1]C", "C[Ins(T):R1]G", 
"C[Ins(T):R2]A", "C[Ins(T):R2]C", "C[Ins(T):R2]G", "C[Ins(T):R3]A", 
"C[Ins(T):R3]C", "C[Ins(T):R3]G", "C[Ins(T):R4]A", "C[Ins(T):R4]C", 
"C[Ins(T):R4]G", "C[Ins(T):R5]A", "C[Ins(T):R5]C", "C[Ins(T):R5]G", 
"C[Ins(T):R6]A", "C[Ins(T):R6]C", "C[Ins(T):R6]G", "C[Ins(T):R7]A", 
"C[Ins(T):R7]C", "C[Ins(T):R7]G", "C[Ins(T):R8]A", "C[Ins(T):R8]C", 
"C[Ins(T):R8]G", "C[Ins(T):R9]A", "C[Ins(T):R9]C", "C[Ins(T):R9]G", 
"G[Ins(T):R0]A", "G[Ins(T):R0]C", "G[Ins(T):R0]G", "G[Ins(T):R1]A", 
"G[Ins(T):R1]C", "G[Ins(T):R1]G", "G[Ins(T):R2]A", "G[Ins(T):R2]C", 
"G[Ins(T):R2]G", "G[Ins(T):R3]A", "G[Ins(T):R3]C", "G[Ins(T):R3]G", 
"G[Ins(T):R4]A", "G[Ins(T):R4]C", "G[Ins(T):R4]G", "G[Ins(T):R5]A", 
"G[Ins(T):R5]C", "G[Ins(T):R5]G", "G[Ins(T):R6]A", "G[Ins(T):R6]C", 
"G[Ins(T):R6]G", "G[Ins(T):R7]A", "G[Ins(T):R7]C", "G[Ins(T):R7]G", 
"G[Ins(T):R8]A", "G[Ins(T):R8]C", "G[Ins(T):R8]G", "G[Ins(T):R9]A", 
"G[Ins(T):R9]C", "G[Ins(T):R9]G", "Ins(2,4):M", "Ins(5,):M", 
"Ins2:U1:R0", "Ins3:U1:R0", "Ins4:U1:R0", "Ins2:U2:R0", "Ins3:U3:R0", 
"Ins4:U2:R0", "Ins4:U4:R0", "Ins(5,):R0", "Ins2:U1:R1", "Ins2:U1:R2", 
"Ins2:U1:R3", "Ins2:U1:R4", "Ins2:U1:R(5,9)", "Ins2:U2:R1", "Ins2:U2:R2", 
"Ins2:U2:R3", "Ins2:U2:R4", "Ins2:U2:R(5,9)", "Ins3:U1:R1", "Ins3:U1:R2", 
"Ins3:U1:R3", "Ins3:U1:R4", "Ins3:U1:R(5,9)", "Ins3:U3:R1", "Ins3:U3:R2", 
"Ins3:U3:R3", "Ins3:U3:R4", "Ins3:U3:R(5,9)", "Ins4:U1:R1", "Ins4:U1:R2", 
"Ins4:U1:R3", "Ins4:U1:R4", "Ins4:U1:R(5,9)", "Ins4:U2:R1", "Ins4:U2:R2", 
"Ins4:U2:R3", "Ins4:U2:R4", "Ins4:U2:R(5,9)", "Ins4:U4:R1", "Ins4:U4:R2", 
"Ins4:U4:R3", "Ins4:U4:R4", "Ins4:U4:R(5,9)", "Ins(5,):U1:R1", 
"Ins(5,):U1:R2", "Ins(5,):U1:R3", "Ins(5,):U1:R4", "Ins(5,):U1:R(5,9)", 
"Ins(5,):U2:R1", "Ins(5,):U2:R2", "Ins(5,):U2:R3", "Ins(5,):U2:R4", 
"Ins(5,):U2:R(5,9)", "Ins(5,):U(3,):R1", "Ins(5,):U(3,):R2", 
"Ins(5,):U(3,):R3", "Ins(5,):U(3,):R4", "Ins(5,):U(3,):R(5,9)", 
"A[Del(C):R1]A", "A[Del(C):R1]G", "A[Del(C):R1]T", "A[Del(C):R2]A", 
"A[Del(C):R2]G", "A[Del(C):R2]T", "A[Del(C):R3]A", "A[Del(C):R3]G", 
"A[Del(C):R3]T", "A[Del(C):R4]A", "A[Del(C):R4]G", "A[Del(C):R4]T", 
"A[Del(C):R5]A", "A[Del(C):R5]G", "A[Del(C):R5]T", "A[Del(C):R6]A", 
"A[Del(C):R6]G", "A[Del(C):R6]T", "A[Del(C):R7]A", "A[Del(C):R7]G", 
"A[Del(C):R7]T", "A[Del(C):R8]A", "A[Del(C):R8]G", "A[Del(C):R8]T", 
"A[Del(C):R9]A", "A[Del(C):R9]G", "A[Del(C):R9]T", "G[Del(C):R1]A", 
"G[Del(C):R1]G", "G[Del(C):R1]T", "G[Del(C):R2]A", "G[Del(C):R2]G", 
"G[Del(C):R2]T", "G[Del(C):R3]A", "G[Del(C):R3]G", "G[Del(C):R3]T", 
"G[Del(C):R4]A", "G[Del(C):R4]G", "G[Del(C):R4]T", "G[Del(C):R5]A", 
"G[Del(C):R5]G", "G[Del(C):R5]T", "G[Del(C):R6]A", "G[Del(C):R6]G", 
"G[Del(C):R6]T", "G[Del(C):R7]A", "G[Del(C):R7]G", "G[Del(C):R7]T", 
"G[Del(C):R8]A", "G[Del(C):R8]G", "G[Del(C):R8]T", "G[Del(C):R9]A", 
"G[Del(C):R9]G", "G[Del(C):R9]T", "T[Del(C):R1]A", "T[Del(C):R1]G", 
"T[Del(C):R1]T", "T[Del(C):R2]A", "T[Del(C):R2]G", "T[Del(C):R2]T", 
"T[Del(C):R3]A", "T[Del(C):R3]G", "T[Del(C):R3]T", "T[Del(C):R4]A", 
"T[Del(C):R4]G", "T[Del(C):R4]T", "T[Del(C):R5]A", "T[Del(C):R5]G", 
"T[Del(C):R5]T", "T[Del(C):R6]A", "T[Del(C):R6]G", "T[Del(C):R6]T", 
"T[Del(C):R7]A", "T[Del(C):R7]G", "T[Del(C):R7]T", "T[Del(C):R8]A", 
"T[Del(C):R8]G", "T[Del(C):R8]T", "T[Del(C):R9]A", "T[Del(C):R9]G", 
"T[Del(C):R9]T", "A[Del(T):R1]A", "A[Del(T):R1]C", "A[Del(T):R1]G", 
"A[Del(T):R2]A", "A[Del(T):R2]C", "A[Del(T):R2]G", "A[Del(T):R3]A", 
"A[Del(T):R3]C", "A[Del(T):R3]G", "A[Del(T):R4]A", "A[Del(T):R4]C", 
"A[Del(T):R4]G", "A[Del(T):R5]A", "A[Del(T):R5]C", "A[Del(T):R5]G", 
"A[Del(T):R6]A", "A[Del(T):R6]C", "A[Del(T):R6]G", "A[Del(T):R7]A", 
"A[Del(T):R7]C", "A[Del(T):R7]G", "A[Del(T):R8]A", "A[Del(T):R8]C", 
"A[Del(T):R8]G", "A[Del(T):R9]A", "A[Del(T):R9]C", "A[Del(T):R9]G", 
"C[Del(T):R1]A", "C[Del(T):R1]C", "C[Del(T):R1]G", "C[Del(T):R2]A", 
"C[Del(T):R2]C", "C[Del(T):R2]G", "C[Del(T):R3]A", "C[Del(T):R3]C", 
"C[Del(T):R3]G", "C[Del(T):R4]A", "C[Del(T):R4]C", "C[Del(T):R4]G", 
"C[Del(T):R5]A", "C[Del(T):R5]C", "C[Del(T):R5]G", "C[Del(T):R6]A", 
"C[Del(T):R6]C", "C[Del(T):R6]G", "C[Del(T):R7]A", "C[Del(T):R7]C", 
"C[Del(T):R7]G", "C[Del(T):R8]A", "C[Del(T):R8]C", "C[Del(T):R8]G", 
"C[Del(T):R9]A", "C[Del(T):R9]C", "C[Del(T):R9]G", "G[Del(T):R1]A", 
"G[Del(T):R1]C", "G[Del(T):R1]G", "G[Del(T):R2]A", "G[Del(T):R2]C", 
"G[Del(T):R2]G", "G[Del(T):R3]A", "G[Del(T):R3]C", "G[Del(T):R3]G", 
"G[Del(T):R4]A", "G[Del(T):R4]C", "G[Del(T):R4]G", "G[Del(T):R5]A", 
"G[Del(T):R5]C", "G[Del(T):R5]G", "G[Del(T):R6]A", "G[Del(T):R6]C", 
"G[Del(T):R6]G", "G[Del(T):R7]A", "G[Del(T):R7]C", "G[Del(T):R7]G", 
"G[Del(T):R8]A", "G[Del(T):R8]C", "G[Del(T):R8]G", "G[Del(T):R9]A", 
"G[Del(T):R9]C", "G[Del(T):R9]G", "Del2:U1:R1", "Del3:U1:R1", 
"Del4:U1:R1", "Del5:U1:R1", "Del6:U1:R1", "Del7:U1:R1", "Del8:U1:R1", 
"Del9:U1:R1", "Del(10,):U1:R1", "Del2:U(2,):R1", "Del3:U(2,):R1", 
"Del4:U(2,):R1", "Del5:U(2,):R1", "Del6:U(2,):R1", "Del7:U(2,):R1", 
"Del8:U(2,):R1", "Del9:U(2,):R1", "Del(10,):U(2,):R1", "Del2:U1:R3", 
"Del2:U1:R4", "Del2:U1:R(5,9)", "Del2:U2:R2", "Del2:U2:R3", "Del2:U2:R4", 
"Del2:U2:R(5,9)", "Del3:U1:R4", "Del3:U1:R(5,9)", "Del3:U3:R2", 
"Del3:U3:R3", "Del3:U3:R4", "Del3:U3:R(5,9)", "Del4:U1:R(5,9)", 
"Del4:U2:R3", "Del4:U2:R4", "Del4:U2:R(5,9)", "Del4:U4:R2", "Del4:U4:R3", 
"Del4:U4:R4", "Del4:U4:R(5,9)", "Del5:U1:R(6,9)", "Del5:U5:R2", 
"Del5:U5:R3", "Del5:U5:R4", "Del5:U5:R(5,9)", "Del(6,):U1:R(7,9)", 
"Del(6,):U2:R(4,9)", "Del(6,):U3:R(3,9)", "Del(6,):U(4,):R(2,9)", 
"Del2:M1", "Del3:M1", "Del3:M2", "Del4:M1", "Del4:M2", "Del4:M3", 
"Del5:M1", "Del5:M2", "Del5:M3", "Del5:M4", "Del6:M1", "Del6:M2", 
"Del6:M3", "Del6:M4", "Del6:M5", "Del(7,):M1", "Del(7,):M2", 
"Del(7,):M3", "Del(7,):M4", "Del(7,):M5", "Del(7,):M(6,)", "Complex(0,1)", 
"Complex(2,5)", "Complex(6,10)", "Complex(11,20)", "Complex(21,)"
), Indel = c("Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
"Ins(C)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
"Del(2,):R(1,9)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Complex", "Complex", "Complex", 
"Complex", "Complex"), Indel3 = c("Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Complex", "Complex", "Complex", "Complex", "Complex"), Figlabel = c("A[Ins(C):R0]A", 
"A[Ins(C):R0]G", "A[Ins(C):R0]T", "A[Ins(C):R1]A", "A[Ins(C):R1]G", 
"A[Ins(C):R1]T", "A[Ins(C):R2]A", "A[Ins(C):R2]G", "A[Ins(C):R2]T", 
"A[Ins(C):R3]A", "A[Ins(C):R3]G", "A[Ins(C):R3]T", "A[Ins(C):R4]A", 
"A[Ins(C):R4]G", "A[Ins(C):R4]T", "A[Ins(C):R5]A", "A[Ins(C):R5]G", 
"A[Ins(C):R5]T", "A[Ins(C):R6]A", "A[Ins(C):R6]G", "A[Ins(C):R6]T", 
"A[Ins(C):R7]A", "A[Ins(C):R7]G", "A[Ins(C):R7]T", "A[Ins(C):R8]A", 
"A[Ins(C):R8]G", "A[Ins(C):R8]T", "A[Ins(C):R9]A", "A[Ins(C):R9]G", 
"A[Ins(C):R9]T", "G[Ins(C):R0]A", "G[Ins(C):R0]G", "G[Ins(C):R0]T", 
"G[Ins(C):R1]A", "G[Ins(C):R1]G", "G[Ins(C):R1]T", "G[Ins(C):R2]A", 
"G[Ins(C):R2]G", "G[Ins(C):R2]T", "G[Ins(C):R3]A", "G[Ins(C):R3]G", 
"G[Ins(C):R3]T", "G[Ins(C):R4]A", "G[Ins(C):R4]G", "G[Ins(C):R4]T", 
"G[Ins(C):R5]A", "G[Ins(C):R5]G", "G[Ins(C):R5]T", "G[Ins(C):R6]A", 
"G[Ins(C):R6]G", "G[Ins(C):R6]T", "G[Ins(C):R7]A", "G[Ins(C):R7]G", 
"G[Ins(C):R7]T", "G[Ins(C):R8]A", "G[Ins(C):R8]G", "G[Ins(C):R8]T", 
"G[Ins(C):R9]A", "G[Ins(C):R9]G", "G[Ins(C):R9]T", "T[Ins(C):R0]A", 
"T[Ins(C):R0]G", "T[Ins(C):R0]T", "T[Ins(C):R1]A", "T[Ins(C):R1]G", 
"T[Ins(C):R1]T", "T[Ins(C):R2]A", "T[Ins(C):R2]G", "T[Ins(C):R2]T", 
"T[Ins(C):R3]A", "T[Ins(C):R3]G", "T[Ins(C):R3]T", "T[Ins(C):R4]A", 
"T[Ins(C):R4]G", "T[Ins(C):R4]T", "T[Ins(C):R5]A", "T[Ins(C):R5]G", 
"T[Ins(C):R5]T", "T[Ins(C):R6]A", "T[Ins(C):R6]G", "T[Ins(C):R6]T", 
"T[Ins(C):R7]A", "T[Ins(C):R7]G", "T[Ins(C):R7]T", "T[Ins(C):R8]A", 
"T[Ins(C):R8]G", "T[Ins(C):R8]T", "T[Ins(C):R9]A", "T[Ins(C):R9]G", 
"T[Ins(C):R9]T", "A[Ins(T):R0]A", "A[Ins(T):R0]C", "A[Ins(T):R0]G", 
"A[Ins(T):R1]A", "A[Ins(T):R1]C", "A[Ins(T):R1]G", "A[Ins(T):R2]A", 
"A[Ins(T):R2]C", "A[Ins(T):R2]G", "A[Ins(T):R3]A", "A[Ins(T):R3]C", 
"A[Ins(T):R3]G", "A[Ins(T):R4]A", "A[Ins(T):R4]C", "A[Ins(T):R4]G", 
"A[Ins(T):R5]A", "A[Ins(T):R5]C", "A[Ins(T):R5]G", "A[Ins(T):R6]A", 
"A[Ins(T):R6]C", "A[Ins(T):R6]G", "A[Ins(T):R7]A", "A[Ins(T):R7]C", 
"A[Ins(T):R7]G", "A[Ins(T):R8]A", "A[Ins(T):R8]C", "A[Ins(T):R8]G", 
"A[Ins(T):R9]A", "A[Ins(T):R9]C", "A[Ins(T):R9]G", "C[Ins(T):R0]A", 
"C[Ins(T):R0]C", "C[Ins(T):R0]G", "C[Ins(T):R1]A", "C[Ins(T):R1]C", 
"C[Ins(T):R1]G", "C[Ins(T):R2]A", "C[Ins(T):R2]C", "C[Ins(T):R2]G", 
"C[Ins(T):R3]A", "C[Ins(T):R3]C", "C[Ins(T):R3]G", "C[Ins(T):R4]A", 
"C[Ins(T):R4]C", "C[Ins(T):R4]G", "C[Ins(T):R5]A", "C[Ins(T):R5]C", 
"C[Ins(T):R5]G", "C[Ins(T):R6]A", "C[Ins(T):R6]C", "C[Ins(T):R6]G", 
"C[Ins(T):R7]A", "C[Ins(T):R7]C", "C[Ins(T):R7]G", "C[Ins(T):R8]A", 
"C[Ins(T):R8]C", "C[Ins(T):R8]G", "C[Ins(T):R9]A", "C[Ins(T):R9]C", 
"C[Ins(T):R9]G", "G[Ins(T):R0]A", "G[Ins(T):R0]C", "G[Ins(T):R0]G", 
"G[Ins(T):R1]A", "G[Ins(T):R1]C", "G[Ins(T):R1]G", "G[Ins(T):R2]A", 
"G[Ins(T):R2]C", "G[Ins(T):R2]G", "G[Ins(T):R3]A", "G[Ins(T):R3]C", 
"G[Ins(T):R3]G", "G[Ins(T):R4]A", "G[Ins(T):R4]C", "G[Ins(T):R4]G", 
"G[Ins(T):R5]A", "G[Ins(T):R5]C", "G[Ins(T):R5]G", "G[Ins(T):R6]A", 
"G[Ins(T):R6]C", "G[Ins(T):R6]G", "G[Ins(T):R7]A", "G[Ins(T):R7]C", 
"G[Ins(T):R7]G", "G[Ins(T):R8]A", "G[Ins(T):R8]C", "G[Ins(T):R8]G", 
"G[Ins(T):R9]A", "G[Ins(T):R9]C", "G[Ins(T):R9]G", "Ins(2,4):M", 
"Ins(5,):M", "Ins2:U1:R0", "Ins3:U1:R0", "Ins4:U1:R0", "Ins2:U2:R0", 
"Ins3:U3:R0", "Ins4:U2:R0", "Ins4:U4:R0", "Ins(5,):R0", "Ins2:U1:R1", 
"Ins2:U1:R2", "Ins2:U1:R3", "Ins2:U1:R4", "Ins2:U1:R(5,9)", "Ins2:U2:R1", 
"Ins2:U2:R2", "Ins2:U2:R3", "Ins2:U2:R4", "Ins2:U2:R(5,9)", "Ins3:U1:R1", 
"Ins3:U1:R2", "Ins3:U1:R3", "Ins3:U1:R4", "Ins3:U1:R(5,9)", "Ins3:U3:R1", 
"Ins3:U3:R2", "Ins3:U3:R3", "Ins3:U3:R4", "Ins3:U3:R(5,9)", "Ins4:U1:R1", 
"Ins4:U1:R2", "Ins4:U1:R3", "Ins4:U1:R4", "Ins4:U1:R(5,9)", "Ins4:U2:R1", 
"Ins4:U2:R2", "Ins4:U2:R3", "Ins4:U2:R4", "Ins4:U2:R(5,9)", "Ins4:U4:R1", 
"Ins4:U4:R2", "Ins4:U4:R3", "Ins4:U4:R4", "Ins4:U4:R(5,9)", "Ins(5,):U1:R1", 
"Ins(5,):U1:R2", "Ins(5,):U1:R3", "Ins(5,):U1:R4", "Ins(5,):U1:R(5,9)", 
"Ins(5,):U2:R1", "Ins(5,):U2:R2", "Ins(5,):U2:R3", "Ins(5,):U2:R4", 
"Ins(5,):U2:R(5,9)", "Ins(5,):U(3,):R1", "Ins(5,):U(3,):R2", 
"Ins(5,):U(3,):R3", "Ins(5,):U(3,):R4", "Ins(5,):U(3,):R(5,9)", 
"A[Del(C):R1]A", "A[Del(C):R1]G", "A[Del(C):R1]T", "A[Del(C):R2]A", 
"A[Del(C):R2]G", "A[Del(C):R2]T", "A[Del(C):R3]A", "A[Del(C):R3]G", 
"A[Del(C):R3]T", "A[Del(C):R4]A", "A[Del(C):R4]G", "A[Del(C):R4]T", 
"A[Del(C):R5]A", "A[Del(C):R5]G", "A[Del(C):R5]T", "A[Del(C):R6]A", 
"A[Del(C):R6]G", "A[Del(C):R6]T", "A[Del(C):R7]A", "A[Del(C):R7]G", 
"A[Del(C):R7]T", "A[Del(C):R8]A", "A[Del(C):R8]G", "A[Del(C):R8]T", 
"A[Del(C):R9]A", "A[Del(C):R9]G", "A[Del(C):R9]T", "G[Del(C):R1]A", 
"G[Del(C):R1]G", "G[Del(C):R1]T", "G[Del(C):R2]A", "G[Del(C):R2]G", 
"G[Del(C):R2]T", "G[Del(C):R3]A", "G[Del(C):R3]G", "G[Del(C):R3]T", 
"G[Del(C):R4]A", "G[Del(C):R4]G", "G[Del(C):R4]T", "G[Del(C):R5]A", 
"G[Del(C):R5]G", "G[Del(C):R5]T", "G[Del(C):R6]A", "G[Del(C):R6]G", 
"G[Del(C):R6]T", "G[Del(C):R7]A", "G[Del(C):R7]G", "G[Del(C):R7]T", 
"G[Del(C):R8]A", "G[Del(C):R8]G", "G[Del(C):R8]T", "G[Del(C):R9]A", 
"G[Del(C):R9]G", "G[Del(C):R9]T", "T[Del(C):R1]A", "T[Del(C):R1]G", 
"T[Del(C):R1]T", "T[Del(C):R2]A", "T[Del(C):R2]G", "T[Del(C):R2]T", 
"T[Del(C):R3]A", "T[Del(C):R3]G", "T[Del(C):R3]T", "T[Del(C):R4]A", 
"T[Del(C):R4]G", "T[Del(C):R4]T", "T[Del(C):R5]A", "T[Del(C):R5]G", 
"T[Del(C):R5]T", "T[Del(C):R6]A", "T[Del(C):R6]G", "T[Del(C):R6]T", 
"T[Del(C):R7]A", "T[Del(C):R7]G", "T[Del(C):R7]T", "T[Del(C):R8]A", 
"T[Del(C):R8]G", "T[Del(C):R8]T", "T[Del(C):R9]A", "T[Del(C):R9]G", 
"T[Del(C):R9]T", "A[Del(T):R1]A", "A[Del(T):R1]C", "A[Del(T):R1]G", 
"A[Del(T):R2]A", "A[Del(T):R2]C", "A[Del(T):R2]G", "A[Del(T):R3]A", 
"A[Del(T):R3]C", "A[Del(T):R3]G", "A[Del(T):R4]A", "A[Del(T):R4]C", 
"A[Del(T):R4]G", "A[Del(T):R5]A", "A[Del(T):R5]C", "A[Del(T):R5]G", 
"A[Del(T):R6]A", "A[Del(T):R6]C", "A[Del(T):R6]G", "A[Del(T):R7]A", 
"A[Del(T):R7]C", "A[Del(T):R7]G", "A[Del(T):R8]A", "A[Del(T):R8]C", 
"A[Del(T):R8]G", "A[Del(T):R9]A", "A[Del(T):R9]C", "A[Del(T):R9]G", 
"C[Del(T):R1]A", "C[Del(T):R1]C", "C[Del(T):R1]G", "C[Del(T):R2]A", 
"C[Del(T):R2]C", "C[Del(T):R2]G", "C[Del(T):R3]A", "C[Del(T):R3]C", 
"C[Del(T):R3]G", "C[Del(T):R4]A", "C[Del(T):R4]C", "C[Del(T):R4]G", 
"C[Del(T):R5]A", "C[Del(T):R5]C", "C[Del(T):R5]G", "C[Del(T):R6]A", 
"C[Del(T):R6]C", "C[Del(T):R6]G", "C[Del(T):R7]A", "C[Del(T):R7]C", 
"C[Del(T):R7]G", "C[Del(T):R8]A", "C[Del(T):R8]C", "C[Del(T):R8]G", 
"C[Del(T):R9]A", "C[Del(T):R9]C", "C[Del(T):R9]G", "G[Del(T):R1]A", 
"G[Del(T):R1]C", "G[Del(T):R1]G", "G[Del(T):R2]A", "G[Del(T):R2]C", 
"G[Del(T):R2]G", "G[Del(T):R3]A", "G[Del(T):R3]C", "G[Del(T):R3]G", 
"G[Del(T):R4]A", "G[Del(T):R4]C", "G[Del(T):R4]G", "G[Del(T):R5]A", 
"G[Del(T):R5]C", "G[Del(T):R5]G", "G[Del(T):R6]A", "G[Del(T):R6]C", 
"G[Del(T):R6]G", "G[Del(T):R7]A", "G[Del(T):R7]C", "G[Del(T):R7]G", 
"G[Del(T):R8]A", "G[Del(T):R8]C", "G[Del(T):R8]G", "G[Del(T):R9]A", 
"G[Del(T):R9]C", "G[Del(T):R9]G", "Del2:U1:R1", "Del3:U1:R1", 
"Del4:U1:R1", "Del5:U1:R1", "Del6:U1:R1", "Del7:U1:R1", "Del8:U1:R1", 
"Del9:U1:R1", "Del(10,):U1:R1", "Del2:U(2,):R1", "Del3:U(2,):R1", 
"Del4:U(2,):R1", "Del5:U(2,):R1", "Del6:U(2,):R1", "Del7:U(2,):R1", 
"Del8:U(2,):R1", "Del9:U(2,):R1", "Del(10,):U(2,):R1", "Del2:U1:R3", 
"Del2:U1:R4", "Del2:U1:R(5,9)", "Del2:U2:R2", "Del2:U2:R3", "Del2:U2:R4", 
"Del2:U2:R(5,9)", "Del3:U1:R4", "Del3:U1:R(5,9)", "Del3:U3:R2", 
"Del3:U3:R3", "Del3:U3:R4", "Del3:U3:R(5,9)", "Del4:U1:R(5,9)", 
"Del4:U2:R3", "Del4:U2:R4", "Del4:U2:R(5,9)", "Del4:U4:R2", "Del4:U4:R3", 
"Del4:U4:R4", "Del4:U4:R(5,9)", "Del5:U1:R(6,9)", "Del5:U5:R2", 
"Del5:U5:R3", "Del5:U5:R4", "Del5:U5:R(5,9)", "Del(6,):U1:R(7,9)", 
"Del(6,):U2:R(4,9)", "Del(6,):U3:R(3,9)", "Del(6,):U(4,):R(2,9)", 
"Del2:M1", "Del3:M1", "Del3:M2", "Del4:M1", "Del4:M2", "Del4:M3", 
"Del5:M1", "Del5:M2", "Del5:M3", "Del5:M4", "Del6:M1", "Del6:M2", 
"Del6:M3", "Del6:M4", "Del6:M5", "Del(7,):M1", "Del(7,):M2", 
"Del(7,):M3", "Del(7,):M4", "Del(7,):M5", "Del(7,):M(6,)", "Complex(0,1)", 
"Complex(2,5)", "Complex(6,10)", "Complex(11,20)", "Complex(21,)"
)), class = "data.frame", row.names = c(NA, -476L))




  muts_basis_melt <- reshape2::melt(muts_basis,"IndelType")

  muts_basis_melt <- merge(indel_template_type_4_full_figurelabel, muts_basis_melt,by="IndelType",all.x=T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType","Indel","Indel3","Figlabel","Sample","freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)



  indel_mypalette_fill <- c("#000000", # FEABB9 Complex
                            "#762A83", # Del(2,):M(1,)
                            "#EE6677", # Del(2,):R(0,9)
                            "#004488", # Del(C)
                            "#997700", # Del(T)
                            "#EE99AA", #"#668D3C"  Ins(2,)
                            "#6699CC", #"#007996"  Ins(C)
                            "#EECC66") # Ins(T)

  indel_positions <- indel_template_type_4_full_figurelabel$IndelType
  indel_positions_labels <- indel_template_type_4_full_figurelabel$Figlabel
  entry <- table(indel_template_type_4_full_figurelabel$Indel)
  order_entry <- c("Ins(C)", "Ins(T)", "Ins(2,)", "Del(C)", "Del(T)", "Del(2,):R(1,9)", "Del(2,):M(1,)", "Complex")
  entry <- entry[order_entry]
  blocks <- data.frame(Type=unique(indel_template_type_4_full_figurelabel$Indel),
                       fill=indel_mypalette_fill,
                       xmin=c(0,cumsum(entry)[-length(entry)])+0.5,
                       xmax=cumsum(entry)+0.5)
  blocks$ymin <- -max(muts_basis_melt$freq)*0.12#
  blocks$ymax <- 0
  blocks$labels <-c("1bp C", "1bp T", ">=2bp", "1bp C", "1bp T", ">=2bp", "Mh", "X")
  blocks$cl <-c("black", "black", "black", "white", "white", "white", "white",  "white")

  # grey blocks for insertion or deletion or complex
  indel_mypalette_fill3 <- c("#000000", # FEABB9 Complex
                             "#888888", # Deletion
                             "#DDDDDD") # Insertion

  entry3 <- table(indel_template_type_4_full_figurelabel$Indel3)
  order_entry3 <- c("Insertion", "Deletion", "Complex")
  entry3 <- entry3[order_entry3]
  blocks3 <- data.frame(Type=unique(indel_template_type_4_full_figurelabel$Indel3),
                        fill=indel_mypalette_fill3,
                        xmin=c(0,cumsum(entry3)[-length(entry3)])+0.5,
                        xmax=cumsum(entry3)+0.5)
  blocks3$ymin <- max(muts_basis_melt$freq)*1.08#
  blocks3$ymax <- max(muts_basis_melt$freq)*1.2
  blocks3$labels <-c("Insertion", "Deletion", "X")
  blocks3$cl <-c("black", "white",  "white")

  indel_mypalette_fill_all <- c("#000000", # FEABB9 Complex

                                "#762A83", # Del(2,):M(1,)
                                "#EE3377", # Del(2,):R(0,9) #EE6677
                                "#004488", # Del(C)
                                "#997700", # Del(T)
                                "#888888", # Deletion

                                "#EE99AA", #"#668D3C"  Ins(2,)
                                "#6699CC", #"#007996"  Ins(C)
                                "#EECC66", # Ins(T)
                                "#DDDDDD") # Insertion

  p <- ggplot2::ggplot(data=muts_basis_melt, ggplot2::aes(x=IndelType, y=freq,fill=Indel))+ ggplot2::geom_bar(stat="identity",position="dodge", width=.7)+ggplot2::xlab("Indel Types")+ggplot2::ylab("Count")
  #  p <- p+scale_y_continuous(limits=c(0,40),breaks=(seq(0,40,10)))
  p <- p+ggplot2::scale_x_discrete(limits = indel_positions,labels = indel_positions_labels)+ ggplot2::ggtitle(plot_title)
  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill_all)+ggplot2::coord_cartesian(ylim=c(unique(blocks$ymin),unique(blocks3$ymax)), expand = F)
  p <- p+ggplot2::theme_classic()+ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                                                 axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                                                 axis.ticks.x=ggplot2::element_blank(),
                                                 axis.line.x=ggplot2::element_blank(),
                                                 legend.position = "none",
                                                 #axis.title.x = ggplot2::element_text(size=15),
                                                 axis.title.y = ggplot2::element_text(size=15))

  ## Add the overhead blocks
  p <- p+ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    # geom_text(data=blocks,aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels),size=text_size,inherit.aes = F,colour="white")
    ggplot2::geom_text(data=blocks,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)+ggplot2::scale_colour_manual(values=c("black", "white"))

  ## Add the overhead blocks for insertion or deletion or complex
  p <- p+ggplot2::geom_rect(data = blocks3, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    # geom_text(data=blocks,aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels),size=text_size,inherit.aes = F,colour="white")
    ggplot2::geom_text(data=blocks3,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)

  return(p)


  # return(muts_basis)
}






#' Plot indel profile in a extended channel bar plot, original plots_type_4_m4_89 function
#'
#' @param muts_basis A indel catalogue of multiple samples
#' @param colnum Number of columns
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param text_size Size of text
#' @param print_Xlabel print X axis label or not
#' @param outputname Output file name of the plot
#' @return A plot including extended channel indel profile of multiple samples
#' @import gridExtra
#' @export
plots_indelprofile_full<- function(muts_basis,colnum, h,w,text_size,print_Xlabel=T, print_pdf = T, print_png = F, outputname){

  cnames <- names(muts_basis)
  #  cnames <- cnames[cnames !="IndelType"]
  #  muts_basis2 <- muts_basis[,names(muts_basis) != "IndelType"]
  muts_basis2 <- muts_basis

  if(length(cnames)>1){
    p_all <- list()
    for(i in 1:dim(muts_basis2)[2]){

      plottitle <- paste0(names(muts_basis2)[i], ", N =", sum(muts_basis2[,i]))
      if(print_Xlabel==T){
        p <- gen_plot_catalougefull_single(data.frame("Sample"=muts_basis2[,i],"IndelType"=rownames(muts_basis2)), text_size,plottitle)

      }else{
        p <- gen_plot_catalougefull_single_noXlabel(data.frame("Sample"=muts_basis2[,i],"IndelType"=rownames(muts_basis2)), text_size,plottitle)

      }
      p_all[[length(p_all)+1]] <- p

    }

    if(is.null(outputname)==F){
      
      if(print_pdf){
        filename <- paste0(outputname, ".pdf")
        grDevices::pdf(file=filename, onefile=TRUE,width=w,height=h)
        
        do.call("grid.arrange", c(p_all, ncol = colnum))
        grDevices::dev.off()
      }
      
      if(print_png){
        filename <- paste0(outputname, ".png")
        grDevices::png(file=filename,width=w,height=h)
        
        do.call("grid.arrange", c(p_all, ncol = colnum))
        grDevices::dev.off()
      }   
      
    }else{
      
      return(do.call("grid.arrange", c(p_all, ncol = colnum)))
      
    }
    
    




  }else{
    plottitle <- paste0(names(muts_basis2)[1], ", N =", sum(muts_basis2[,1]))
    p_all <- list()
    if(print_Xlabel==T){
      p <- gen_plot_catalougefull_single(data.frame("Sample"=muts_basis2[,1],"IndelType"=rownames(muts_basis2)), text_size,plottitle)

    }else{
      p <- gen_plot_catalougefull_single_noXlabel(data.frame("Sample"=muts_basis2[,1],"IndelType"=rownames(muts_basis2)), text_size,plottitle)

    }
    p_all[[length(p_all)+1]] <- p
    
    if(is.null(outputname)==F){
      
      if(print_pdf){
            filename <- paste0(outputname, ".pdf")
            grDevices::pdf(file=filename, onefile=TRUE,width=w,height=h)
        
            do.call("grid.arrange", c(p_all, ncol = colnum))
            grDevices::dev.off()
      }
      
      if(print_png){
            filename <- paste0(outputname, ".png")
            grDevices::png(file=filename,width=w,height=h)
        
            do.call("grid.arrange", c(p_all, ncol = colnum))
            grDevices::dev.off()
      }   
      
    }else{
      
          return(do.call("grid.arrange", c(p_all, ncol = colnum)))
      
    }
    

  }

}


#' Plot indel profile in a extended channel bar plot, original plots_type_4_m4_89 function
#' Not show X labels
#' @param muts_basis A indel catalogue of multiple samples
#' @param colnum Number of columns
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param text_size Size of text
#' @param outputname Output file name of the plot
#' @return A plot including extended channel indel profile of multiple samples
#' @import gridExtra
#' @export
plots_indelprofile_fullch_noXlabel<- function(muts_basis,colnum, h,w,text_size,outputname){

  cnames <- names(muts_basis)
  cnames <- cnames[cnames !="IndelType"]
  muts_basis2 <- muts_basis[,names(muts_basis) != "IndelType"]

  if(length(cnames)>1){
    p_all <- list()
    for(i in 1:dim(muts_basis2)[2]){

      plottitle <- paste0(names(muts_basis2)[i], ", N =", sum(muts_basis2[,i]))
      p <- gen_plot_catalougefull_single_noXlabel(data.frame("Sample"=muts_basis2[,i],"IndelType"=rownames(muts_basis2)), text_size,plottitle)
      p_all[[length(p_all)+1]] <- p

    }

    filename <- paste0(outputname, ".pdf")
    grDevices::pdf(file=filename, onefile=TRUE,width=w,height=h)

    do.call("grid.arrange", c(p_all, ncol = colnum))
    grDevices::dev.off()

  }else{
    p <- gen_plot_catalougefull_single_noXlabel(data.frame("Sample"=muts_basis[,cnames],"IndelType"=rownames(muts_basis)), text_size,cnames)
    filename <- paste0(outputname, ".pdf")
    grDevices::pdf(file=filename, onefile=TRUE,width=10,height=5)

    do.call("grid.arrange", c(p, ncol = 1))
    grDevices::dev.off()


  }

}

