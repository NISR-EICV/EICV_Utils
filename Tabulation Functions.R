library(haven)
setwd("C:/Muhoza/R/Muhoza_Project")
EICV5_person <- read_dta("C:/EICV5/2.Working_Data/PartA/cs_S1_S2_S3_S4_S6A_S6E_Person.dta")
attach(EICV5_person)


createCrossTab <- function(var1,var2 = NULL ,col  = FALSE,row = FALSE) {
#  createCrossTab <- function(var1,var2) {
  require(labelled) 
  var1<-to_factor(var1)
  
  
  if (is.null(var2)){
    
    tabe<-table(var1)
    tabe<- data.frame(tabe)
    total<-sum(tabe[,2])
    total <- data.frame(cbind("Total",as.integer(total)))
    names(total) <- names(tabe)
    tabe<-rbind(tabe,total)

  }
 
  else {
    var2<-to_factor(var2)
    
    if (row == TRUE){
      tabe<-table(var1,var2)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
      tabe <- round(prop.table(tabe,1)*100,2)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      # tabe_col <- colSums(tabe)
    }
    # 
    if (col == TRUE){
      tabe<-table(var1,var2)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      tabe <- round(prop.table(tabe,2)*100,2)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
    }
    
    if (col == FALSE & row == FALSE)  { 
      tabe<-table(var1,var2)
      names_row<-levels(var1) 
      names_col<-levels(var2) 
      names_row<-append(names_row,"Total")
      names_col<-append(names_col, "Total") 
      tabe<-as.matrix.data.frame(tabe) 
      
      tabe_row <- rowSums(tabe)
      tabe<- cbind(tabe,tabe_row)
      tabe_col<- colSums(tabe)
      tabe<-rbind(tabe,tabe_col)
      
      colnames(tabe) <- names_col
      rownames(tabe) <- names_row 
    }
  }
    
    tabe

    }
  


