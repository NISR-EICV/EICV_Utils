library(haven)
setwd("C:/Muhoza/R/Muhoza_Project")
EICV5_person <- read_dta("C:/EICV5/2.Working_Data/PartA/cs_S1_S2_S3_S4_S6A_S6E_Person.dta")
attach(EICV5_person)


createCrossTab <- function(var1,var2 = NULL ,col  = FALSE,row = FALSE, cell = FALSE) {
#  createCrossTab <- function(var1,var2) {
  require(dplyr)
  require(labelled) 
  var1<-to_factor(var1)

    
  if (is.null(var2)){
    
    tabe<-table(var1)
    tabe<- data.frame(tabe)
    total<-sum(tabe[,2])
    total <- data.frame(cbind("Total",as.integer(total)))
    names(total) <- names(tabe)
    tabe<-rbind(tabe,total)
        ### Convert tabe as a dataframe for easy manipulation
        ### We want to compute the column percentage
        tabe<-as.data.frame(tabe)
      ### Create a vector with missing that will store the column proportions
        Prop<-rep(NA, nrow(tabe))
           for (c in 1:nrow(tabe)) {
             output<-as.integer(tabe[c,2])/as.integer(tabe[nrow(tabe),2])*100
             Prop[c]<-output
             ### Round the output to two degits
             Prop<-round(Prop,2)
           }
        tabe<-cbind(tabe,Prop)
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
    
    ### Compute the percentage on cell
    if (cell == TRUE){
      tabe<-table(var1,var2)
      tabe <- round(prop.table(tabe)*100,2)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
    }
    
    if (col == FALSE & row == FALSE & cell == FALSE)  { 
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
  
#a<-aggregate(weight~province+s1q1, data=EICV5_person, FUN = sum) 

# df<-EICV5_person  %>% 
# group_by(province,s1q1) %>% 
#     summarise(sum=sum(weight)) %>% 
  # arrange(province,s1q1)
df<-aggregate(weight~province+s1q1, data=EICV5_person, FUN = sum)%>% 
pivot_wider(names_to = "s1q1", values_to ="weight")


pivot_wider(df[2:3], names_to = names(df[2]), values_to =names(df[3]))



