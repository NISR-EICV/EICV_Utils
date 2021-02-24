#' Crosstabulation of survey data (Weighted or unweighted data)
#'
#'
#' This function helps the crosstabulation of Single, Two variables either weighted with survey weight or unweighted.
#' It is based on tabulate function like the one used in Stata,it is helpful for survey data.

#' @param var1  Input Variable one for survey data
#' @param var2 Input Variable Two for survey data. By default is null when a single crosstabulation is needed
#' @param weight Attach the weight for data if  you want to weight them. By default is null when weight is not needed
#' @param col Is used when the crosstabulation is needed on column, by default is False make it true when the crosstabulation is needed by column
#' @param row Is used when the crosstabulation is needed on row, by default is False make it true when the crosstabulation is needed by row
#' @param cell Is used when the crosstabulation is needed on cell, by default is False make it true when the crosstabulation is needed by cell
#' @keywords tab
#' @export
#' @examples
#' tab()


tab <- function(var1,var2 = NULL ,weight = NULL, col  = FALSE,row = FALSE, cell = FALSE) {

  #  createCrossTab <- function(var1,var2) {
  # library(dplyr)
  # library(labelled)
  var1<-to_factor(var1)

  ## Weight and var2 are not used

  if (is.null(weight) & is.null(var2)) {

    # if (is.null(var2)){

    #Turn this into a function for one argument
    #crosstab1var(var2)
    tabe<-table(var1)
    tabe<-data.matrix(tabe)
    ## Sum var1 for the total
    Total<-colSums(tabe)
    # Total<-c("Total",total)
    tabe<-rbind(tabe,Total)

    ### Create a vector with missing that will store the column proportions
    Prop<-rep(NA, nrow(tabe))
    for (c in 1:nrow(tabe)) {
      output<-as.integer(tabe[c,1])/as.integer(tabe[nrow(tabe),1])*100
      Prop[c]<-output
      ### Round the output to two degits
      Prop<-round(Prop,2)
    }
    tabe<-cbind(tabe,Prop)

    var1<-to_factor(var1)
    names_row<-levels(var1)

    # names_row <- rbind(names_row,"total")
    names_row<-c(names_row,"Total")
    names_col<-c("Freq","Prop")
    rownames(tabe)<-names_row
    colnames(tabe)<-names_col
  }
  # Weight a single cross tabulation
  else {

    if ((is.null(var2) & !is.null(weight))) {
            tabe<-aggregate(weight~var1, FUN = sum)
      tabe<-data.matrix(tabe[,2])
      ## Sum var1 for the total
      Total<-colSums(tabe)
      # Total<-c("Total",total)
      tabe<-rbind(tabe,Total)

      ### Create a vector with missing that will store the column proportions
      Prop<-rep(NA, nrow(tabe))
      for (c in 1:nrow(tabe)) {
        output<-as.integer(tabe[c,1])/as.integer(tabe[nrow(tabe),1])*100
        Prop[c]<-output
        ### Round the output to two degits
        Prop<-round(Prop,2)
      }
      tabe<-cbind(tabe,Prop)

      var1<-to_factor(var1)
      names_row<-levels(var1)

      # names_row <- rbind(names_row,"total")
      names_row<-c(names_row,"Total")
      names_col<-c("Freq","Prop")
      rownames(tabe)<-names_row
      colnames(tabe)<-names_col
    }

    #crosstab2var()
    # var2<-to_factor(var2)

    if ((row == TRUE) & is.null(weight)){
      tabe<-table(var1,var2)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
      tabe <- round(prop.table(tabe,1)*100,2)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      # tabe_col <- colSums(tabe)
    }

    if (row == TRUE & !is.null(weight) & !is.null(var2)){

      tabe<-aggregate(weight~var1+var2,FUN = sum)
      tabe<- tabe %>%
        pivot_wider(names_from = var2, values_from = weight)
      # tabe<-as.matrix(tabe)
      tabe<-data.matrix(tabe)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
      tabe <- round(prop.table(tabe[,c(2:ncol(tabe))],1)*100,2)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      # Total<-rowSums(tabe)
      # tabe<-cbind(tabe,Total)


      var1<-to_factor(var1)
      var2<-to_factor(var2)

      names_row<-levels(var1)
      names_col<-levels(var2)

      # names_row <- rbind(names_row,"total")
      names_row<-c(names_row,"Total")
      names_col<-c(names_col,"Total")
      rownames(tabe)<-names_row
      colnames(tabe)<-names_col

    }

    if (col == TRUE & is.null(weight)){
      tabe<-table(var1,var2)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      tabe <- round(prop.table(tabe,2)*100,2)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
    }

    if (col == TRUE & !is.null(weight) & !is.null(var2)){

      tabe<-aggregate(weight~var1+var2,FUN = sum)
      tabe<- tabe %>%
        pivot_wider(names_from = var2, values_from = weight)
      ### Convert tabe as data.matrix so that prop.table can work as it is an array,
      tabe<-data.matrix(tabe)
      # tabe<-as.matrix(tabe)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      ## start from the second column to skip value label for var1.
      tabe <- round(prop.table(tabe[,c(2:ncol(tabe))],2)*100,2)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)

      var1<-to_factor(var1)
      var2<-to_factor(var2)

      names_row<-levels(var1)
      names_col<-levels(var2)

      # names_row <- rbind(names_row,"total")
      names_row<-c(names_row,"Total")
      # "" for skiping the first column and start at the next column
      names_col<-c(names_col,"Total")

      # Here i took first column of tabe and give it the names for names_row
      rownames(tabe)<-names_row
      colnames(tabe)<-names_col

    }

    ### Compute the percentage on cell
    if (cell == TRUE & is.null(weight) & !is.null(var2)){
      tabe<-table(var1,var2)
      tabe <- round(prop.table(tabe)*100,2)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
    }
    if (cell == TRUE & !is.null(weight)){

      tabe<-aggregate(weight~var1+var2,FUN = sum)
      tabe<- tabe %>%
        pivot_wider(names_from = var2, values_from = weight)
      # tabe<-as.matrix(tabe)
      # tabe <- round(prop.table(tabe)*100,2)
      tabe <- round(prop.table(data.matrix(tabe[,c(2:ncol(tabe))]))*100,2)
      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
      var1<-to_factor(var1)
      var2<-to_factor(var2)

      names_row<-levels(var1)
      names_col<-levels(var2)

      # names_row <- rbind(names_row,"total")
      names_row<-c(names_row,"Total")
      # "" for skiping the first column and start at the next column
      names_col<-c(names_col,"Total")

      # Here i took first column of tabe and give it the names for names_row
      rownames(tabe)<-names_row
      colnames(tabe)<-names_col

    }

    if (col == FALSE & row == FALSE & cell == FALSE & is.null(weight) & !is.null(var2))  {
      tabe<-table(var1,var2)
      var1<-to_factor(var1)
      var2<-to_factor(var2)
      names_row<-levels(var1)
      names_col<-levels(var2)
      names_row<-c(names_row,"Total")
      names_col<-c(names_col, "Total")
      tabe<-as.matrix.data.frame(tabe)

      tabe_row <- rowSums(tabe)
      tabe<- cbind(tabe,tabe_row)
      tabe_col<- colSums(tabe)
      tabe<-rbind(tabe,tabe_col)

      colnames(tabe) <- names_col
      rownames(tabe) <- names_row
    }
    if (col == FALSE & row == FALSE & cell == FALSE & !is.null(weight) & !is.null(var2)){

      tabe<-aggregate(weight~var1+var2, FUN = sum)
      tabe<- tabe %>%
        pivot_wider(names_from = var2, values_from = weight)
      tabe <- data.matrix(tabe)

      Total<-rowSums(tabe)
      tabe<-cbind(tabe,Total)
      Total<-colSums(tabe)
      tabe<-rbind(tabe,Total)
      var1<-to_factor(var1)
      var2<-to_factor(var2)

      names_row<-levels(var1)
      names_col<-levels(var2)

      # names_row <- rbind(names_row,"total")
      names_row<-c(names_row,"Total")
      names_col<-c(names_col,"Total")

      # Here i took first column of tabe and give it the names for names_row
      ## Omit the first column so that it can be replaced by labels
      tabe<-tabe[,-1]
      ### Names column and rows
      rownames(tabe)<-names_row
      colnames(tabe)<-names_col

    }
  }

  tabe

}


