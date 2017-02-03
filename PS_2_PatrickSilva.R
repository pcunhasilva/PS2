###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 2
## Author: Patrick Cunha Silva
## Tasks: 1 - Create benford()
##        2 - Create print.benfords()


# Generate data to test the functions.
rm(list = ls())
set.seed(123)
data_vector <- round(rexp(100)*1000)
data_matrix <- matrix(round(rexp(64)*1000), nrow = 8, ncol = 8)

# Create the law_benford function.
# This function computes the Leemis's m statistics and the Cho-Gains's d. 
# The function contains two arguments:
# x: the data to be used. It can be a vector or a matrix.
# statistic: That can assume three values "Both" to compute m and d, "d"
# and "m".
# The function returns a list contains the proportion of each number as 
# first digit and the selected statistics.

benford <- function(x, statistic = "Both"){
   first_digit <- NULL
   for(j in 1:length(x)){
      first_digit[j]  <-as.numeric(unlist(strsplit(as.character(x),"")[[j]][1]))
   }
   distribution <- NULL
   equation_term <- NULL
   for(j in unique(first_digit)){
      distribution[j] <- sum(first_digit==j)/length(first_digit) 
      equation_term[j] <- (distribution[j]-log10(1 + 1/ j))
   }
   names(distribution)<-sort(unique(first_digit))
   
   if (statistic == "Both"){
      m <- max(equation_term)
      d <- sqrt(sum((equation_term)^2))
      return(list(m = m, d = d, distribution = distribution))
   }
   if (statistic == "m"){
      m <- max(equation_term)
      return(list(m = m, distribution = distribution))
   }
   if (statistic == "d"){
      d <- sqrt(sum((equation_term)^2))
      return(list(d = d, distribution = distribution))
   }
}

# Examples:
benford(data_matrix)
benford(data_vector, statistic = "m")

# Generates print.benfords()

# 

print.benfords <- function(x){
   if(!is.null(x[["m"]])){
      dfm <- data.frame(Estimate = x[["m"]], 
                        Stars = c(""), 
                        row.names = c("Leemis' m"),
                        stringsAsFactors = FALSE)
      if (dfm$Estimate[1]>=0.851) {
         dfm$Stars[1] <- "*"
      } else if (dfm$Estimate[1]>=0.967) {
         dfm$Stars[1] <- "**"
      } else if (dfm$Estimate[1]>= 1.212) {
         dfm$Stars[1] <- "***"
      }
   }
   if(!is.null(x[["d"]])){
      dfd <- data.frame(Estimate = x[["d"]], 
                        Stars = c(""), 
                        row.names = c("Cho-Gain's d"),
                        stringsAsFactors = FALSE) 
      if (dfd$Estimate[1]>=1.212) {
         dfd$Stars[1] <- "*"
      } else if (dfd$Estimate[1]>=1.330) {
         dfd$Stars[1] <- "**"
      } else if(dfd$Estimate[1]>=1.569) {
         dfd$Stars[1] <- "***"
      }
   }
   if(exists("dfm") & exists("dfd")){
      df <- rbind(dfm, dfd)
   } else if (exists("dfm") & !exists("dfd")){
      df <- dfm
   } else if (!exists("dfm") & exists("dfd")){
      df <- dfd
   } 
   colnames(df) <- c("Estimate", "")
   note <- paste("Significance Level:","'0.01'", "***", "'0.05'", "**", "'0.1'", "*")
   outcome <- list(Results = df, Note = note)
   print(outcome)
}



# Examples:
print.benfords(benford(data_matrix))  
print.benfords(benford(data_matrix, statistic = "m"))
print.benfords(benford(data_matrix, statistic = "d"))
