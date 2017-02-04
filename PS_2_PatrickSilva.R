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
   first_digit <- NULL # Create an empty object to store the first digit of each number
   # Extract the first digit of each number and save them in the vector first_digit
   for(j in 1:length(x)){
      first_digit[j]  <-as.numeric(unlist(strsplit(as.character(x),"")[[j]][1]))
   }
   distribution <- NULL # Generate an empty object to store the number distribution
   equation_term <- NULL # Generate an empty object to store the main component of the formulas
   # Compute the distribution of numbers and the equation term.
   for(j in unique(first_digit)){
      distribution[j] <- sum(first_digit==j)/length(first_digit) 
      equation_term[j] <- (distribution[j]-log10(1 + 1/ j))
   }
   names(distribution)<-sort(unique(first_digit)) # Assign a name to the vector of distribution.
   
   # Compute both statistics and return them, if statistic == "Both"
   if (statistic == "Both"){
      m <- max(equation_term)
      d <- sqrt(sum((equation_term)^2))
      return(list(m = m, d = d, distribution = distribution))
   }
   # Compute Leemis' m statistic and return it, if  statistic == "m"
   if (statistic == "m"){
      m <- max(equation_term)
      return(list(m = m, distribution = distribution))
   }
   # Compute Cho-Gain's d statistic and return it, if statistic == "d"   
   if (statistic == "d"){
      d <- sqrt(sum((equation_term)^2))
      return(list(d = d, distribution = distribution))
   }
}

# Examples:
benford(data_matrix)
benford(data_vector, statistic = "m")
benford(data_vector, statistic = "d")

# Generates print.benfords()
print.benfords <- function(x){
   if (!is.list(x)){
      stop("Please, use an object contains the outcome of benford.")
   }
   if (!is.null(x[["m"]])){
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
   if (!is.null(x[["d"]])){
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
   if (exists("dfm") & exists("dfd")){
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
print.benfords(seq(1,10)) 
print.benfords(benford(data_matrix))  
print.benfords(benford(data_matrix, statistic = "m"))
print.benfords(benford(data_matrix, statistic = "d"))

# Generate save_benford_csv      
save_benfords_csv <- function(x, path = NULL, file.name = "benford.table"){
   outputpath <- paste0(path, file.name,".csv")
   sink(outputpath)
   print.benfords(x)
   sink()
}

# Example:
save_benfords_csv(benford(data_matrix), path = "~/Desktop/", file.name = "test")