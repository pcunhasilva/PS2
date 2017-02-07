###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 2
## Author: Patrick Cunha Silva
## Tasks: 1 - Create benford()
##        2 - Create print.benfords()
##        3 - Create save.benfords.csv()

# Generates the data to  test the three functions created with this script.
rm(list = ls())
set.seed(123)
data_vector <- round(rexp(100)*1000)
data_matrix <- matrix(round(rexp(64)*1000), nrow = 8, ncol = 8)

# benford()
# Description: computes the Leemis's m statistics and the Cho-Gains's d.
# Default: benford(x, statistic  = "Both")
# Arguments: 
#
# x             data to be used. It can be a vector or a matrix.
# statistic     an option that assumes three possible values "Both" to compute 
#               Leemis's m and  Cho-Gains's d, "d" to compute only  "Cho-Gains's d" and "m"
#               to compute only Leemis' m.
# Details:      The function returns a list contains the proportion of each number as 
#               first digit and the selected statistics.
# Author:       Patrick C. Silva.

benford <- function(x, statistic = "Both"){
   
   # Extract the first digit of each number and save them in the vector first_digit
   first_digit <- as.numeric(substr(as.character(x), 1, 1))
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
      return(list(Leemis_m = m, ChoGains_d = d, distribution = distribution))
   }
   
   # Compute Leemis' m statistic and return it, if  statistic == "m"
   if (statistic == "m"){
      m <- max(equation_term)
      return(list(Leemis_m = m, distribution = distribution))
   }
   
   # Compute Cho-Gain's d statistic and return it, if statistic == "d"   
   if (statistic == "d"){
      d <- sqrt(sum((equation_term)^2))
      return(list(ChoGains_d = d, distribution = distribution))
   }
}

# Examples:
benford(data_matrix) # Compute both statistics.
benford(data_vector, statistic = "m") # Compute only Leemis's m stastistic
benford(data_vector, statistic = "d") # Compute only Cho-Gain's d stastistic

# print.benford()
# Description: print the results of benford function as a table and 
#              add stars to inform the statistical significance 
#              of each statistic.
# Default: print.benford(x)
# Arguments:
# x         object contains the results of benford.
# Details:      The function returns a table with the statistics and stars to 
#               inform the statistical significance of each statistic.
#               The final object is a list.
# Author:       Patrick C. Silva.

print.benford <- function(x){
   # Test if the object x is a list. If not, stop and display the error message.
   if(!is.list(x)){
      stop("Please, use an object contains the outcome of benford.")     
   }
   # Test if the object x is a list. If not, stop and display the error message.
   if (is.null(x[["Leemis_m"]]) & is.null(x[["ChoGains_d"]])){
      stop("Please, use an object contains the outcome of benford.")
   }
   # Test if the object "x" contains the Leemis'm statistic
   # Create a data.frame to store the statistic and the stars.
   if (!is.null(x[["Leemis_m"]])){
      dfm <- data.frame(Estimate = x[["Leemis_m"]], 
                        Stars = c(""), 
                        row.names = c("Leemis' m"),
                        stringsAsFactors = FALSE)
      # Add starts if
      if (dfm$Estimate[1]>=0.851) {
         dfm$Stars[1] <- "*"
      } else if (dfm$Estimate[1]>=0.967) {
         dfm$Stars[1] <- "**"
      } else if (dfm$Estimate[1]>= 1.212) {
         dfm$Stars[1] <- "***"
      }
   }
   # Test if the object "x" contains the Cho-Gain's d statistic
   # Create a data.frame to store the statistic and the stars.
   if (!is.null(x[["ChoGains_d"]])){
      dfd <- data.frame(Estimate = x[["ChoGains_d"]], 
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
   if (is.null(x[["Leemis_m"]]) & is.null(x[["ChoGains_d"]])){
      stop("Please, use an object contains the outcome of benford.")
   }
   # Rename the dataframes to create a table.
   if (exists("dfm") & exists("dfd")){
      df <- rbind(dfm, dfd)
   } else if (exists("dfm") & !exists("dfd")){
      df <- dfm
   } else if (!exists("dfm") & exists("dfd")){
      df <- dfd
   } 
   # Rename the names of the dataframe collumns
   colnames(df) <- c("Estimate", "")
   # Create an explanation note.
   note <- paste("Significance Level:","'0.01'", "***", "'0.05'", "**", "'0.1'", "*")
   # Generate a list that will be used as the table.
   outcome <- list(Results = df, Note = note)
   # print the table.
   print(outcome)
}

# Examples:
print.benford(benford(data_matrix))  # Return a table with both statistics.
print.benford(benford(data_matrix, statistic = "m")) # Return a table with Leemis'm statistic.
print.benford(benford(data_matrix, statistic = "d")) # Return a table with Cho-Gain's d statistic.

# save_benford_csv()
# Description: Save the table as the one created by print.benford in a .csv file.
# Default: save_benford_csv(x, path = NULL, file.name = "benford.table")
# Arguments:
# x             object contains the results of benford.
# path          local directory used to save the file.
# file.name     name of the .csv file.
# Details:      The function saves a .csv file in the local directory that 
#               contains the a table created by print.benford. 
# Author:       Patrick C. Silva.

save_benford_csv <- function(x, path = NULL, file.name = "benford.table"){
   outputpath <- paste0(path, file.name,".csv") # Define the path.
   sink(outputpath) # Set where the file will be saved.
   print.benford(x) # Generate the table
   sink() # Save the table.
}

# Example:
save_benford_csv(benford(data_matrix), path = "~/Desktop/", file.name = "test") # Save a file in the
                                                                                 # Desktop.