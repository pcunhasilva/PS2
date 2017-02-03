# Generate data to test the function stats_benford.
data_vector <- round(rexp(1000)*1000)
data_matrix <- matrix(round(rexp(64)*1000), nrow = 8, ncol = 8)

# Create the law_benford function.
# This function computes the Leemis's m statistics and the Cho-Gains's d. 
# The function contains two arguments:
# x: the data to be used. It can be a vector or a matrix.
# statistic: That can assume three values "Both" to compute m and d, "d"
# and "m".
# The function returns a list contains the proportion of each number as 
# first digit and the selected statistics.

law_benford <- function(x, statistic = "Both"){
   first_digit <- NULL
   for(j in 1:length(data)){
      first_digit[j]  <-as.numeric(unlist(strsplit(as.character(data),"")[[j]][1]))
   }
   distribution <- NULL
   equation_term <- NULL
   for(j in unique(first_digit)){
      distribution[j] <- sum(first_digit==j)/length(first_digit) 
      equation_term[j] <- (distribution[j]-log10(1+1/j))
   }
   names(distribution)<-c(1:9)
   
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

# Test the function with a matrix:
law_benford(data_matrix)
# Test the function with a vector and return only "m":
law_benford(data_vector, statistic = "m")