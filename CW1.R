#Question number 3: Write a function called classify_number(x) that
#accepts one finite numeric value and classifies it as negative, zero,
#or positive. For whole numbers, also classify it as even or odd,
#treating zero as even. For non-whole numbers, report that even-or-odd 
#classification is not applicable. Return an informative error for invalid input.

classify_number <- function(x) {
  if(!is.numeric(x) || length(x) !=1 || !is.finite(x)) {
    stop("Invalid input: Group A's number must be a finite numerical value ")
  }

  if(x<0) {
  sign_classify <- "negative"
  } else if(x==0) {
  sign_classify <- "zero"
  } else {
    sign_classify <- "positive"
  }
  
  #let's check whether x is a whole number
  
  if(x==floor(x)){
    if(x%%2==0){
      groupa <- "even"
    } else {
      groupa <- "odd"
    }
  } else {
    groupa <- "even-or-odd classification is not applicable"
  }
  
  cat("Group A number is ",sign_classify, "and", groupa)
}  


#Question 4: Write a function called summarise_vector(x) that returns the total length, minimum, maximum, mean, median, and number of missing values in a numeric vector. Do not use summary(). Explain how missing values are handled, including when all values are missing.

summarise_vector<- function(x) {
  #let's check if the input is numeric or if all values are missing
  if(!is.numeric(x) && !all(is.na(x))){
    stop("X has to be a numeric value!")
  }
  
  #check total length and number of missing values
  
  total_length <- length(x)
  missing_number <- sum(is.na(x))
  
  #let's check if every value in x is missing
  if(all(is.na(x))) {
    minimum <- NA_real_
    maximum <- NA_real_
    average <- NA_real_
    middle <- NA_real_
  } else {
  minimum <- min(x, na.rm = TRUE)
  maximum <- max(x, na.rm = TRUE)
  average <- mean(x, na.rm=TRUE)
  middle <- median(x, na.rm=TRUE)
  
  }
  
  #return all results
  return(list(
    total_length = total_length,
    minimum = minimum,
    maximum = maximum,
    mean_average = average,
    middle_number = middle,
    missing_values = missing_number
  ))
}


#Missing values in R are represented by NA. In this function, is.na(x) identifies missing values, while sum(is.na(x)) counts how many missing values are present in the vector.
# When some values are missing, the function uses na.rm = TRUE in min(), max(), mean(), and median(). This tells R to ignore the missing values when calculating. But, length(x) still counts all elements, including missing values.
# When all values are missing, the function checks this using all(is.na(x)). If the condition is true, the minimum, maximum, mean, and median are assigned NA_real_, because these statistics cannot be calculated. The function still returns the total length and the number of missing values.

#test - 3
classify_number("hello")   
classify_number(8)
classify_number(-4)
classify_number(7)
classify_number(0)
classify_number(2.5)
classify_number(-3.7)

#test - 4
summarise_vector(c(1, 2, 3, 4, 5))
summarise_vector(c(-5, -3, -1))
summarise_vector(c(-2, 0, 4))
summarise_vector(10)
summarise_vector(c(4, 4, 4, 4))
summarise_vector(c(1, 2, 3, 4))
summarise_vector(c(1.5, 2.5, 3.5))
summarise_vector(c(1, NA, 3, 5))
summarise_vector(c(NA, 2, NA, 4))
summarise_vector(c(NA, NA, NA, NA))
summarise_vector(c(NA_real_, NA_real_))

# Invalid inputs: please run individually
summarise_vector(c("a", "b", "c"))
summarise_vector("hello")
summarise_vector(list(1, 2, 3))
