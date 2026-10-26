data <- USArrests

# The dataset includes three columns measuring different types of violent crime rates
# and  a fourth column, UrbanPop that shows population stats 

# QUESTION 1

# DISPLAYING THE FIRST FOUR ROWS
cat("====================== First four rows ========================\n")
first_four <- head(data, n=4)
print(first_four)

# DISPLAYING THE LAST THREE ROWS
cat("====================== Last three rows ========================\n")
last_three <- tail(data, n=3)
print(last_three)
# USArrests DATASET DIMENSIONS
cat("====================== Data dimention ========================\n")
print(dim(data))

# what the dimension results mean
# 50  4
# 50 Rows = The 50 unique US States included in the study.
# 4 Columns = The 4 variables being tracked per state:
  # 1. Murder (Rate) | 2. Assault (Rate) | 3. UrbanPop (%) | 4. Rape (Rate)
# Summary: we are working with  exactly 200 data points (50 x 4) 
  

#variable types
cat("====================== Variable types ========================\n")
print(sapply(data,class))


#state names and missing-value counts.
states <- rownames(data)
states

missing_counts <- rowSums(is.na(data))
missing_counts

#data frame to see both names and missing-value counts
data.frame(State = states, MissingValues = missing_counts)

cat("====================== Summary ========================\n")
print(summary(data))

# validation function
validate_data <- function(x){
  #1. check for duplicate states
  states <- rownames(x)
  any_duplicates <- any(duplicated(states))
  print(paste("Has duplicate states?:", any_duplicates))
  
  # 2. check for non-numeric columns
  # 'sapply' checks the type of every column saving time.
  # we want to see if any are NOT numeric
  numeric_check <- all(sapply(x, is.numeric))
  print(paste("Are all analysis columns numeric?:",numeric_check))
  
  #3. check for negative values
  # 'any(x < 0)' looks at the entire table at once for minus signs
  
  has_negatives <- any(x < 0, na.rm = TRUE)
  print(paste("Has negative values?:", has_negatives))
  
  # na.rm = TRUE: Tells R to ignore/remove missing values 
  # while checking ranges so the code doesn't crash if data is blank.

  #4. Check plausible ranges 
  
  # We check if any value in a specific column crosses our defined maximum limit
  bad_murder   <- any(x$Murder > 100, na.rm = TRUE)
  bad_assault  <- any(x$Assault > 1000, na.rm = TRUE)
  bad_urbanpop <- any(x$UrbanPop > 100, na.rm = TRUE)   # Cannot be over 100%
  bad_rape     <- any(x$Rape > 200, na.rm = TRUE)
  
  # Combine range checks into one true/false result
  has_out_of_bounds <- bad_murder || bad_assault || bad_urbanpop || bad_rape
  print(paste("Has values outside plausible ranges?:", has_out_of_bounds))
}
cat("========================= Data validation ==========================\n")
validate_data(data)

# DATA VALIDATION RESULTS & COMMENTARY FOR USARRESTS

# FIGURING OUT THE PLAUSIBLE RANGES
# 1. Checked the official help documentation (?USArrests): 
#    This confirmed exactly what the metrics mean. It told me that Murder, 
#    Assault, and Rape are rates calculated "per 100,000 residents", and 
#    UrbanPop is an actual "percentage".
#
# 2. ALSO ASSESSED THET DATASET USING (summary(USArrests))

# MY COMMENTARY & FINDINGS (What the results actually mean):

# I ran the validation function on the built-in USArrests dataset.

# 1. No Duplicate States (FALSE)
#
# > validate_data(USArrests)
# [1] "Has duplicate states?: FALSE"
# [1] "Are all analysis columns numeric?: TRUE"
# [1] "Has negative values?: FALSE"
# [1] "Has values outside plausible ranges?: FALSE"
#
# 4. Everything Is Within Plausible Ranges (FALSE for "has out of bounds"): 
#    - The urban population numbers stay under my 100% ceiling.
#    - The crime rates stay well below the safety caps (100, 1000, 200).
#
# CONCLUSION: 
# The dataset is clean, realistic, and free of any typos.
