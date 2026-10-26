# QUESTION 2: DESCRIPTIVE SUMMARY TABLE

# 1. Created a function to calculate 
# the min, max, mean, median, standard deviation
# and interquartile range for a single column
calculate_stats <- function(column) {
  c(
    Min    = min(column),
    Max    = max(column),
    Mean   = mean(column),
    Median = median(column),
    SD     = sd(column),
    IQR    = IQR(column)
  )
}

# 2. Applied the calculate function across the 4 target columns
# 'sapply' runs the function on each column
summary_table <- sapply(USArrests[ c("Murder", "Assault", "UrbanPop", "Rape")],calculate_stats)


print(summary_table)

# Result:
#           Murder   Assault UrbanPop
# Min     0.80000  45.00000 32.00000
# Max    17.40000 337.00000 91.00000
# Mean    7.78800 170.76000 65.54000
# Median  7.25000 159.00000 66.00000
# SD      4.35551  83.33766 14.47476
# IQR     7.17500 140.00000 23.25000
#           Rape
# Min     7.300000
# Max    46.000000
# Mean   21.232000
# Median 20.100000
# SD      9.366385
# IQR    11.100000

# rounding up the table so its easier to read

summary_table_clean <- round(summary_table, digits = 2)
print(summary_table_clean)

# in order to identify any features,ill use some visualisation

# feature 1 - Center and Spread

boxplot(USArrests, col = "blue", main = "Visualizing Center & Spread")
# This creates a simple boxplot comparing all columns side-by-side

# If you look at the boxplot, 
# the Assault column completely taller than everything else.
# It has a lot of numbers and a huge spread, while Murder sits at the bottom.

# Feature 2 - comparing the mean and median to check for possible skewness
# To check for skewness using just numbers you can compare the mean and the median of the individual columns

# For the crimes (Murder, Assault, Rape), the average is higher than the middle number. 
# That tells us that a few states that are high in crime rates are pulling the average number up, making the data skew to a side.
# For Urban population the numbers look balanced because the average and middle are almost identical.

