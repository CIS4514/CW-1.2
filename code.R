#############################################################################################
# Contribution:
  #Datonye Tamuno(27070654) - Requirment 1 and 2
  #Sandip Pradhan (24899810) - Requirment 3 and 4
  #Folasade Olukoju(27030610) - 500-700 word critical analysis
############################################################################################
#install GGally
if (!requireNamespace("GGally", quietly = TRUE)) { #auto install required package GGally
  install.packages("GGally")
}
if (!requireNamespace("corrplot", quietly = TRUE)) { #auto install required package coorplot
  install.packages("corrplot")
}
library("corrplot")
library("GGally")

data <- USArrests

# The dataset includes three columns measuring different types of violent crime rates
# and  a fourth column, UrbanPop that shows population stats 

###############################################################################################
############################ Requirement 1 ####################################################
###############################################################################################

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

###############################################################################################
############################ Requirement 2: DESCRIPTIVE SUMMARY TABLE #########################
###############################################################################################

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




###############################################################################################
############################ Requirement 3 ####################################################
###############################################################################################
#creating boxplot for murder, assault, UrbanPop, Rape

# Calculate quartiles
rq <- quantile(data$Rape)
mq <- quantile(data$Murder)
uq <- quantile(data$UrbanPop)
aq <- quantile(data$Assault)

# Boxplots
par(mfrow = c(2, 2)) #2x2 plot

#Rape
boxplot(data$Rape, main = "Rape", col = "orange")
text(x = 1.2, y = rq, labels = round(rq, 0), pos = 4, cex = 0.9) # add the quartile value to boxplot for readability (x,y) position of text
#Murder
boxplot(data$Murder, main = "Murder", col = "orange")
text(x = 1.2, y = mq, labels = round(mq, 0), pos = 4, cex = 0.9)
#UrbanPop
boxplot(data$UrbanPop, main = "UrbanPop", col = "orange")
text(x = 1.2, y = uq, labels = round(uq, 0), pos = 4, cex = 0.9)
#Assault
boxplot(data$Assault, main = "Assault", col = "orange")
text(x = 1.2, y = aq, labels = round(aq, 0), pos = 4, cex = 0.9)



#creating histogram for murder, assault, UrbanPop, Rape
par(mfrow = c(2, 2)) # create 2x2 plot 
#Murder
murder <- hist(data$Murder, main="Murder Frequency", xlab="Murder", ylab = "Frequency", col="orange", border="black", xaxt = "n", ylim = c(0, max(hist(data$Murder, plot = FALSE)$counts) * 1.25)) #xaxt = "n" prevent default x axis values from drawing
text(murder$mids, murder$counts, labels = murder$counts, pos = 3) # add frequency text to the bar for readability 

axis(1, at = murder$breaks) #adjust the bin labels (x axis) for readability
#Assult
assault <- hist(data$Assault, main="Assault Frequency", xlab="Assault", ylab = "Frequency", col="orange", border="black", ylim = c(0, max(hist(data$Assault, plot = FALSE)$counts) * 1.25))
text(assault$mids, assault$counts, labels = assault$counts, pos = 3) # add frequency text to the bar for readability 

#urbanpop
urbanpop <- hist(data$UrbanPop, main="UrbanPop Frequency", xlab="UrbanPop", ylab = "Frequency", col="orange", border="black", ylim = c(0, max(hist(data$UrbanPop, plot = FALSE)$counts) * 1.25))
text(urbanpop$mids, urbanpop$counts, labels = urbanpop$counts, pos = 3) # add frequency text to the bar for readability 

#rape
rape <- hist(data$Rape, main="Rape Frequency", xlab="Rape", ylab = "Frequency", col="orange", border="black", xaxt = "n", ylim = c(0, max(hist(data$Rape, plot = FALSE)$counts) * 1.25))
text(rape$mids, rape$counts, labels = rape$counts, pos = 3) # add frequency text to the bar for readability 

axis(1, at = rape$breaks) #adjust the bin labels (x axis) for readability


##################### Analysis #######################################
#Box plot
#The box plots provide information about the central tendency, variability, and potential outliers of the four variables in the USArrests dataset.
#The murder and assault box plots show considerable variation between states, but no potential outliers are identified. Lowest murder arrest is 1 and highest murder arrest is 17, 
#while most states has arrest rate between 4 and 11 per 100 thousand. 

#The urban population box plot indicates that the median urban population percentage is approximately 66%, 
#with most states having values between 54% and 78%. Minimum urban population is 32% and maximum urban population is 91%.

#The Rape box plot identifies two potential outliers above the upper whisker, representing states with unusually high rape arrest rates compared with the 
#remaining states. These observations may contribute to the right skewness observed in the Rape histogram.

#Overall,the box plots suggest that the Rape variable contains two potential outliers, while the other three variables did not.



# Murder Histogram 
# It seems to be right skewed which suggest most states having lower to moderate murder arrest rates. 
# This suggests frequency of high murder arrests decreases as the murder rate increases.
# 12 states with 2-4 murder arrest observed, which is the the highest peak.  

# Assault Histogram
# Assault frequency seems to be bimodal, a histogram with two distinct peaks.
# The highest frequency occurred in the bins 100-150 assaults in 12 states. 
# Followed by second highest frequency of 10 states with assaults range of 250-300.


# Urban population histogram
# It appears approximately symmetric, with most states concentrated between 50% and 80% urban population. 
# The highest frequency occurs in the 60–70% interval, containing 12 states. 
# The frequencies decrease towards both ends of the distribution, suggesting that most states have moderate to high levels of urbanisation.
# While relatively few have very low or very high urban population percentages.

# Rape histogram 
# It seems to be right skewed, most states concentrated in low to moderate arrests, while few states reporting higher number of 
# arrests related to rape. 15-20 arrests made in 12 states which is the highest frequency. 
# The frequencies generally decrease as the arrest rate increases, with relatively few states having high values. 
# This indicates that the distribution has a longer tail towards higher arrest rates.


###############################################################################################
############################ Requirement 4 ####################################################
###############################################################################################


# Pearson correlation
cat("Pearson correlation", "\n")
pearson <- cor(data, method = "pearson")
print(pearson) #print pearson correlation
cat("\n") #add empty line


# Spearman correlation
cat("Spearman correlation", "\n")
spearman <- cor(data, method = "spearman")
print(spearman)

#heatmaps
par(mfrow=c(1,2)) # create 1x2 plots
corrplot(pearson,
         method = "color", #create coloured map
         type = "lower", # lower triangle only to avoid displaying some duplicates 
         addCoef.col = "orange", #set correlation value color 
         tl.col = "black", #label color
         tl.srt = 45, # label text angle
         title = "Pearson Correlation Heatmap",
         mar = c(0, 0, 2, 0)) #add 2 top margin

corrplot(spearman,
         method = "color",
         type = "lower",
         addCoef.col = "orange",
         tl.col = "black",
         tl.srt = 45,
         title = "Spearman Correlation Heatmap",
         mar = c(0, 0, 2, 0))

# matrix plot
#pearson
print(ggpairs(
  data,
  title = "Pearson",
  upper = list(continuous = wrap("cor", method = "pearson")), #upper triangle show the correlation coefficient
  lower = list(continuous = "points"), #lower triangle show the scatter plot
  diag = list(continuous = "densityDiag")
))
#spearman
print(ggpairs(
  data,
  title = "Spearman",
  upper = list(continuous = wrap("cor", method = "spearman")),
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag")
))

#ref Holtz, Y. (2018). The R graph gallery – help and inspiration for R charts. [online] The R Graph Gallery. Available at: https://r-graph-gallery.com/ [Accessed 26 Oct. 2026].

########################## Analysis ###########################
# Heatmap 
# The heatmap displaying the correlation coefficient between the variables.
# The highest correlation is between assault and murder with positive 0.8 (Pearson). The Spearman correlation is slightly higher, at 0.82.
# Both Pearson and Spearman results indicate a strong positive association. Which can be seen in the scatter plot. This suggests as the number of arrests made related to assault, the number of murder related arrest goes up as well.
# The second highest correlation is between assault and rape with positive 0.67 (Pearson) coefficient. The Spearman coefficient is slightly higher, at 0.71 Which suggest the number of rape related arrest goes up with assault related arrest.
# The lowest correlation is between urban population and murder with 0.07. The Spearman correlation is also close to zero, at 0.11.
# The correlation is very weak, indicating weak association between murder and urban population. Which can be seen in the scatter plot.
# Although the Spearman coefficient is slightly higher, both methods suggest that there is little association between these variables.

# ggpairs
# This plot displaying three different types of information. 
# The scatter plot, displaying the rate of arrests of all the variables. Each dot represent a state, the y axis represent the number of arrests.
# The density curve showing how values are distributed for each variable. 
# Correlation coefficient, measuring the the correlation between two variables.
# Murder and Assault scatter plot showing a strong positive correlation, which is supported by the correlation coefficient of 0.8 (pearson) and 0.82 Spearman coefficient.
# This suggest number of murder related arrest goes up with assault related arrests.
# Murder and Urban population has the weakest positive correlation, looking at scatter plot the points is all over the place and supported by 0.07 (Pearson) correlation coefficient and 0.11 Spearman coefficient. 
# Meaning there is very weak relation between murder and urban population. 

# Pearson vs Spearman
# overall the Pearson and Spearman coefficient is very similar. Suggesting the general pattern of association is consistent between the two methods.
# Some notable difference, between assault and rape increases from 0.67 using Pearson to 0.71 using Spearman. Similarly, the correlation between Murder and Rape increases from 0.56 to 0.68.
# In conclusion, both methods identify murder and assault as the strongest relationship and murder and urban population as the weakest. 
# The overall findings are consistent, although Spearman reveals somewhat stronger associations for certain variable pairs.


