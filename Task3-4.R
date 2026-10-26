#install GGally
if (!requireNamespace("GGally", quietly = TRUE)) { #auto install required package GGally
  install.packages("GGally")
}
if (!requireNamespace("corrplot", quietly = TRUE)) { #auto install required package coorplot
  install.packages("corrplot")
}
library("corrplot")
library("GGally")
data <- USArrests #load dataset

#REQUIREMENT 3
#creating boxplot for murder, assault, UrbanPop, Rape

# Calculate quartiles
rq <- quantile(data$Rape)
mq <- quantile(data$Murder)
uq <- quantile(data$UrbanPop)
aq <- quantile(data$Assault)

# Boxplots
par(mfrow = c(2, 2)) #2x2 plot

#Rape
boxplot(data$Rape, main = "Rape", col = "red")
text(x = 1.2, y = rq, labels = round(rq, 0), pos = 4, cex = 0.8) # add the quartile value to boxplot for readability
#Murder
boxplot(data$Murder, main = "Murder", col = "red")
text(x = 1.2, y = mq, labels = round(mq, 0), pos = 4, cex = 0.8)
#UrbanPop
boxplot(data$UrbanPop, main = "UrbanPop", col = "red")
text(x = 1.2, y = uq, labels = round(uq, 0), pos = 4, cex = 0.8)
#Assault
boxplot(data$Assault, main = "Assault", col = "red")
text(x = 1.2, y = aq, labels = round(aq, 0), pos = 4, cex = 0.8)



#creating histogram for murder, assault, UrbanPop, Rape
par(mfrow = c(2, 2)) # create 2x2 plot 
#Murder
murder <- hist(data$Murder, main="Murder Frequency", xlab="Murder", ylab = "Frequency", col="red", border="black", xaxt = "n", ylim = c(0, max(hist(data$Murder, plot = FALSE)$counts) * 1.25))
text(murder$mids, murder$counts, labels = murder$counts, pos = 3) # add frequency text to the bar for readability 

axis(1, at = murder$breaks) #adjust the bin labels (x axis) for readability
#Assult
assault <- hist(data$Assault, main="Assault Frequency", xlab="Assault", ylab = "Frequency", col="red", border="black", ylim = c(0, max(hist(data$Assault, plot = FALSE)$counts) * 1.25))
text(assault$mids, assault$counts, labels = assault$counts, pos = 3) # add frequency text to the bar for readability 

#urbanpop
urbanpop <- hist(data$UrbanPop, main="UrbanPop Frequency", xlab="UrbanPop", ylab = "Frequency", col="red", border="black", ylim = c(0, max(hist(data$UrbanPop, plot = FALSE)$counts) * 1.25))
text(urbanpop$mids, urbanpop$counts, labels = urbanpop$counts, pos = 3) # add frequency text to the bar for readability 

#rape
rape <- hist(data$Rape, main="Rape Frequency", xlab="Rape", ylab = "Frequency", col="red", border="black", xaxt = "n", ylim = c(0, max(hist(data$Rape, plot = FALSE)$counts) * 1.25))
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


#REQUIRMENT 4


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

