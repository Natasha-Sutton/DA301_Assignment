## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)

# Import the data set from the reviews files
sales <- read.csv("turtle_sales.csv", header=TRUE)

# Find any missing values
sales[is.na(sales)]

# sum of missing values
sum(is.na(sales))

# Remove rows with missing data
sales <- na.omit(sales)

# sum of missing values
sum(is.na(sales))

# Explore the data
head(sales)
dim(sales)
summary(sales)

# Print the data frame.
as.tibble(sales)

# Create a new data frame from a subset of the sales data frame.

# Remove unnecessary columns. 

salesClean <- select(sales, -Ranking)

# View the data frame.
as.tibble(salesClean)

# View the descriptive statistics.
summary(salesClean)

# Change the product Id to character
salesClean$Product <- as.character(salesClean$Product)

# View the data
as.tibble(salesClean)

# Confirm class of Product
class(salesClean$Product)

# Create a new column for 'Other_sales' which is the difference between the 
# Global sales and North Americana and European sales combined.
# Other_Sales represent the rest of the world sales.
# Mutate create a new column, Other_sales then the pipe function positions it 
# after EU_sales

salesClean <- salesClean %>% mutate(salesClean, Other_Sales=Global_Sales-
                                      (NA_Sales+EU_Sales),
                              .after= 'EU_Sales')

# View new dataset
as.tibble(salesClean)

# Save the data
write.csv(salesClean, "sales_clean.csv")

# Print the data frame.
as.tibble(salesClean)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
# 1. Scatterplot NA Sales
qplot(Product, NA_Sales, colour = Platform, data=salesClean)

# 2. Scatterplot EU Sales
qplot(Product, EU_Sales, colour = Platform, data=salesClean)

# 3. Scatterplot Other Sales
qplot(Product, Other_Sales, colour = Platform, data=salesClean)

# 4. Scatterplot Global Sales
qplot(Product, Global_Sales, colour = Platform, data=salesClean)



## 2b) Histograms
# Create histograms.

# 1. Product histogram
qplot(Product, data=salesClean, bins = 20)

# 2. Order platform in ascending order
# platform_asc <- salesClean[order(salesClean$Platform),]

# 3. Platform histogram
qplot(Platform, data=salesClean)

# 4. year histogram
qplot(Year, data=salesClean)

# 5. NA Sales Histogram
qplot(NA_Sales, data=salesClean, bins = 50)

# 6. EU Sales Histogram
qplot(EU_Sales, data=salesClean)

# 7. Other Sales Histogram
qplot(Other_Sales, data=salesClean)

# 8. Global Sales Histogram
qplot(Global_Sales, data=salesClean)


## 2c) Boxplots
# Create boxplots.

# 1. Boxplot for Product, NA_sales, Platform)
qplot(Platform, NA_Sales, data=salesClean, geom='boxplot')

# 2. Boxplot for Product, EU_sales, Platform)
qplot(Platform, EU_Sales, data=salesClean, geom='boxplot')

# 3. Boxplot for Product, Other_sales, Platform)
qplot(Platform, Other_Sales, data=salesClean, geom='boxplot')

# 4. Boxplot for Product, Global_sales, Platform)
qplot(Platform, Global_Sales, data=salesClean,  geom='boxplot')


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# 1. North America, Europe and Global sales show a single highest top product is 
# Wii platform
# 2. For both EU and NA the majority of sales are <5M and Glaboal <10M,
# histograms are skewed to the right
# 3. There was a sharp increase in product releases post 2000, especially 
# in 2010 and over(2b.4)
# 4. Global sales, inclusing the three regions show one top selling product
# belonging to the wii platform 
# 5. In NA the platform NES was the strongest(other than wii) with a lot of
# products GB and X360. The weakest were PSV, PC and XB
# 6. In EU, the top platforms were wii, PS4, PS3,DS amd weakest PSV,2600, XB
# 7. The trend in Other regions was different to NA and EU DS,GB then Wii
# apart from on top wii product.
# 6. The global sales trend is similar to that of NA sales.

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.

# Install and import Tidyverse.
library(tidyverse)

# Import the data set from the sales files
salesClean2 <- salesClean


# Explore the data
head(salesClean2)
dim(salesClean2)
summary(salesClean2)

# Print the data frame.
as.tibble(salesClean2)

# Create a subset with the three sales data:North America, Europe and Global;

salesDa <- select(salesClean2, NA_Sales, EU_Sales, Other_Sales, Global_Sales)

# View the sadesDA dataset
as.tibble(salesDa)

      #################################################################
# Check output: Determine the min, max, and mean values of all the sales data


# Call the function to calculate the mean of NA, EU and Global sales
mean(salesDa$NA_Sales) 
mean(salesDa$EU_Sales) 
mean(salesDa$Other_Sales)
mean(salesDa$Global_Sales) 

# Determine the minimum and maximum value.
# North America Sales
min(salesDa$NA_Sales)   
max(salesDa$NA_Sales) 

# European Sales
min(salesDa$EU_Sales)   
max(salesDa$EU_Sales) 

# Other Sales
min(salesDa$Other_Sales)   
max(salesDa$Other_Sales)

# Global Sales
min(salesDa$Global_Sales)   
max(salesDa$Global_Sales) 


# View the descriptive statistics.
# Create a dataframe with the summary of the descriptive analysis

summary(salesDa)

ds_sales <- data.frame(unclass(summary(salesDa)), check.names = FALSE)

ds_sales

write.csv(ds_sales, 'sales_summary.csv')


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

salesClean2 

 Total_sales <- salesClean2 %>% 
   group_by(Product) %>% 
   summarise(total_sale_NA=sum(NA_Sales),
 total_sale_EU=sum(EU_Sales),
 total_sale_Other=sum(Other_Sales),
 total_sale_Global=sum(Global_Sales))

# View the data frame.
Total_sales

# Explore the data frame.

as.tibble(Total_sales)
head(Total_sales)
dim(Total_sales)
summary(Total_sales)

################################################################################
# 2. Explore the data set

# Determine descriptive statistics of the data set.
summary(Total_sales)
summary(Total_sales$total_sale_NA)
summary(Total_sales$total_sale_EU)
summary(Total_sales$total_sale_Other)
summary(Total_sales$total_sale_Global)

# Measure central tendencies of BMI with mean and median.
mean(Total_sales$total_sale_NA)
median(Total_sales$total_sale_NA)

mean(Total_sales$total_sale_EU)
median(Total_sales$total_sale_EU)

mean(Total_sales$total_sale_Other)
median(Total_sales$total_sale_Other)

mean(Total_sales$total_sale_Global)
median(Total_sales$total_sale_Global)

# Statistics of extreme values (max and min).
min (Total_sales$total_sale_NA)
max (Total_sales$total_sale_NA)

min (Total_sales$total_sale_EU)
max (Total_sales$total_sale_EU)


min (Total_sales$total_sale_Other)
max (Total_sales$total_sale_Other)


min (Total_sales$total_sale_Global)
max (Total_sales$total_sale_Global)

# Measure the variability 
# Range = Maximum - Minimum.
max(Total_sales$total_sale_NA)- min(Total_sales$total_sale_NA)  

max(Total_sales$total_sale_EU)- min(Total_sales$total_sale_EU)  

max(Total_sales$total_sale_Other)- min(Total_sales$total_sale_Other)  

max(Total_sales$total_sale_Global)- min(Total_sales$total_sale_Global)  

# Function to calculate Q1.
quantile(Total_sales$total_sale_NA, 0.25)  

quantile(Total_sales$total_sale_EU, 0.25) 

quantile(Total_sales$total_sale_Other, 0.25) 

quantile(Total_sales$total_sale_Global, 0.25) 

# Function to calculate Q2.
quantile(Total_sales$total_sale_NA, 0.75)   

quantile(Total_sales$total_sale_EU, 0.75) 

quantile(Total_sales$total_sale_Other, 0.75) 

quantile(Total_sales$total_sale_Global, 0.75) 

# Function to calculate IQR.
IQR(Total_sales$total_sale_NA)    

IQR(Total_sales$total_sale_EU) 

IQR(Total_sales$total_sale_Other)

IQR(Total_sales$total_sale_Global) 

# Function to determine the variance.
var(Total_sales$total_sale_NA)

var(Total_sales$total_sale_EU)

var(Total_sales$total_sale_Other)

var(Total_sales$total_sale_Global)

# Function to return the standard deviation.
sd(Total_sales$total_sale_NA) 

sd(Total_sales$total_sale_EU) 

sd(Total_sales$total_sale_Other)

sd(Total_sales$total_sale_Global) 

################################################################################

## 2b) Determine which plot is the best to compare product sales.
#################################################################################
# Create scatterplots.

# Scatterplot NA Sales
ggplot(data=Total_sales, aes(x = Product, y = total_sale_NA))+geom_point(color='red')

# Scatterplot EU Sales
ggplot(data=Total_sales, aes(x = Product, y = total_sale_EU))+geom_point(color='blue')

# Scatterplot Other Sales
ggplot(data=Total_sales, aes(x = Product, y = total_sale_Other))+geom_point(color='yellow')

# Scatterplot Global Sales
ggplot(data=Total_sales, aes(x = Product, y = total_sale_Global))+geom_point(color='green')

################################################################################
# Create histograms.

# Histogram NA Sales
ggplot(Total_sales, aes(x=total_sale_NA))+geom_histogram(bins=20, fill='red')

# Histogram EU Sales
ggplot(Total_sales, aes(x=total_sale_EU))+geom_histogram(bins=20, fill='blue')

# Histogram Other Sales
ggplot(Total_sales, aes(x=total_sale_Other))+geom_histogram(bins=20, fill='yellow')

# Histogram Global Sales
ggplot(Total_sales, aes(x=total_sale_Global))+geom_histogram(bins=20, fill='green')

################################################################################

# Create boxplots.

# Boxplot for NA Sales
# Set the data source, set and pass x. 
ggplot(Total_sales, aes(x = Product, y = total_sale_NA)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'red')  

# Boxplot for EU Sales
# Set the data source, set and pass x. 
ggplot(Total_sales, aes(x = Product, y = total_sale_EU)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'blue')  


# Boxplot for Other Sales
# Set the data source, set and pass x. 
ggplot(Total_sales, aes(x = Product, y = total_sale_Other)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'yellow') 


# Boxplot for Global Sales
# Set the data source, set and pass x. 
ggplot(Total_sales, aes(x = Product, y = total_sale_Global)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'green') 


###############################################################################
# conclusions from point 3
# 1. There are too many products, 175 for boxplot to work efficiently
# 2. The histogram show that in NA and EU there are few high perfoming products
# wheareas in Other regions the sale are from a high volume but low cost products
# 3. The scatterplots of the sales v products show that In general there are 
# same types of products that generate majority of the sales across the regions
# apart for a handful of higher performers
# The number of products are too big to see their individual performance.

##############################################################################

# 3. Determine the normality of the data set.

#------------------------------------------------------------------------------
## 3a) Create Q-Q Plots

# Q-Q plot, NA sales:
qqnorm(Total_sales$total_sale_NA)
# Add a reference line:
qqline(Total_sales$total_sale_NA, col='red')

# Q-Q plot, EU sales:
qqnorm(Total_sales$total_sale_EU)
# Add a reference line:
qqline(Total_sales$total_sale_EU, col='blue')

# Q-Q plot, Other sales:
qqnorm(Total_sales$total_sale_Other)
# Add a reference line:
qqline(Total_sales$total_sale_Other, col='purple')

# Q-Q plot, Global sales:
qqnorm(Total_sales$total_sale_Global)
# Add a reference line:
qqline(Total_sales$total_sale_Global, col='green')

# The Q-Q plots in all the plots above show that the data falls along the straight 
# line at 45 degree angle apart from at the tail ends.  

          ########################################################
## 3b) Perform Shapiro-Wilk test

# Install and import Moments.
library(moments)

# Shapiro-Wilk test - NA Sales
shapiro.test((Total_sales$total_sale_NA))
# Our p-value is <0.05,so the data is not normally distributed.

# Shapiro-Wilk test - EU Sales
shapiro.test((Total_sales$total_sale_EU))
# Our p-value is <0.05,so the data is not normally distributed.


# Shapiro-Wilk test - Other Sales
shapiro.test((Total_sales$total_sale_Other))
# Our p-value is <0.05,so the data is not normally distributed.


# Shapiro-Wilk test - Global Sales
shapiro.test((Total_sales$total_sale_Global))
# Our p-value is <0.05,so the data is not normally distributed.

# For all the top four plots of the sales data of the four regions,
# the Shapiro-wilk test showed the p values to be less
# than 0.05, which leads us to reject the null hypothesis of a normal distribution.
# The data is not normally distributed as it is heavily skewed to the right 
# due to handful of high performing products.

          #######################################################
## 3c) Determine Skewness and Kurtosis

# Check for skewness- NA Sales.
skewness(Total_sales$total_sale_NA)
# Our output suggests a positive skewness of 3.

# Check for Kurtosis - NA Sales
kurtosis(Total_sales$total_sale_NA)
# Our kurtosis value is 15.60,greater than 3,has a heavy tail and is asymmetric 


# Check for skewness- EU Sales.
skewness(Total_sales$total_sale_EU)
# Our output suggests a positive skewness of 2.9.

# Check for Kurtosis - EU Sales
kurtosis(Total_sales$total_sale_EU)
# Our kurtosis value is 16.23, greater than 3,has a heavy tail and is asymmetric 

# Check for skewness- Other Sales.
skewness(Total_sales$total_sale_Other)
# Our output suggests a positive skewness of 1.63.

# Check for Kurtosis - Other Sales
kurtosis(Total_sales$total_sale_Other)
# Our kurtosis value is 6, greater than 3,has a heavy tail and better symmetry 
# than the sales from the other two regions

# Check for skewness- Global Sales.
skewness(Total_sales$total_sale_Global)
# Our output suggests a positive skewness of 3.

# Check for Kurtosis - Global Sales
kurtosis(Total_sales$total_sale_Global)
# Our kurtosis value is 17.79, greater than 3,has a heavy tail and is asymmetric 

# The skewness and Kurtosis evaluation on the sales of the four regions again 
# confirms the presence of asymmertric distribution due to skewness on the right
# side i.e positive skewness.  However, the sales of the other region showed a 
# better symmetry.


          ######################################################

## 3d) Determine correlation
# Determine correlation.

# Check correlation between NA and Global sales.
cor(Total_sales$total_sale_NA, Total_sales$total_sale_Global)
# Our correlation coefficient of 92% suggests a strong positive correlation.

# Check correlation between EU and Global sales.
cor(Total_sales$total_sale_EU, Total_sales$total_sale_Global)
# Our correlation coefficient of 85% suggests a strong positive correlation.

# Check correlation between Other and Global sales.
cor(Total_sales$total_sale_Other, Total_sales$total_sale_Global)
# Our correlation coefficient of 72.7% suggests a positive correlation.

# Check correlation between NA and EU sales.
cor(Total_sales$total_sale_NA, Total_sales$total_sale_EU)
# Our correlation coefficient of 62% suggests a positive correlation.

# Check correlation between NA and Other sales.
cor(Total_sales$total_sale_NA, Total_sales$total_sale_Other)
# Our correlation coefficient of 53.1% suggests a  weak positive correlation.

# Check correlation between EU and Other sales.
cor(Total_sales$total_sale_EU, Total_sales$total_sale_Other)
# Our correlation coefficient of 53.6% suggests a strong positive correlation.


# There is a strong correlation between Global sales and Na and EU sales
# There is also a positive correlation between NA and Eu sales.
# The correlation between the sales from other regions and EU and with NA is 
# positive and fairly similar.
#
###############################################################################
# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

###############################################################################

# Reshape the sales data so include different geographical regions in one column
# using the melt function

###############################################################################

# Reference: Stackoverflow
# 1. Determine Sales per Year of product release date

sales_year <- salesClean2 %>% 
  group_by(Year) %>% 
  summarise(total_sale_NA=sum(NA_Sales),
            total_sale_EU=sum(EU_Sales),
            total_sale_Other=sum(Other_Sales),
            total_sale_Global=sum(Global_Sales),
            Products=n())

# View Total_sales
as.tibble(sales_year)


# Change the columns names
colnames(sales_year) <- c("Year", "NA", "EU", "Other", "Global", "Products")

# View the header
head(sales_year)

# Install reshapre for melt function
library(reshape2)

# Reshape the total sales dataframe so all the regions are in one column
sales_Year2 <- melt(sales_year, id = c ("Year", "Products"))

# View the new dataset
 as.tibble(sales_Year2)
 
# Change the columns names
colnames(sales_Year2) <- c("Year", "Products", "Region", "Total_Sales")

# View the header
head(sales_Year2)


-----------------------------------------------------------------------------
# Charts of sales by Year and no of products
----------------------------------------------------------------------------
# reference:https://r-graph-gallery.com/ggplot2-color.html?utm_content=cmp-true,
# r-graphics.org

# Chart1: View the product sales per region per year - Chart 1
  ggplot(sales_Year2, aes(x=Year, y=Total_Sales, fill=Region))+
   # Crete a bar chart
  geom_col(position='dodge', width=0.8)+
   # Customise bar colours 
  scale_fill_manual(values=c('red', 'blue', 'darkorange', 'darkgreen'))+
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(1980, 2020, 4), "Year of Product Release") +
  scale_y_continuous(breaks=seq(0, 180, 10), "Sales (in £M)") +
  theme_bw()+
  # Place the title in the centre
  theme(plot.title = element_text(hjust = 0.5))+
  # Add a title and subtitle.
  labs(title="Product Sales per Region by Product Release Year")+
  theme(legend.position = c(0.4, 0.6), legend.justification = c(1,0), 
        legend.background =element_blank(), legend.key = element_blank())
  
  
# Install the package.
install.packages('plotly')

# Import the plotly library.
library(plotly) 
  

# reference:https://r-graph-gallery.com/ggplot2-color.html?utm_content=cmp-true,
# r-graphics.org
# View the product sales per region per year
  
# Chart 2: Create a chart of the global sales by year - interactive chart2
plot1 <- ggplot(sales_Year2, aes(x=Year, y=Total_Sales, fill=Region))+
  geom_col(position='dodge', width=0.8)+
  scale_fill_manual(values=c('red', 'blue', 'darkorange', 'darkgreen'))+
# Add labels and change axes marks.
scale_x_continuous(breaks=seq(1980, 2020, 4), "Year of Product Release") +
  scale_y_continuous(breaks=seq(0, 180, 10), "Sales (in £M)") +
  theme_bw()+
  # Place the title in the centre
  theme(plot.title = element_text(hjust = 0.5))+
  # Add a title and subtitle.
  labs(title="Product Sales per Region by Product Release Year")+
  theme(legend.position = c(0.4, 0.6), legend.justification = c(1,0), 
        legend.background =element_blank(), legend.key = element_blank())
ggplotly(plot1)




# Chart 3: Line plot for the number of products by Year
 ggplot(sales_Year2, aes(x=Year, y=Products))+
  geom_line(size = 1.5, color="blue", group = 1)+
  theme_bw()+
  scale_x_continuous(breaks=seq(1980, 2020, 4), "Year of Product release") +
  scale_y_continuous(breaks=seq(0, 40, 5), "Products") +
  theme_bw()+
  # Add a title and subtitle.
  labs(title="Number of products Released")


# Chart 4: Line plot for the product and global sales as bar chart
# Reference:biostats.w.uib.no/overlaying a line plot and a column plot
ggplot(sales_Year2, aes(x=Year))+
    geom_col(aes(y=Total_Sales), position='dodge')+
    geom_line(aes(y=Products), color="yellow")+
  facet_grid(Region ~., scales = "free_y")
 


# Reference:biostats.w.uib.no/overlaying a line plot and a column plot and
# ggplots-boor.org

# Chart5: Variation of chart4, with regional sales and product count per Year
  ggplot(sales_Year2, aes(x=Year))+
  # Crete a bar chart
  geom_col(aes(y=Total_Sales), position='dodge')+
  # Add line for the product count
  geom_line(aes(y=Products), color="red")+
  # Create a plot for all regions using facet function
  facet_wrap(~Region)+
  # Use BW theme
  theme_bw()+
  # Center the title
  theme(plot.title = element_text(hjust = 0.5))+
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(1980, 2020, 4), "Year of Product release") +
  scale_y_continuous(breaks=seq(0, 180, 30), "Sales (in £M)") +
  # Add a title and subtitle.
  labs(title="Total Sales by Product Release Year")
  
  
  
  
# Chart6: Chart of global sale with Product line 
   ggplot(sales_Year2, aes(x=Year))+
    # Crete a bar chart
    geom_col(aes(y=Total_Sales), position='dodge', color="black", fill="green")+
    # Add line for the product count
    geom_line(aes(y=Products), color="red", size=1)+
    # Use BW theme
    theme_bw()+
    # Center the title
    theme(plot.title = element_text(hjust = 0.5))+
    # Add labels and change axes marks.
    scale_x_continuous(breaks=seq(1980, 2020, 2), "Year of Product release") +
    scale_y_continuous(breaks=seq(0, 180, 30), "Sales (in £M)") +
    # Add a title and subtitle.
    labs(title="Total Sales by Product Release Year")

   
# Showing plot1 & plot 2 side by side to show sales and product count by year
#install gridextra
install.packages("gridExtra")
library(gridExtra)
# Plot two graphs side by side
# Ref: Intr2r.com, one row 2 columns
par(mfrow = c(1,2))
plot1
plot2 
grid.arrange(plot1,plot2, ncol=2)


# 1. Total sales increase by year of product release as the number of products 
# released per year also ramped up.
# 2. Trend of sales of NA is very similar that to the global trend.

-------------------------------------------------------------------------------
# Chart of percentage sale Across the region
-------------------------------------------------------------------------------


# Determine the percentage sales contributing to the global sales per release year
sales_year

sales_perc <- sales_year 


# View new datasframe
head(sales_perc)

sales_perc <- sales_perc %>% mutate(sales_perc, NA_perc = (`NA`/Global)*100,
                                     EU_perc = (EU/Global)*100,
                                     Other_perc = (Other/Global)*100)
# View the updated dataset
sales_perc

# Remove Global sale values
sales_perc1 <- select (sales_perc, -Global, -`NA`, -EU, -Other)

# View the dataset
sales_perc1

# Change the columns names
colnames(sales_perc1) <- c("Year", "Products", "NA", "EU", "Other")

# View the dataset
sales_perc1

# Reshape the total sales dataframe so all the regions are in one column
sales_perc2 <- melt(sales_perc1, id = c ("Year", "Products"))

# View the dataset
sales_perc2

# Change the columns names
colnames(sales_perc2) <- c("Year", "Products", "Region", "Perc_Sales")

# View the changes
sales_perc2

# Refernce: changing point shapes-www.sthda.com,
# WWW. Stackoverflow, removing size from legend
# Change legend position:www.statology
# Change legend label:Statology

# Chart 7: Plot the percentage Sales per released year 
ggplot(sales_perc2, aes(x=Year, y=Perc_Sales, group=Region))+
 # Add line for the region
  geom_line(aes(color=Region))+
  # Add a point key for the three regions
  geom_point(aes(shape=Region, color=Region, size=I(5))) +
  # Make the legend keys bigger
  guides(color=guide_legend(override.aes = list(size=3)))+
 # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(1980, 2020, 4), "Year of game release") +
  scale_y_continuous(breaks=seq(0, 100, 10), "Sales (Percentage %)") +
  # Change the theme to balck and white
  theme_bw()+
  # Place the title in the centre
  theme(plot.title = element_text(hjust = 0.5))+
# Add a title and subtitle.
  labs(title="Percentage of Global Sales per Region by Game Release Year")+
  # Change the legend positioning
theme(legend.position = c(0.5,0.9), legend.direction = "horizontal",
      legend.background =element_blank(), legend.key = element_blank())


## 1.When the gaming industry began in 1982, NA dominated the sales
# 2. Although the market is expanding the sales from EU are increasing steadily
# 3. There is also a good demand for products on other regions
# 4. The sales of NA and EU markets are almost converging
-------------------------------------------------------------------------------

# Charts -  TOp Performing Genre globally and across all regions &
  
  
------------------------------------------------------------------------------
# Determine Sales per Genre 

sales_genre <- salesClean2 %>% 
  group_by(Genre) %>% 
  summarise(total_sale_NA=sum(NA_Sales),
            total_sale_EU=sum(EU_Sales),
            total_sale_Other=sum(Other_Sales),
            total_sale_Global=sum(Global_Sales),
            Products=n())

# View Total_sales
as.tibble(sales_genre)


# Change the columns names
colnames(sales_genre) <- c("Genre", "NA", "EU", "Other", "Global", "Products")

# View the header
head(sales_genre)

# Install reshapre for melt function
library(reshape2)

# Reshape the total sales dataframe so all the regions are in one column
sales_genre2 <- melt(sales_genre, id = c ("Year", "Products"))

# View the new dataset
as.tibble(sales_genre2)

# Change the columns names
colnames(sales_genre2) <- c("Genre", "Products", "Region", "Total_Sales")

# View the header
head(sales_genre2)

# Refernece: Reorder-bookdown.org
# Chart9: View the product sales per region per Genre - Chart 5
 ggplot(sales_genre2, aes(x=reorder(Genre,-Total_Sales),
                         y=Total_Sales, fill=Region))+
  # Crete a bar chart
  geom_col(position='dodge', width=0.8)+
  # Customise bar colours 
  scale_fill_manual(values=c('red', 'blue', 'darkorange', 'darkgreen'))+
  # Add labels and change axes marks
  scale_y_continuous(breaks=seq(0, 300, 50), "Sales (in £M)") +
  theme_bw()+
  # Place the title in the centre
  theme(plot.title = element_text(hjust = 0.5))+
  # Add a title and subtitle.
  labs(title="Sales Performance by Genre Accross All Regions", x = "Genre")+
  theme(legend.position = c(0.9, 0.6), legend.justification = c(1,0), 
        legend.background =element_blank(), legend.key = element_blank())

 
 
 # Chart 10: Global Sales Performance by Genre and number of Products released
 ggplot(sales_genre2, aes(x=reorder(Genre,-Total_Sales),
                          y=Total_Sales))+
   # Crete a boxplot chart
   geom_boxplot(fill='darkgreen', outlier.color='darkgreen')+
   # Add line for the product count
   geom_point(aes(y=Products), color="red", size=3)+
   # Add labels and change axes marks
   scale_y_continuous(breaks=seq(0, 300, 50), "Sales (in £M)") +
   theme_bw()+
   # Place the title in the centre
   theme(plot.title = element_text(hjust = 0.5))+
   # Add a title and subtitle.
   labs(title="Global Sales Performance by Genre and no. of Products Released ",
        x = "Genre")+
   theme(legend.position = c(0.9, 0.6), legend.justification = c(1,0), 
         legend.background =element_blank(), legend.key = element_blank())
 

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Chart for Percentage sale distribution per region across the genre
 
#view dataframe
 sales_genre
 
# Change name of dataframe and drop products
 genre_perc <- select(sales_genre, -Products)
 
 
 # View the new dataframe
 head(genre_perc)
 
 # Reshape the dataframe
genre_perc2 <- melt(genre_perc, id = c ("Genre"))

 # View the updated dataset
genre_perc2
 
 # Change the columns names
 colnames(genre_perc2) <- c("Genre", "Region", "Sales")
 
 # View the dataset
 genre_perc2
 
 # Reference: Calculate percent by Group in R
 # https://www.youtube.com/watch?v=HcttxmymP8U
 
# Calculate percentages per region
 genre_perc3 <- genre_perc2 %>% 
   group_by(Region) %>%
   mutate(Perc_Sales = Sales/sum(Sales)*100) %>%
   as.data.frame()
 
 # View the Dataframe for the percentages
 head(genre_perc3)
 
 
 # Refernce: changing point shapes-www.sthda.com,
 # WWW. Stackoverflow, removing size from legend
 # Change legend position:www.statology
 # Change legend label:Statology
 
# Chart 11: Plot the percentage Sales by Genre per Region 
 ggplot(genre_perc3, aes(x=reorder(Genre,-Perc_Sales),
                          y=Perc_Sales, fill=Region))+
   # Crete a bar chart
   geom_col(position='dodge', width=0.7)+
   # Customise bar colours 
   scale_fill_manual(values=c('red', 'blue', 'darkorange', 'darkgreen'))+
   # Add labels and change axes marks
   scale_y_continuous("Percentage Sales (%)") +
   theme_bw()+
   # Place the title in the centre
   theme(plot.title = element_text(hjust = 0.5))+
   # Add a title and subtitle.
   labs(title="Percentage Sales by Genre per Region", x = "Genre")+
   theme(legend.position = c(0.9, 0.6), legend.justification = c(1,0), 
         legend.background =element_blank(), legend.key = element_blank())
 
 # 1.Percentage sale by Genre per region shows that each region has a different
 # popularity to the types of products.
 # 2. In NA the order of preference is:Shooter, Platform, Action, Role-Playing
 # 3. In EU the Preference: Shooter=Action=Sports
 # 4. In Other regions the preference is: Role-Playing, Platform, Action
 # 5. Action is in the top three fo all regions
 # 6. Shooter is the most popular overall led by NA sales
 
 #---------------------------------------------------------------------------
# Charts of the performance of Platforms
 #------------------------------------------------------------------------------
 
 # Sales Performance of the Platforms
 
 # View dataframe salesClean2
 head(salesClean2)
 
 # Remove Year, Publisher, Genre, 
 sales_platform <- select(salesClean2, -Year, -Genre, -Publisher)
 
 
 # View the amended dataframe
 head(sales_platform)
 
 
 # Determine Sales per Platform
 sales_platform2 <- sales_platform %>% 
   group_by(Platform) %>% 
   summarise(total_sale_NA=sum(NA_Sales),
             total_sale_EU=sum(EU_Sales),
             total_sale_Other=sum(Other_Sales),
             total_sale_Global=sum(Global_Sales),
             Products=n())
 
 # View Total_sales
 as.tibble(sales_platform2)
 
 
 # Change the columns names
 colnames(sales_platform2) <- c("Platform", "NA", "EU", "Other", "Global", "Products")
 
 # View the header
 head(sales_platform2)
 
 # Install reshapre for melt function
 library(reshape2)
 
 # Reshape the total sales dataframe so all the regions are in one column
 sales_platform3 <- melt(sales_platform2, id = c ("Platform", "Products"))
 
 # View the new dataset
 as.tibble(sales_platform3)
 
 # Change the columns names
 colnames(sales_platform3) <- c("Platform", "Products", "Region", "Total_Sales")
 
 # View the header
 head(sales_platform3)
 
 # Refernece: Reorder-bookdown.org
 # Chart 12: View the product sales per region per Genre 
 ggplot(sales_platform3, aes(x=reorder(Platform,-Total_Sales),
                          y=Total_Sales, fill=Region))+
   # Crete a bar chart
   geom_col(position='dodge', width=0.8)+
   # Customise bar colours 
   scale_fill_manual(values=c('red', 'blue', 'darkorange', 'darkgreen'))+
   # Add labels and change axes marks
   scale_y_continuous(breaks=seq(0, 300, 50), "Sales (in £M)") +
   theme_bw()+
   # Place the title in the centre
   theme(plot.title = element_text(hjust = 0.5))+
   # Add a title and subtitle.
   labs(title="Sales Performance by Platform Accross All Regions", x = "Platform")+
   theme(legend.position = c(0.9, 0.6), legend.justification = c(1,0), 
         legend.background =element_blank(), legend.key = element_blank())
 
 
 
 # Global sales by Platform and products released
 # Chart 13: Global sale by Platform
 ggplot(sales_platform3, aes(x=reorder(Platform,-Total_Sales),
                          y=Total_Sales))+
   # Crete a geom_boxplot chart
  geom_boxplot(fill = 'gold',
                outlier.color = 'gold') +
   # Add line for the product count
   geom_point(aes(y=Products), color="red", size=3)+
   # Add labels and change axes marks
   scale_y_continuous(breaks=seq(0, 300, 50), "Sales (in £M)") +
   theme_bw()+
   # Place the title in the centre
   theme(plot.title = element_text(hjust = 0.5))+
   # Add a title and subtitle.
   labs(title="Global Sales Performance by Platform and no. of Products Released ",
        x = "Platform")+
   theme(legend.position = c(0.9, 0.6), legend.justification = c(1,0), 
         legend.background =element_blank(), legend.key = element_blank())

 
 # 1. The platforms Wii, X360, PS3 and DS are the top performing globaly
 # 2. Ds equally prefered across all regions
 # 3. PS4 and PS3 more poplular in Europe
 # 4. X360 had the most number of product releases

 
 
 
 
 
 #-----------------------------------------------------------------------------
# charts -  Regional and global sales per product with Year and Platform
#------------------------------------------------------------------------------
# Chart to View Global sales by Product and Platform
 
 # View salesclean2
 head(salesClean2)
 
sales_product <- salesClean2 %>% 
  group_by( Product, Year, Platform) %>% 
  summarise(Total_NA = sum(NA_Sales),
            Total_EU=sum(EU_Sales),
            Total_Other=sum(Other_Sales),
            Total_Global=sum(Global_Sales))

 # View the Dataframe for the percentages
 sales_product

 # Refernce: changing point shapes-www.sthda.com,
 # WWW. Stackoverflow, removing size from legend
 # Change legend position:www.statology
 # Change legend label:Statology
 
 # Chart 14:Global Sales per product with Platform, Interactive - Chart 10

 P1 <- ggplot(data=sales_product, mapping=aes(x=Product, y=Total_Global,
                                             color=Platform))+
  # Change alpha values
 geom_point(alpha=1, size =1)+
  # Add labels and change axes marks
  scale_y_continuous("Total Sales (£M)") +
  theme_bw()+
  # Place the title in the centre
  theme(plot.title = element_text(hjust = 0.5))+
  # Add a title and subtitle.
  labs(title="Global Sales of Product by Platform", x = "Products")
  #theme(legend.position = "top", legend.justification = c(1,0), 
   #     legend.background =element_blank(), legend.key = element_blank())

ggplotly(P1)

# Top performing product belongs to the Wii platform

#-----------------------------------------------------------------------------
# Chart to visualise the regional sales per product
#-----------------------------------------------------------------------------
# Determine Regional and Global Sales per Product 

# View dataset sales_product, top 50 selling products
print(sales_product[order(sales_product$Total_Global, decreasing=TRUE),], n=50)


# Reshape the dataframe
sales_product1 <- melt( sales_product, id = c ("Year", "Product", "Platform"))

# View the updated dataset
sales_product1

# Change the columns names
colnames(sales_product1) <- c("Year", "Product", "Platform", "Region", "Sales")

# View the dataset
head(sales_product1)

# View dataset sales_product, top 50 selling products
head(sales_product1[order(sales_product1$Sales, decreasing=TRUE),], n=50)

# Chart 15: Regional and Global Sales per Product
P2 <- ggplot(sales_product1, aes(x = reorder(Product, -Sales), y=Sales, 
                                 group=Region)) +
  # Add line for the region
  geom_line(aes(color=Region))+
  # Add a point key for the three regions
  geom_point(aes(shape=Region, colour=Region, size=I(1))) +
  # Make the legend keys bigger
 # guides(color=guide_legend(override.aes = list(size=3)))+
  # Add labels and change axes marks
  scale_y_continuous("Total Sales (£M)") +
  # Change the theme to black and white
 theme_bw()+
  # Place the title in the centre
  theme(plot.title = element_text(hjust = 0.5))+
  # Add labels and change axes marks.
  scale_y_continuous(breaks=seq(0, 70, 5), "Sales (in £M)")+
  # Add a title and subtitle.
  labs(title="Global_Sales per Product", x = "Products") +
  # Change the legend position
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
        legend.background =element_blank(), legend.key = element_blank()) +
  coord_cartesian(xlim=c(0,175))
 
ggplotly(P2)

# Sales of some products is highest in NA
# Majority of the products sales of NA and EU overlap with other regions 
# following closely behind
#------------------------------------------------------------------------------
 
 # Chart 16: to visualise Global sales per product with release year
 
P3 <- ggplot(sales_product1, aes(x = reorder(Product, -Sales), y=Sales, 
                            group=Year)) +
   # Add line for the region
   # geom_line()+
   # Add a point key for release year
   geom_point(aes(colour=Year, size=I(1))) +
   # Make the legend keys bigger
   # guides(color=guide_legend(override.aes = list(size=3)))+
   # Add labels and change axes marks
   scale_y_continuous("Total Sales (£M)") +
   # Change the theme to black and white
   theme_bw()+
   # Place the title in the centre
   theme(plot.title = element_text(hjust = 0.5))+
   # Add labels and change axes marks.
   scale_y_continuous(breaks=seq(0, 70, 5), "Sales (in £M)")+
   # Add a title and subtitle.
   labs(title="Global_Sales per Product with Release Year", x = "Products") +
   # Change the legend position
   #theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
   #legend.background =element_blank(), legend.key = element_blank()) +
coord_cartesian(xlim=c(0,176))

 

ggplotly(P3)

# 1. The chart shows that most of the high performing products were released 
# a while back.
# 2. Products release early 2010 are relatively new hence sales appear low
# 3. Some older products like puzzles are no longer in demand
# 4. Product 107 is the highest ranking product and responsible for the highest 
# grossing sale


################################################################################

# 5. Observations and insights
# Your observations and insights here...
# The sales of other region has a more normal distribution than NA and EU
# The global sales are strongly correlated to NA sales then European sales and 
# lastly other regions.

#Charts: 1to6
# 1. Total sales increase by year of product release as the number of products 
# released per year also ramped up.
# 2. Trend of sales of NA is very similar that to the global trend.

# Charts:7 to 8
## 1.When the gaming industry began in 1982, NA dominated the sales
# 2. Although the market is expanding the sales from EU are increasing steadily
# 3. There is also a good demand for products on other regions
# 4. The sales of NA and EU markets are almost converging

# Charts: 9 to 11
# 1.Percentage sale by Genre per region shows that each region has a different
# popularity to the types of products.
# 2. In NA the order of preference is:Shooter, Platform, Action, Role-Playing
# 3. In EU the Preference: Shooter=Action=Sports
# 4. In Other regions the preference is: Role-Playing, Platform, Action
# 5. Action is in the top three fo all regions
# 6. Shooter is the most popular overall led by NA sales

# Charts: 12 to 13

# 1. The platforms Wii, X360, PS3 and DS are the top performing globaly
# 2. Ds equally prefered across all regions
# 3. PS4 and PS3 more poplular in Europe
# 4. X360 had the most number of product releases


# Charts: 14
# Top performing product belongs to the Wii platform

# Charts: 15 to 
# Sales of some products is highest in NA
# Majority of the products sales of NA and EU overlap with other regions 
# following closely behind

# Chart: 16

# 1. The chart shows that most of the high performing products were released 
# a while back.
# 2. Products release early 2010 are relatively new hence sales appear low
# 3. Some older products like puzzles are no longer in demand
# 4. Product 107 is the highest ranking product and responsible for the highest 
# grossing sale

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
head(salesClean)

# Determine a summary of the data frame.
summary(salesClean)


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

# Reference:Correlation plot, r-coder.com

# Remove non numeric columns

sales_lr <- select_if(salesClean, is.numeric)

# View the Dataset
head(sales_lr)

#Plot correlation Matrix
pairs(sales_lr)

# Install the psych package.
install.packages('psych')

# download library psych
library(psych)

# Plot correlation coefficient table
corPlot(sales_lr, cex=1)

# :Output

# 1. correlation between Global and NA sales - 0.94 suggests a strong positive correlation

# 2. correlation between Global and EU sales - 0.88 suggests a positive correlation

# 3. correlation between Global and Other sales - 0.82 suggests a positive correlation

# 4. correlation between NA and EU sales- 70.6% suggests a positive correlation

# 5. correlation between NA and Other sales - 64.3% suggests a  weak positive correlation

# 6. Check correlation between EU and Other sales - 67.4% suggests a  weak positive correlation.

# 7. Check correlation year and Global sales - -24% suggests a  weak negative correlation.
 
# 8. Check correlation year and NA sales - -32.5% suggests a  weak negative correlation.

# 9. Check correlation year and EU sales - -1.0% suggests no real correlation

# 10. Check correlation year and Other sales - -25.4% suggests a  weak negative correlation
###############################################################################
# 2b) Create a plot (simple linear regression) 

# Create simple linear regression visualisations for 
# for Variables, NA,EU,Other and Year on Global Sales

# Basic visualisation.

# 1.Global and NA sales
plot(sales_lr$Global_Sales, sales_lr$NA_Sales)
model_lm1 <- lm(NA_Sales~Global_Sales,
          data=sales_lr)
# View the model
model_lm1


# summary of the model
summary(model_lm1)
# Very low standard error, p is very low, high t values

# For every unit NA sales, Global sales to increase by 0.51, R squared =87.5%

# Plot the residuals
plot(model_lm1$residuals)
# Show a random scatter indicating a good fit of the model


# Add the regression line
plot(sales_lr$Global_Sales, sales_lr$NA_Sales, main="NA, Global Sales Correlation",
     xlab="Global Sales(£M)",
     ylab= "NA Sales(£M)")
abline(lm(NA_Sales~Global_Sales,
          data=sales_lr), col='red')



# 2.Global and EU sales
plot(sales_lr$Global_Sales, sales_lr$EU_Sales)
model_lm2 <- lm(EU_Sales~Global_Sales,
                data=sales_lr)
# View the model
model_lm2


# summary of the model
summary(model_lm2)
# Very low standard error, p is very low, high t values

# For every unit NA sales, Global sales to increase by 0.28, R squared =77.05%

# Plot the residuals
plot(model_lm2$residuals)
# Show a random scatter indicating a good fit of the model


# Add the regression line
plot(sales_lr$Global_Sales, sales_lr$EU_Sales)
abline(lm(EU_Sales~Global_Sales,
          data=sales_lr), col='red')



# 3.Global and OtherSales
plot(sales_lr$Global_Sales, sales_lr$Other_Sales)
model_lm3 <- lm(Other_Sales~Global_Sales,
                data=sales_lr)
# View the model
model_lm3


# summary of the model
summary(model_lm3)
# Very low standard error, p is very low, high t values

# For every unit Other region sales, Global sales to increase by 0.21,
# R squared =67.32%

# Plot the residuals
plot(model_lm3$residuals)
# Show a random scatter indicating a good fit of the model


# Add the regression line
plot(sales_lr$Global_Sales, sales_lr$Other_Sales)
abline(lm(Other_Sales~Global_Sales,
          data=sales_lr), col='red')


# 4.Global and Year
plot(sales_lr$Global_Sales, sales_lr$Year)
model_lm4 <- lm(Year~Global_Sales,
                data=sales_lr)
# View the model
model_lm4


# summary of the model
summary(model_lm4)
# Low standard error, p is very low, low t values

# For every unit Other region sales, Global sales to decrease by 0.26,
# R squared =6%, which is a very weak correlation

# Plot the residuals
plot(model_lm4$residuals)
# Show a random scatter indicating a good fit of the model


# Add the regression line
plot(sales_lr$Global_Sales, sales_lr$Year)
abline(lm(Year~Global_Sales,
          data=sales_lr), col='red')


# Similar to EU sales, the variation in the sales of other regions are not 
# seem with variations in years.
###############################################################################

# 3. Create a multiple linear regression model

# View the dataframe create above with numeric columns alone
head(sales_lr)

# Select only numeric columns from the original data frame.
# sales_mlr <- select_if(salesClean, is.numeric)

# View he amended dataset
# head(sales_mlr)

# Explore the dataset
summary(sales_lr)
dim(sales_lr)
head(sales_lr)


# Determine the correlation

# Determine correlation between variables.
cor(sales_lr)


# Visualise the correlation
# As in section 6.2
corPlot(sales_lr, cex=1)

###############################################################################

# The plot indicates the correlation between variables by colour and a
# numeric value (r). The blue colour indicates a positive, red indicates a 
# negative correlation, and white indicates no correlation.
# The heatmap shows that the global sales has strong positive correlations with:
# NA sles(94%), EU sales(88%), other sales (82%)
# and a weeak negative correlation with year (-24%)


##############################################################################

# Multiple linear regression model.

# Create a new object and 
# specify the lm function and the variables.
# The variable are NA, EU sales and the Year. As Other sales is calculated 
# i.e. Other sales = Global-(NA+EU) sales, it will not be included due to
# multicollinearity.

# For the MLR Model:The variables are: Global, NA, Eu sales and Year 

modelSales1 = lm(Global_Sales~NA_Sales+EU_Sales+Year, data=sales_lr)
# Print the summary statistics.
summary(modelSales1)

# The output shows that the r squared is 1 with all the variables


# Dropping the NA sales in the mlr
modelSales2 = lm(Global_Sales~EU_Sales+Year, data=sales_lr)

# Print the summary statistics.
summary(modelSales2)
# R=82.3%

# Dropping the EU sales in the mlr
modelSales3 = lm(Global_Sales~NA_Sales+Year, data=sales_lr)

# Print the summary statistics.
summary(modelSales3)
# R=87.9%

# Now the R square only drops 87.9%, R adjusted drops slightly, indicating a 
# stronger dependency on NA Sales than EU
# The R square drops to 82.3%, R adjusted very slightly. This shows global 
# sales are dependent on NA sales

# Dropping the sales from other regions
modelSales4 = lm(Global_Sales~NA_Sales+EU_Sales, data=sales_lr)

# Print the summary statistics.
summary(modelSales4)

# The R square 97% is high and r adjusted only drops slightly.
# than when EU sales were dropped which indicates 
# the sales from other regions has the third least impact on the global sales

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
# Reference:Statology.org/residual-plot-r



# The best model to predict would be the first model but without the year as it 
# was the least significant.  However, these values may be unreliable as the 
# global sales are essentially the addition of the three regions so others will
# be eliminated to make the model a little realistic. Dropping the sales from 
# other regions as these were just calculated values and not true values.
 
# Here the correlation R square is 96.7%

# Determine a list of residuals
modelSales4.res = resid(modelSales4)

# View the residuals
head(modelSales4.res)

# Plot the fitted vs the residuals plot 
plot(fitted(modelSales4), modelSales4.res, main="MLR-Residual Plot" )

# add a horizonal line at 0
abline(0, 0)

# The plot shows spead of residuals is fairly random, apart from a couple of points.
# This suggest the model is acceptable.

# Create a Q-Q plot for residuals
qqnorm(modelSales4.res)

# add a straight diagnol line to the plot
qqline(modelSales4.res)

# The residuals tend to stray from the line near the tails, which indicate not
# a perfect normal distribution


# Create a density plot of residuals
plot(density(modelSales4.res))
# The plot shows a bell shaped curve like a normal distribution but slightly 
# skewed to the right. As the are a few points a transformation is not required
# for this dataset.   

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Join the variables to make a dataframe of the given values
# salesPred <- data.frame(NA_Sales, EU_Sales)

#View the new dataframe
# head(salesPred)
# str(salesPred)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a new object and specify the predict function, pass through given values
# predictsales = predict(modelSales4, newdata=salesPred,
                      # interval='confidence')

# Print the object.
# predictsales

# predictTest 
# fit       lwr       upr
# 1 71.668796 70.356743 72.980850
# 2  6.858034  6.719488  6.996579
# 3  4.250722  4.103385  4.398059
# 4  4.136789  4.010304  4.263274
# 5 26.437646 25.414290 27.461002

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prediction of Global Sales with given values

# NA_Sales <- c(34.2,3.93, 2.73, 2.26, 22.08)
# EU_Sales <- c(23.8,1.56,0.65,0.97,0.52)

# First find the actual global sales values from original dataframe
# Extract the rows by the NA and EU sales values given 
sales_acc <- sales_lr[sales_lr$NA_Sales %in% c('34.02', '3.93', '2.73', 
                                              '2.26', '22.08'), ]

# View the values
head(sales_acc)

# remove year
sales_acc <- select(sales_acc, -Year)

# View the dataset
head(sales_acc)

# Predict the global sales values using Modelsales4
prediction1 = predict(modelSales4, newdata=sales_acc,
                      interval='confidence')

# Round the values to two decimal places
prediction2 = round(data.frame(prediction1), digits=2)

# Arrange the sales in descending order
prediction3 <- prediction2[order(-prediction2$fit),]

# View the new dataset
head(prediction3)

# Change the columns names
colnames(prediction3) <- c("Predicted_Sales", "Lwr", "Upr")

# View the new column names
head(prediction3)

# Create dataframe of actual and predicted sales values
Turtle_Predsales <- cbind(sales_acc, prediction3)

# View the dataframe with predicted sales
head(Turtle_Predsales)

# Save the data
write.csv(Turtle_Predsales, "Turtle_Predsales.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Check for accuracy of the predictions 

# Reference: STHDA-Regression Model Validation
# rdocumantation.org/packages/Metrics

# Install Metrics Package
install.packages("Metrics")

# Download Metrics
library(Metrics)

#  Model performance
# (a) prediction error, RMSE

# Determine the root mean square errors 
rmse(sales_acc$Global_Sales, prediction1)
# 2.21

# Calculate MAPE: Mean absolute percentage error
mape(sales_acc$Global_Sales, prediction1)
# 0.103, The mape is 10.3%, less than 10% is considered excellent so 10.3% is 
# close to being excellent

# Calculate MAE
mae(sales_acc$Global_Sales, prediction1)
# 1.66



###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The multiple linear regression provides a good model to predict global sale
# values.  

# The global sales a heavily dependent on the NA sales and secondly on the 
# European market.

# There are global economic factors that influence all three markets to a certain
# degree

# The Mean Absolute Percentage error is an indication of accuracy of predictions
# Model 4 for the MLR gives predictions of 10.5%.  According to section 6.1.3
# Course 3, Canvas according to rule of thumb, a MAPE value of less than 10%
# is considered excellent. Our MAPE is very close to 10%, thus it can be 
# considered an acceptable model for prediction of Global and thus sales of 
# Other regions.



###############################################################################
###############################################################################




