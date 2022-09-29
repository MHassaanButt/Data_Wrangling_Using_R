## Important libraries
library("stringr")
library(tidyr)
library(dataRetrieval)
library(ggplot2)
library("readxl")
library(dplyr)
library(stringr)


## 1.1 Reading Dataset and printing Summary
data <- read.csv("listings.csv")
head(data)

df<-data[,c('id', 'name', 'description', 'host_name', 'neighbourhood_cleansed', 'property_type', 'room_type', 
            'accommodates', 'bathrooms_text', 'bedrooms', 'beds', 'amenities', 'price', 'number_of_reviews', 
            'number_of_reviews_ltm', 'review_scores_rating', 'review_scores_accuracy', 'review_scores_cleanliness', 
            'review_scores_checkin', 'review_scores_communication', 'review_scores_location', 
            'review_scores_value')]
str(df)
summary(df)

## 1.2 How many listings have their name column contain
b_upr<-sum(str_detect(df$name, 'Beautiful'))
b_low<-sum(str_detect(df$name, 'beautiful'))
sum_of_beafutiful_count=b_low+b_upr
q_upr<-sum(str_detect(df$name, 'Quiet'))
q_low<-sum(str_detect(df$name, 'quiet'))
sum_of_quiet_count=q_low+q_upr
overall_sum=sum_of_beafutiful_count+sum_of_quiet_count
cat("Sum of  Both Quiet and Beautiful (in upper or lower case or mixed): ",overall_sum)

## 1.3 List the five neighbourhoods with the highest number of reviews

df_12_month<-data[data$availability_365 > 364, ]
df_12_month<-df_12_month[df_12_month$review_scores_rating > 4.8,]
ref_df<-df_12_month[,c('host_name','host_location','host_neighbourhood','review_scores_rating')]
ref_df = na.omit(ref_df)
head(ref_df)

## 1.4 Display the average price and average review score rating of each property type in Woollahra

w_df<-data[data$neighbourhood_cleansed == 'Woollahra', ]
w_df<-w_df[,c('price','review_scores_rating','property_type')]
w_df = na.omit(w_df)

ap_df<-w_df[w_df$property_type == 'Entire rental unit', ]
cat("Average review score rating of Entire rental unit", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in home', ]
cat("Average review score rating of Private room in home", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire home', ]
cat("Average review score rating of Entire home", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in rental unit', ]
cat("Average review score rating of Private room in rental unit", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire townhouse', ]
cat("Average review score rating of Entire townhouse", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire guest suite', ]
cat("Average review score rating of Entire guest suite", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire condo', ]
cat("Average review score rating of Entire condo", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in cabin', ]
cat("Average review score rating of Private room in cabin", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire villa', ]
cat("Average review score rating of Entire villa", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire cottage', ]
cat("Average review score rating of Entire cottage", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in townhouse', ]
cat("Average review score rating of Private room in townhouse", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in condo', ]
cat("Average review score rating of Private room in condo", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire guesthouse', ]
cat("Average review score rating of Entire guesthouse", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Shared room in rental unit', ]
cat("Average review score rating of Shared room in rental unit", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire bungalow', ]
cat("Average review score rating of Entire bungalow", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in loft', ]
cat("Average review score rating of Private room in loft", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire serviced apartment', ]
cat("Average review score rating of Entire serviced apartment", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Room in boutique hotel', ]
cat("Average review score rating of Room in boutique hotel", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Shared room in boutique hotel', ]
cat("Average review score rating of Shared room in boutique hotel", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room', ]
cat("Average review score rating of Private room", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Room in hotel', ]
cat("Average review score rating of Room in hotel", mean(ap_df$review_scores_rating))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Boat', ]
cat("Average review score rating of Boat", mean(ap_df$review_scores_rating))
cat("\n")

w_df <- w_df %>% separate(price, c('Currency', 'price'))
w_df = subset(w_df, select = -c(Currency) )
w_df$price = as.numeric(w_df$price)

ap_df<-w_df[w_df$property_type == 'Entire rental unit', ]
cat("Price review score rating of Entire rental unit", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in home', ]
cat("Price review score rating of Private room in home", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire home', ]
cat("Price review score rating of Entire home", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in rental unit', ]
cat("Price review score rating of Private room in rental unit", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire townhouse', ]
cat("Price review score rating of Entire townhouse", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire guest suite', ]
cat("Price review score rating of Entire guest suite", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire condo', ]
cat("Price review score rating of Entire condo", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in cabin', ]
cat("Price review score rating of Private room in cabin", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire villa', ]
cat("Price review score rating of Entire villa", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire cottage', ]
cat("Average review score rating of Entire cottage", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in townhouse', ]
cat("Price review score rating of Private room in townhouse", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in condo', ]
cat("Price review score rating of Private room in condo", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire guesthouse', ]
cat("Price review score rating of Entire guesthouse", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Shared room in rental unit', ]
cat("Price review score rating of Shared room in rental unit", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire bungalow', ]
cat("Price review score rating of Entire bungalow", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room in loft', ]
cat("Price review score rating of Private room in loft", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Entire serviced apartment', ]
cat("Price review score rating of Entire serviced apartment", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Room in boutique hotel', ]
cat("Price review score rating of Room in boutique hotel", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Shared room in boutique hotel', ]
cat("Price review score rating of Shared room in boutique hotel", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Private room', ]
cat("Price review score rating of Private room", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Room in hotel', ]
cat("Price review score rating of Room in hotel", mean(ap_df$price))
cat("\n")
ap_df<-w_df[w_df$property_type == 'Boat', ]
cat("Price review score rating of Boat", mean(ap_df$price))
cat("\n")


## 1.5  You are going to visit Sydney and want to find an accommodation

sy_df<-data[data$neighbourhood_cleansed == 'Sydney', ]
sy_df<-sy_df[,c('neighbourhood','price','bathrooms_text','amenities')]
sy_df = na.omit(sy_df)
head(sy_df,10)

## 1.6 Draw a bar chart to show the number of listing of the top 10 hosts

counts <- table(data$host_name)
barplot(counts, main="Listing of the top 10 hosts",
        xlab="Host Names")

Price <- w_df$price
h <- hist(Price,ylim=c(0,40))
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

cl_name <- attr(w_df, "variableInfo")[["parameter_nm"]]
cl_site <- attr(w_df, "siteInfo")[["station_nm"]]
ggplot(data = w_df,
       aes(x = property_type, y = price)) +
  geom_boxplot() +
  xlab("Types of Private room") +
  ylab(cl_name) +
  labs(title = cl_site)

## 2.1. Read in the 2011 dataset into a dataframe. Show the structure of the dataframe.

my_data <- read_excel("censusdata.xlsx")
colnames(my_data) <- c('Stat','Count/Percentage')
my_data = my_data[-1,]

names(my_data)

## 2.2 Investigate and fix if there are any inconsistent values in the first column.

my_data %>%
  mutate(across('Stat', str_replace, 'None', ' '))

## 2.3 . You can see that the second column contains both count and percentage. Let split the two values into 2 columns

my_data[c('Count', 'Percentage')] <- str_split_fixed(my_data$`Count/Percentage`, '/', 2)

print(my_data)

## 2.4 Each suburb have 9 rows of data. Display the number of suburbs in the dataframe

drop <- c("Count/Percentage")
df = my_data[,!(names(my_data) %in% drop)]
t1 <- df %>% group_by(Stat
) %>% slice(1)
df_new<-as.data.frame(t(t1))
print(df_new)

## 2.5  Remove the percentage column.

drop <- c("Percentage")
df = df[,!(names(df) %in% drop)]
print(df)

## 2.6 Add the year column showing the year of the data

year <- c(2011)

df_new$year <- year
data <- df_new[, c("year", "V1", "V2", "V3", "V4", "V5","V6","V7", "V8")] 
colnames(data)[2] = "br_count_0"
colnames(data)[3] = "br_count_1"
colnames(data)[4] = "br_count_2"
colnames(data)[5] = "br_count_3"
colnames(data)[6] = "br_count_4_or_more"
colnames(data)[7] = "br_count_unstated"
colnames(data)[8] = "av_per_dwelling"
colnames(data)[9] = "av_per_household"
data = data[-1,]
print(data)

## 2.7 How many regions do we have in the data?

require(tidyverse)
unique_values<- list(unique(my_data$Count))
regions=gsub('[[:digit:]]+', '', unique_values)
print(regions)

## 2.8 Define a function that takes a year

new.function <- function(a) {
  colnames(my_data) <- c('Stat','Count/Percentage')
  my_data = my_data[-1,]
  
  my_data[c('Count', 'Percentage')] <-                                           str_split_fixed(my_data$`Count/Percentage`, '/', 2)
  drop <- c("Count/Percentage")
  df = my_data[,!(names(my_data) %in% drop)]
  
  drop <- c("Percentage")
  df = df[,!(names(df) %in% drop)]
  #define new column to add
  year <- c(a)
  df_new$year <- year
  data <- df_new[, c("year", "V1", "V2", "V3", "V4", "V5","V6","V7", "V8")] 
  colnames(data)[2] = "br_count_0"
  colnames(data)[3] = "br_count_1"
  colnames(data)[4] = "br_count_2"
  colnames(data)[5] = "br_count_3"
  colnames(data)[6] = "br_count_4_or_more"
  colnames(data)[7] = "br_count_unstated"
  colnames(data)[8] = "av_per_dwelling"
  colnames(data)[9] = "av_per_household"
  data = data[-1,]
  print(data)
}

## 2.9 Call the defined function to have two dataframes for 2011 and 2016.

df1<-new.function(2011)
print(df1)
df2=new.function(2016)
print(df2)


## 2.10 . Investigate the combined dataframe 

combined <- rbind(df1, df2)
summary(combined)

## 2.11 How many houses with 2 or 3 bedrooms in 2016

df2$br_count_2 <- as.numeric(as.character(df2$br_count_2))
df2$br_count_3 <- as.numeric(as.character(df2$br_count_3))
counts=df2$br_count_2 + df2$br_count_3
cat("houses with 2 or 3 bedrooms in 2016 are:",counts[1])

## 2.12 Which region has the largest decrease in the number of 3 bed room houses from 2011 to 2016?

df_count<-df
df_count<-str_replace_all(df_count$Count, "[[:digit:]]", "")
print(df_count)















