####################
#
#  Data Modelling
#
####################

# Load the required libraries

library(stringr) # for string data manipulation
library(readr)   # for data load
library(dplyr)      # for data wrangling
library(ggplot2) # for awesome graphics
library(GGally)  # extension of ggplot for nice correlation visualisations
library(rsample)    # for creating validation splits
library(recipes)    # for feature engineering
library(lubridate)  # for calculating recency
library(knitr)     # for visualising outputs in a nice table
library(ggstatsplot) # for visualising outliers
library(factoextra) # for measuring the distance in th clusters
library(pdp)       # for 1+ visualisations together
library(tidyr)     # for data cleaning
library(qdapRegex)
library(readr)    # to extract data for the app

## for geomarketing analysis

library(tmap) 
library(sf)
library(datasets)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)


## Load the data

df<-read_csv("Data/data.csv")
country_coord<-read_csv("Data/countries_geographic_coordinates.csv")


#join the 2 tables to get the geographic coordinates of the countries

df<- inner_join(df,country_coord )

# check the output table

df %>% 
  count(Country)%>%
  arrange(desc(n))

df %>% 
  count(Description)%>%
  arrange(desc(n))


############## Data preparation #################


## remove NAs and convert data types where necessary

#identify the location of NAs

which(is.na(df))

# define the count of NAs 

sum(is.na(df)) #[1] 245326

# identify the columns containing missing values

colSums(is.na(df)) 

# rename Customer Id column

df<-df%>%
  rename(CustomerId =`Customer ID`)

df%>%arrange(CustomerId)

#Invoice   StockCode Description    Quantity InvoiceDate       Price Customer ID     Country      iso_a2      iso_a3     Numeric 
#0           0        4382           0           0           0      240944           0           0           0           0 

# The missing data ids from customer ID and description column. If we remove the data with missing ID wee will get wrong insights for the total sales. If we keep it,
# we will get wrong insights for the customer behaviour. the same applies to the description variable with NAs in reference to the product performance.
# We can for now and exclude them in the different steps of data exploration and modelling. For example we can exclude the Description NAs when we explore the 
# product performance, but keep it when we explore the revenue figures.For now we will just substitute the NAs with "fake" values from the relevant data type
# so that we can continue with the data cleaning. We can set Customer ID NA values as "O" and for the Description NAs we can use "Missing". We can use the same value for
# all other irrelevant descriptions that qualify as completed transactions.


df<-df%>%replace_na(list(CustomerId = "0", Description = "Missing"))

# quick check to see the result

df %>% filter(CustomerId == "0")    #240,934
df %>% filter(Description == "Missing" )  #4,383
df %>% filter(Description == "Missing" & Price == 0) #4,383
df %>% filter(Price == 0)    # 6,191
df %>% filter(Quantity<0)    # 22,116

# All transactions with missing values are not monetised, great, we can remove them altogether with the rest of the ,non-monetised values
# e.g. by removing all prices = 0 we remove all missing descriptions


# reformat the dates field from character to date format
# rename the Customer ID column and from double to character

df$InvoiceDate<- as.Date(df$InvoiceDate,format = "%m/%d/%Y")

df$CustomerID <- as.character(df$CustomerID)

# After we clean the data we can run some basic statistics to get an idea of the main data features and their distribution. The R summary() function is a perfect tool for that purpose.
# The stats of the data set give us two main insights the time dimensions of the data the fact that there are some discrepancies in the values of the transactions.

summary(df)


# From the summary data we can see that the time span of the data is 2 years (Min:2009-12-01  -- Max.:2011-12-09);
# The summary of the data shows that there are some discrepancies in the values of the transactions. 
# There are some transactions with total product price of 0 and products with negative quantities. 
#This means that we might be dealing with refunds and discounts.
# The way we will deal with refunds and discounts in transactional data depends pretty much on the business case. 
# Are we interested in all purchases as registering the frequency of touch point with the customer. 
# Or we are interested only in the monetised transactions as adding an actual net value to the business.

 
################ Deal with descriptions ###################


# identify all descriptions that do not contain only letters


not_transaction<- df%>% filter(!grepl("[a-zA-Z]",Description))%>%
  arrange(Description) #105

glimpse(not_transaction)

not_transaction %>% filter(Price == 0)%>%
  arrange(Description) #105

# 105 transactions have description that does not contain any letters.
# these are also transactions that are not monetised, so we will get get rid of them when removing all transactions 
# with price  = 0

# take an overall look at the smallest(negative) price and quantity values


df%>%
  arrange(Price, ascending = TRUE) # we can asssume taht all the refunds were accounted for with "Adjust for bad debts"


df%>%
  arrange(Quantity, ascending = TRUE)


# quick check for seaonality in the business

df %>%
  ggplot(aes(x=InvoiceDate))+
  geom_line(stat='count')

# it seems that there is certain level of seasonality in the purchasing habits of the customer. Also it looks like the number of purchases for 
# 2010 is higher than the number of purchases for 2010

# take a look at the revenue performance over the 2 years


df %>%
  ggplot(aes(x=InvoiceDate, y= Price))+
  geom_line()

write_csv(df, "./Data/Main_Dashboard_Data.csv")

###################################### Building K-means Segmentation #######################################################

# 1. We can build customer segmentation based on RFM K-Means Clustering Model
# For the modelling to be correct, we will be interested only in revenue transactions (remove refunds) and in records with existing CustomerID

df_segments <- df%>%
  filter(CustomerId !=0 & Quantity>0 & Price >0) #781,522
  

## Features engineering 

#We want to build a RFM based segmentation of the customers. Therefore first we need to generate the values for the recency, frequency and monetary variables. 
# We can use the below equations.
# recency = the most recent date of the data set minus the maximum date per customer purchase. The lower value we get the more recent is the customer purchase
# frequency = number of distinct purchases. The higher is the value here the more often the customer interacts with the business
# monetary = the sum of the total spent per customer. The higher is the value here the more valuable is the customer for the business
# These variables together with the customer id will form the data frame for our clusters

summary(df_segments)

grouped_df <- df_segments %>% 
  group_by(CustomerId) %>% 
  summarise(recency = as.numeric(as.Date("2011-12-09")-max(InvoiceDate)),
            frequency = n_distinct(Invoice), 
            total_value = round(sum(Quantity*Price)/n_distinct(Invoice),1))


glimpse(grouped_df) # 5,813


# K- Means is calculated based on the distances between the data points. 
# This makes it sensitive to the presence of outliers. 
# Running a summary statistics of the data set will help us define whether there are any extreme values in the model features. 
# Again the quickest way to do that is to use the summary() function. 


summary(grouped_df) 


#The summary statistics show that we have outliers in the frequency and monetary variables. 
# For the purpose of getting the clustering right we need to remove these outliers. 
# However, we should not get completely rid of them as they represent our most valuable customers. 
# These customers can even be defined as our loyal customers - high level of interaction an purchasing value. 
# We can mark them as a separate segment and bring them back when analysing the segments.
# To remove the outliers we need to identify a numerical cut-off range that differentiates an outlier from a non-outlier. 
# We will use the [1.5*iqr rule](https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data/box-whisker-plots/a/identifying-outliers-iqr-rule) for that purpose.
# We can calculate the interquartile range by finding first the quantiles.
# Here we use the quantile() function to find the 25th and the 75th percentile of the dataset and the IQR() function, which gives the difference between the 75th and 25th percentiles.



Q_value     <- quantile(grouped_df$total_value, probs=c(.25, .75), na.rm = FALSE)
Q_frequency <- quantile(grouped_df$frequency, probs=c(.25, .75), na.rm = FALSE)

iqr_value     <-IQR(grouped_df$total_value)
iqr_frequency <-IQR(grouped_df$frequency)
 

# Then we calculate the upper and lower range limits of our data:
  
#  **upper** <-  Q3+1.5*iqr
#  **lower** <- Q1-1.5*iqr 

# We can use the subset() function to apply the 1.5*iqr rule and remove the outliers from the data set


grouped_df<- subset(grouped_df, grouped_df$total_value > (Q_value[1] - 1.5*iqr_value) 
                    & grouped_df$total_value < (Q_value[2]+1.5*iqr_value))

grouped_df<- subset(grouped_df, grouped_df$frequency > (Q_frequency[1] - 1.5*iqr_frequency) 
                    & grouped_df$frequency < (Q_frequency[2]+1.5*iqr_frequency))

## now you can extract a .csv of the cleaned and scaled data to use it latter in your shiny app



write_csv(grouped_df, "./Data/KMeans_Data.csv") ## 3725


# Now we can check whether our values range looks more evenly spread. Visualise the range by using a boxplot visualisation


grouped_df%>%
  ggplot(aes(total_value, recency ))+
  geom_boxplot()

grouped_df%>%
  ggplot(aes(total_value,frequency))+
  geom_boxplot()



###### Scaling (normalising) the data 

# The next step is to normalise the variables to a state where they have a mean = 0 and a variance = 1. 
# For this we will use the R scale() function. This step is important as we want the K - Means algorithm to weight equally the variables when creating the clusters.


grouped_df_scale <- 
  grouped_df %>%
  subset(select = - CustomerId) %>%
  scale()

## now you can extract a .csv of the cleaned and scaled data to use it latter in your shiny app




# Now we can use a heatmap to identify the existence of any correlations between the variables. 
# This step is important as any strong correlations will lead to bias when building the model. 


ggcorr(grouped_df, method =  c("everything", "pearson"),label = TRUE, label_size = 5, label_round = 2) 



# We have only 3 variables. Out of them only recency and frequency show negative correlation. 
# This correlation should actually be read as positive as we already mentioned the recency value is measured in an opposite way, e.g. the lower the better.
# Another observation is that the more frequent transactions have higher value, which kind of make sense. We can take a closer look at these correlations.


grouped_df%>%
  ggplot(aes(recency,frequency))+
  geom_col()

grouped_df%>%
  ggplot(aes(frequency,total_value))+
  geom_smooth(method='lm', formula= y~x)


grouped_df %>%
  ggplot(aes(recency,total_value))+
  geom_smooth(method='lm', formula= y~x)

# To sum it up, it turns out that the more recent transactions are more frequent and have higher value. 
# This is already an anomaly in the customer behaviour. 
# It might be due to a very successful marketing campaign or some sort of seasonality of the business or both.


######################## Building the K means model

### Choose a method to measure the distances between the data points

# The main objective of K-means is to group the data into uniform sets of similar observations that are distinguishable from other sets of similar observations. 
# The similarity and dissimilarity between the observations is defined by computing the distances between the data points. 
# There are [different ways](https://machinelearningmastery.com/distance-measures-for-machine-learning/) of measuring the distances between data points.
# The Euclidean method is the only method that [works](https://stats.stackexchange.com/questions/81481/why-does-k-means-clustering-algorithm-use-only-euclidean-distance-metric) with K-means. 
#Therefore, the Euclidean distance between the data points will define our clusters.


### Define the number of clusters

# The next step is to specify the optimal number of clusters we can split the data on. 
# First we can try to find the number of clusters manually just to get a feeling of how the K-Means work.Then we can automate this process by using an algorithm.
# For the manual calculation we can  use kmeans() function. Let`s calculate and visualise our options with clusters from 2 to 8. 
# The "centres" part of the function defines the number of clusters while the "nstart" defines the number of times the data will be reshuffled to find the most cohesive cluster given the set up. 
# Here we will set the nstar = 25. This means that R will run with 25 random assignments and will select the one with the lowest within cluster variation. 



k2 <- kmeans(grouped_df_scale, centers = 2, nstart = 25)

k3 <- kmeans(grouped_df_scale, centers = 3, nstart = 25)

k4 <- kmeans(grouped_df_scale, centers = 4, nstart = 25)

k5 <- kmeans(grouped_df_scale, centers = 5, nstart = 25)

k6 <- kmeans(grouped_df_scale, centers = 6, nstart = 25)

k7 <- kmeans(grouped_df_scale, centers = 7, nstart = 25)

k8 <- kmeans(grouped_df_scale, centers = 8, nstart = 25)


# to view the results

viz2 <- fviz_cluster(k2, geom = "point", data = grouped_df_scale) + ggtitle("k = 2")
viz3 <- fviz_cluster(k3, geom = "point", data = grouped_df_scale) + ggtitle("k = 3")
viz4 <- fviz_cluster(k4, geom = "point", data = grouped_df_scale) + ggtitle("k = 4")
viz5 <- fviz_cluster(k5, geom = "point", data = grouped_df_scale) + ggtitle("k = 5")
viz6 <- fviz_cluster(k7, geom = "point", data = grouped_df_scale) + ggtitle("k = 6")
viz7 <- fviz_cluster(k7, geom = "point", data = grouped_df_scale) + ggtitle("k = 7")
viz8 <- fviz_cluster(k8, geom = "point", data = grouped_df_scale) + ggtitle("k = 8")

grid.arrange( viz2, viz3, viz4, viz5, viz6, viz7, viz8, nrow=2)


#There are different automated ways to define the optimal number of cluster. 
# We will compare the output of 2 methods - the Elbow and the Average Silhouette Method. 
#For both of them use the fviz_nbclust() function.

# Elbow method - the basic idea of this method is that the total inter cluster variation,e.g. the total within cluster sum of squares is minimised. 



set.seed(123)
fviz_nbclust(grouped_df_scale, kmeans, method = "wss")



# Average Silhouette Method** - measures the quality of the clustering, how well each object lies within its cluster


set.seed(123)
fviz_nbclust(grouped_df_scale, kmeans, method = "silhouette")



#Both of the methods suggested that the best number of clusters is 4. 
#Let`s apply this result in a final K-means calculation. Use the set.seed() function to get reproducible result. 
#Remember from above that the K means uses random reshuffling of the observations to get the optimal cluster split.


set.seed(123)
K_final <- kmeans(grouped_df_scale, 4, nstart = 25)
viz_final <- fviz_cluster(K_final, geom = "point", data = grouped_df_scale) + ggtitle("k final")

viz_final


# After running the algorithm we have got 4 clusters with pretty much similar sizes apart from 2, which is about double the size of the rest of the clusters.


###### Segment Analysis

# As a beginning we can get some basic figures describing the size of the segments and the way they score within the RFM model.
# The size we can define by the number of observations assigned to each cluster. 
# While the mean value of each feature (recency, frequency and monetary) will give us an idea of which are our best and worst performing segments.
# We can get these figures by running a summary table of the clustered data using summarise_all() function. 
# This function works only with numeric data. Therefore before running it we will need to exclude the customer id column as it has character format. 
# After we get our summary table we will transpose it to facilitate its visualisation.


# exclude the customer id column

summary_K <- grouped_df%>%
  subset(select = -CustomerId)%>%
  mutate(Cluster = K_final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(list(n="length", mean="mean"))

summary_K<-
  subset(summary_K,select = -c(recency_n, frequency_n))%>%
  rename(segment_size = total_value_n )

total_n_customers<- sum(summary_K$segment_size)

# transpose the summary table to facilitate the visualisation
summary_K_transposed <- gather(summary_K,
                               key = "feature",
                               value = "value",
                               -Cluster)

fix(summary_K_transposed) ### visualise that

# it klooks like segment 4 is the buggest segment folllowed by segment 1; segment 2 and 3 are pretty much equal in size

#Now we can visualise the segments to determine which are the most and the least valuable ones. This will give us an idea of how and where to focus our marketing efforts.


summary_K_transposed %>%
  ggplot(aes(x=reorder(Cluster,-value),y=value, fill=as.factor(Cluster)))+
  geom_col()+
  facet_grid(.~feature)+
  labs(title="Segments performance", subtitle = paste("Total N of customers: ", sum(summary_K$segment_size)))




# From the visualisation of the summary statistics we can conclude that our most active and most recent segment is segment 2.
# Segment 1 can be defined as totally lapsed with on average two purchase made more thana year ago. 
# This segment is also the lowest in generatin total value. Unfortunately this segment is also the second biggest in size
# So it might worth the effort to try to re-engage the customers within this segment segment. 
# Our largest segment is segment 4. It is comperatively recent with last purchase about 3 month ago. the average number of purchases for the custoomers from this segment is 3, not very active
# also compared to the size of this segment the total value generated is close to the lowest. In general our worst performing segments are the biggest segments - segment! and segment 4
# If we focus our marketing efforts in re-engaging these segments, we might double the annual revenue.
# Finally segment 3 is the most interesting segment, it brings the highest average revenue, however it is very low in activity and the average recency is about 6 months ago.
# It might be worth to analyse further the reason for this segment generating double the profit of the rest of the segments. It might contain single transactions that are 
# high in value


### Geographic Profiling of the segments

# Another interesting analysis of the segments is their geographic distribution. 
# To perform this analysis we need to attach the segments to the initial data frame and get the geographic attributes of the customers. Also do not forget to deduplicate your original data set and leave only unique customer records.


clustered_df<-grouped_df%>%
  mutate(Cluster = K_final$cluster) %>%
  group_by(Cluster)


clustered_all<- inner_join(x=clustered_df ,y=df )


#get only the unique records per segment

segmented_data <-
  clustered_all%>%
  distinct(CustomerId, .keep_all = TRUE)

# We can run some initial bar chart visualisations of the segments performance per country.


segmented_data%>%
  ggplot(aes(Country, fill =as.factor( Cluster)))+
  geom_bar ()+
  coord_flip()


# We can see that the number of customers for UK is disproportionally higher compared to the rest of the countries. 
# This makes sense as the data set is from a UK based online retailer. 
# We will separate the analysis of the segment distribution for the UK from the rest of the countries to get a clear picture of the segment performance per country.



segmented_data1<-segmented_data
segmented_data2<-segmented_data

segmented_data1%>%
  filter(Country != "United Kingdom")%>%
  ggplot(aes(Country, Cluster, fill = as.factor(Cluster)))+
  geom_col()+
  labs(title="Segments performance for the rest of the world", ylab = "Cluster")

segmented_data2%>%
  filter(Country == "United Kingdom")%>%
  ggplot(aes(Cluster,fill = as.factor(Cluster)))+
  geom_bar()+
  labs(title="Segments performance for UK")


# Finally we can get a geospatial representation of the dominant segments per country.



map <- ne_countries(scale = "medium", returnclass = "sf")

map$Segment <- segmented_data$Cluster[match(map$iso_a3, segmented_data$iso_a3)]
plot(map["Segment"])



# The map gives a clear picture of the segment dominance. It can be a perfect starting point for market analysis and business performance measures.
# This type of analysis together with a demographic profiling of the segments 
# will generate enough information for the business to build hypothesis about needed improvements in the product features or marketing communications. 

### We can conclude by saying that K-Means clustering is a great initial point for understanding the customer base, 
### improving marketing communications and most of all testing different features development of the business product and services.