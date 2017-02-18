# Uncomment the below line if you have not already installed tidyverse
# Install.packages("tidyverse") 

# Load the dplyr package
library(dplyr)





# Use hflights as an examle
# Install the hflights package
install.packages(hflights)
# Load the hflights package
library(hflights)

# hflights is a data frame representing the data set
class(hflights)

# Familar with the dataset
head(hflights)
summary(hflights)
str(hflights)
names(hflights)

# View the dataset using View
View(hflights)

# View the dataset using tbl_df
hflights <- tbl_df(hflights)
hflights

# View the dataset using glimpse
glimpse(hflights)

# Uncomment the below line if you want to change the structure back using as.data.frame
#as.data.frame(hflights)

# Create a look up table to modify the label of UniqueCarrier column
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

# Subset the look up table with the UniqueCarrier column of hflights
Carrier = lut[hflights$UniqueCarrier]

# Add the Carrier column to hflights
hflights = mutate(hflights, Carrier)

# Glimpse at hflights
glimpse(hflights)





## select()
#	select(df, var1, var2)
# select(df, 1:4, -2)

# Select four columns of hflights related to delay
select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

# Select columns Origin up to Cancelled
select(hflights, Origin:Cancelled)

# Select columns 1st up to 4th and 12th up to 21st
select(hflights, 1:4, 12:21)

#Helper Functions, use inside of select()
#starts_with("X")
#ends_with("X")
#contains("X")
#matches("X")
#num_range("x", 1:5)
#one_of(x)

# Select columns containing ¡°Delay¡± in the name
select(hflights, contains("Delay"))

# Select columns UniqueCarrier & columns containing ¡°Cancel¡± and "Num" in the name
select(hflights, UniqueCarrier, contains("Cancel"),contains("Num"))

# Select columns ends with "Time" in the name
select(hflights, ends_with("Time"))





##mutate()
#mutate(df, z = x + y)
#mutate(my_df, x = a + b, y = x + c)
g1 = mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime)

g2 = mutate(g1, GroundTime = TaxiIn + TaxiOut)

g3 = mutate(g2, AverageSpeed = Distance / AirTime * 60)

m1 = mutate(hflights, loss = ArrDelay - DepDelay, loss_ratio = loss/DepDelay)

m2 = mutate(hflights, TotalTaxi = TaxiIn+TaxiOut, ActualGroundTime = ActualElapsedTime-AirTime, Diff = TotalTaxi-AcyualGroundTime) 

# Combine the Year, Month and DayofMonth variables to create a Date column: c2
c2 = mutate(hflights, Date = paste(Year,Month, DayofMonth, sep="-") )



## filter()
# filter(df, a > 0)
#	& (and), | (or), and ! (not)
#	filter(df, a > 0 & b > 0)
# filter(df, a > 0, b > 0)
# filter(df, !is.na(x))

# All flights that traveled 3000 miles or more
filter(hflights, Distance>3000)

# All flights flown by one of JetBlue, Southwest, or Delta
filter(hflights, UniqueCarrier %in% c("JetBlue","Southwest","Delta") )

# All flights where taxiing took longer than flying
filter(hflights, TaxiOut+TaxiIn>AirTime )

# All flights that departed before 5am or arrived after 10pm
filter(hflights, DepTime <500 | ArrTime>2200)

# All flights that departed late but arrived ahead of schedule
filter(hflights, DepDelay>0 & ArrDelay<0)

# All flights that were cancelled after being delayed
filter(hflights, DepDelay>0 & Cancelled==1)

# Select the flights that had JFK as their destination: c1
c1 = filter(hflights, Dest == "JFK" )

# How many weekend flights flew a distance of more than 1000 miles 
# but had a total taxiing time below 15 minutes?
nrow(filter(hflights, DayOfWeek %in% c(6,7), Distance > 1000, TaxiIn + TaxiOut < 15)) 

# Remove rows that have NA in ArrDelay column
temp1 = filter(hflights, !is.na(ArrDelay) )






## arrange()
# arrabge(df, x)
# arrange(df,desc(x) )
# arrange(df, x, y)

# Definition of dtc
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))

# Arrange dtc by departure delays
arrange(dtc, DepDelay)

# Arrange dtc so that cancellation reasons are grouped
arrange(dtc, CancellationCode)

# Arrange dtc according to carrier and departure delays
arrange(dtc, UniqueCarrier, DepDelay)

# Arrange according to carrier and decreasing departure delays
arrange(hflights,UniqueCarrier, desc(DepDelay) )

# Arrange flights by total delay (normal order).
arrange(hflights, DepDelay+ArrDelay)





## summarise()
# summarise(df, maximum=max(x1), average=mean(x2), variance=var(x2) )

# What is the shortest distance flown and the longest distance flown?
summarise(hflights, min_dist=min(Distance), max_dist=max(Distance) )

# Waht is the longest Distance for diverted flights?
summarise(filter(hflights, Diverted == 1), max_div = max(Distance) )

# Remove rows that have NA in ArrDelay column
temp1 = filter(hflights, !is.na(ArrDelay) )

# Generate summary about ArrDelay column of temp1
summarise(temp1, earliest=min(ArrDelay), average=mean(ArrDelay), latest=max(ArrDelay), sd=sd(ArrDelay) )

# Generate summarizing statistics for hflights
summarise(hflights,
          n_obs = n(),
          n_carrier = n_distinct(UniqueCarrier),
          n_dest = n_distinct(Dest) )

# All American Airline flights
aa <- filter(hflights, UniqueCarrier == "American")

# Generate summarizing statistics for aa 
summarise(aa, n_flights =n(), n_canc=sum(Cancelled), avg_delay=mean(ArrDelay, na.rm=T) )






## pipe operator %>%

# example
hflights %>%
  mutate(diff = TaxiOut-TaxiIn) %>%
  filter(!is.na(diff) ) %>%
  summarise(avg = mean(diff) )

# example2: Count the number of overnight flights
hflights %>% 
  filter( !is.na(DepTime), !is.na(ArrTime), DepTime>ArrTime ) %>%
  summarise(num=n() )





# group_by()
# group_by(df,x1)

# Make an ordered per-carrier summary of hflights
hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(p_canc =  mean(Cancelled == 1) * 100,
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>%
  arrange(avg_delay, p_canc)

#use mutate() followed by arrange() to reorder the summarise table
# Ordered overview of average arrival delays per carrier
hflights %>%
  filter(!is.na(ArrDelay), ArrDelay>0) %>%
  group_by(UniqueCarrier) %>%
  summarise(avg = mean(ArrDelay)) %>%
  mutate(rank = rank(avg)) %>%
  arrange(rank)

# How many airplanes only flew to one destination?
hflights %>%
  group_by(TailNum) %>%
  summarise(ndest = n_distinct(Dest)) %>%
  filter(ndest == 1) %>%
  summarise(nplanes = n())

# Find the most visited destination for each carrier
hflights %>% 
  group_by(UniqueCarrier, Dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)