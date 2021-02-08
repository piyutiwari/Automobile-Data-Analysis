library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)

# Reading the data file

vehicles <- read.csv("data/vehicles.csv", stringsAsFactors = F)

# Save original data

vehicles_save <- vehicles

# Reading the description file

#labels <- read.table("data/Description.txt",sep = "-",header = F)

labels <-
  do.call(rbind, strsplit(readLines("data/Description.txt"), " - "))


# Basic data Analysis

nrow(vehicles)
# 39542 rows

ncol(vehicles)
# 83 columns

names(vehicles)


# unique years of data

length(unique(vehicles$year))
# 36 years

# First and last year

first_year <- min(vehicles$year)
# 1984

last_year <- max(vehicles$year)
# 2019

# making sure all the years are covered
last_year - first_year + 1

# What are the primary fuel types

table(vehicles$fuelType1)


# What are the major transmissions

table(vehicles$trany)

# It seems there 11 blank values

vehicles$trany[vehicles$trany == ""] <- NA


# We only care is transmission is automatic or manual, we are going to replace all subscript with automatic or manual

vehicles$trany <-
  as.factor(ifelse(substr(vehicles$trany, 1, 4) == "Auto", "Auto", "Manual"))


# Analysis

# Step 1 : Next, we will check the trend in MPG over the years

mpg_by_year <-
  vehicles %>% group_by(year) %>% summarise(
    mean_mpg = mean(comb08) ,
    mean_city_mpg = mean(city08),
    mean_highway_mpg = mean(highway08)
  )


p1 <-
  ggplot(data = mpg_by_year, aes(x = year)) + geom_point(aes(y = mean_mpg)) + geom_smooth(aes(y =
                                                                                                mean_mpg, color = "red"), method = "auto") + geom_point(aes(y = mean_city_mpg)) + geom_smooth(aes(y =
                                                                                                                                                                                                    mean_city_mpg, color = "green"), method = "auto") + geom_point(aes(y = mean_highway_mpg)) + geom_smooth(aes(y =
                                                                                                                                                                                                                                                                                                                  mean_highway_mpg, color = "blue"), method = "auto") + labs(x = "Year", y =
                                                                                                                                                                                                                                                                                                                                                                               "Average MPG", title = "Year on Year average MPG comparison") + scale_color_manual(
                                                                                                                                                                                                                                                                                                                                                                                 values = c("red", "green", "blue"),
                                                                                                                                                                                                                                                                                                                                                                                 label = c("Overall MPG Avg", "Avg City MPG", "Avg Highway MPG")
                                                                                                                                                                                                                                                                                                                                                                               ) + theme_minimal()

p1

# It looks like over gas economy has increased over the years.
# I think it will be worthwhile to check what happens with diffrent fuel types.
mpg_by_year <-
  vehicles %>% group_by(year, fuelType1) %>% summarise(
    mean_mpg = mean(comb08) ,
    mean_city_mpg = mean(city08),
    mean_highway_mpg = mean(highway08)
  )


ggplot(data = mpg_by_year, aes(x = year)) + geom_point(aes(y = mean_mpg)) + geom_smooth(aes(y =
                                                                                              mean_mpg, color = "red"), method = "auto") + geom_point(aes(y = mean_city_mpg)) + geom_smooth(aes(y =
                                                                                                                                                                                                  mean_city_mpg, color = "green"), method = "auto") + geom_point(aes(y = mean_highway_mpg)) + geom_smooth(aes(y =
                                                                                                                                                                                                                                                                                                                mean_highway_mpg, color = "blue"), method = "auto") + labs(x = "Year", y =
                                                                                                                                                                                                                                                                                                                                                                             "Average MPG", title = "Year on Year average MPG comparison") + scale_color_manual(
                                                                                                                                                                                                                                                                                                                                                                               values = c("red", "green", "blue"),
                                                                                                                                                                                                                                                                                                                                                                               label = c("Overall MPG Avg", "Avg City MPG", "Avg Highway MPG")
                                                                                                                                                                                                                                                                                                                                                                             ) + facet_grid( ~ fuelType1) + theme_minimal()

# As can be seen rom the previous graph that the electricity vehicles have contibuted a lot i the fuel economy.
# While fuel econoy of regular gasoline has also increased.

# usually, cars with higher engine have lower fuel economy, It could be very well possible that we have decreased the production of large engine vehicle.
# Let us first check if Higher engine have lower fuel economy.

ggplot(data = vehicles, aes(x = displ, y = comb08)) + geom_point() + ylim(0, 50) +
  geom_smooth() + labs(x = "Engine Displacement", y = "Fuel Economy", title =
                         "Fuel economy versus engine displacement")

# It is pretty evident that the fuel economy decreases with increase in fuel displacement.

# Now, we are going to check if production of larger vehicle has decreased over the years.

p2 <-
  ggplot(data = vehicles, aes(x = year, y = displ, fill = year)) + geom_bar(stat =
                                                                              "summary", fun.y = "mean") + geom_hline(yintercept = mean(vehicles$displ, na.rm = T),
                                                                                                                      color = "red") + geom_text(aes(
                                                                                                                        1984,
                                                                                                                        mean(vehicles$displ, na.rm = T),
                                                                                                                        label = "Lifetime displ avg",
                                                                                                                        vjust = -1
                                                                                                                      ))

p2

# it is for sure that the higher displacement engine count has decreased considerably. Lets us plot and economy and dispalcement next to each other.


grid.arrange(p1, p2)

# Things to notice in the above plots:
# 1. Engine displacement has been incraesing up untill 2008, with a sudden increase between 2006-2008.
# 2. Since 2009, there is considerable decrease in engine size. This partially explains the increase in fuel economy.
# 3. until 2005, engine displacement has increased however fuel efficiency roughly remained constant. This indicates that engine efficiency has increased over the years.
# 4. Although there is jump in engine size between 2006 and 2008 however engine efficiendy has remained constant. This look like a descrpency. Need to investigated more.


# Look like smaller vehicles are in fashion. Let us see whether automatic engine are more efficient than manual engine for four cylinder engines, and how efficienciencies ahve cahnegd over the years.

gascar4 <- subset(vehicles, cylinders == "4")

p5 <-
  ggplot(data = gascar4, aes(factor(year), comb08)) + geom_boxplot() + facet_wrap( ~
                                                                                     trany, ncol = 1) + theme(axis.text.x = element_text(angle = 45)) + labs(x =
                                                                                                                                                               "Year", "Economy")


# Looks like the manual car proportins has been decreasing over the time. Let us just visualiz that.

p3 <-
  ggplot(data = gascar4, aes(factor(year), fill = trany)) + geom_bar(position = "fill") + labs(x =
                                                                                                 "Year", y = "Transmission Proportion", fill = "Transmission") + theme(axis.text.x = element_text(angle = 45)) +
  geom_hline(yintercept = 0.5,
             color = "red",
             linetype = 2)

p4 <-
  gascar4 %>% group_by(year) %>% summarise(proportionTrany = mean(ifelse(trany ==
                                                                           "Manual", 1, 0))) %>% ggplot(aes(factor(year))) + geom_line(aes(y = proportionTrany, group =
                                                                                                                                             1, color = "red")) + geom_line(aes(
                                                                                                                                               y = 1 - proportionTrany,
                                                                                                                                               group = 1,
                                                                                                                                               color = "blue"
                                                                                                                                             )) + labs(x = "Year", y = "Tranmission Proportion", colour = "Transmission") +
  scale_color_manual(values = c("red", "blue"),
                     label = c("Manual", "Auto")) + theme(axis.text.x = element_text(angle = 45))

grid.arrange(p5, p3, p4)
# Above plot indicates that the although afficiency of manual cars is better that auto transmission cars but there proportion has been decreasing considerably.

unique(vehicles$make)

uniquemakes <- subset(gascar4, year < 2015)

uniquemakes <-
  select (subset(gascar4, year < 2015), c(year, make)) %>% distinct()

uniquemakes <-
  select (subset(gascar4, year < 2015), c(year, make)) %>% group_by(year) %>% nest()

reduce(uniquemakes$data, intersect)

carMakes <-
  subset(gascar4) %>% group_by(year) %>% summarise(numberOfMakes = length(unique(make)))

ggplot(carMakes, aes(year)) + geom_point(aes(y = numberOfMakes)) + ylim(20, 45) +
  labs(x = "Year", y = 'Number of models', title = "Total 4 cylinders model per year")

uniquemakes <-
  select (subset(gascar4, year < 2015), c(year, make)) %>% group_by(year) %>% nest()
brands <- reduce(uniquemakes$data, intersect)


subset(gascar4, make %in% brands$make) &&
  year < 2015) %>% group_by(year, make) %>% summarise(avg_fuel_effc = mean(comb08)) %>% ggplot(aes(x =
                                                                                                     year, y = avg_fuel_effc)) + geom_line() + facet_wrap( ~ make)
