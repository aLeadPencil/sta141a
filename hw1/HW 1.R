rm(list=ls())
library(ggplot2)
library(stats)
library(base)
library(dplyr)
work_dir <- "D:/School Stuff/Math/Stats 141A/Data Files/HW 1"
setwd(work_dir)
scorecard = readRDS("D:/School Stuff/Math/Stats 141A/Data Files/HW 1/college_scorecard_2013.rds")

#1
dim(scorecard)
sum(scorecard$main_campus)
#3312 observations, 2431 college counts

##############

#2
table(sapply(scorecard, class))
charac = 'character'
int = 'integer'
logi = 'logical'
factr = 'factor'. 
num = 'numeric'
names((sapply(scorecard, class) == charac)[sapply(scorecard, class) == charac])
names((sapply(scorecard, class) == int)[sapply(scorecard, class) == int])
names((sapply(scorecard, class) == logi)[sapply(scorecard, class) == logi])
names((sapply(scorecard, class) == factr)[sapply(scorecard, class) == factr])
names((sapply(scorecard, class) == num)[sapply(scorecard, class) == num])
#51 feature counts, 4 character features, 4 factor features, 15 integer features, 3 logical features, 25 numeric features

#################

#3
sum(sapply(scorecard, is.na))
NA_table = table(is.na(scorecard))
NA_table2 = colSums(is.na(scorecard))
sum(NA_table2)
NA_table
NA_table2
as.matrix(NA_table2)
summary(NA_table2)
hist(as.matrix(NA_table2), main = 'Frequency of NA Counts', xlab = 'NA Counts', ylab = 'Frequency', col = 'PURPLE')
#23197 NA's, high frequency for 0 NA count features due to there being many non-int/numeric features, high frequency for NA counts from 400-600


#################

#4
summary(scorecard$ownership)
ownership_table = table(scorecard$ownership, scorecard$highest_degree)
plot(ownership_table, color = TRUE, shade = TRUE)
barplot(ownership_table_percentages)
barplot(ownership_table)
ownership_table_percentages = prop.table(table(scorecard$ownership, scorecard$highest_degree), margin = 2)
plot(ownership_table_percentages)
#There are more private colleges than there are public colleges
#Proportion graph from code
prop.table(table(scorecard$ownership, scorecard$highest_degree, margin = 2))
barplot(prop.table(table(scorecard$ownership, scorecard$highest_degree), margin = 2), shade = TRUE)

#################

#5
undergrad_pop_summary = summary(scorecard$undergrad_pop)
undergrad_pop_mean = mean(scorecard$undergrad_pop, na.rm = TRUE)
undergrad_pop_median = median(scorecard$undergrad_pop, na.rm = TRUE)
boxplot(scorecard$undergrad_pop, horizontal = TRUE, main = "Boxplot for undergrad_pop", xlab = "population count")
quantile_list = quantile(scorecard$undergrad_pop, probs = seq(0, 1, 0.1), na.rm = TRUE)
plot(density(quantile_list, main = "undergrad_pop quantile plot", ylab = "population count", xlab = "Quantile"))
abline(v = undergrad_pop_mean)
abline(v = undergrad_pop_median, lty = "dashed")
abline(v = quantile_list, lty = "dotted")

#All quantiles with the exception of 10 has a low value for the population count
#the 10th quantile i.e. the largest value of population count appears to be an outlier

##############

#6
#five most populous states are California, Texas, New York, Illinois, Florida

pop = scorecard[scorecard$state %in% c("CA", "TX", "NY", "IL", "FL"),]
boxplot(tuition~state, data = pop)
pop = droplevels(pop)
boxplot(tuition~state, data = pop, xlab = "Tuition", ylab = "State", horizontal = TRUE)



CA_tuitiondata = scorecard[which(scorecard$state == "CA"),]
TX_tuitiondata = scorecard[which(scorecard$state == "TX"),]
NY_tuitiondata = scorecard[which(scorecard$state == "NY"),]
IL_tuitiondata = scorecard[which(scorecard$state == "IL"),]
FL_tuitiondata = scorecard[which(scorecard$state == "FL"),]

#catx = merge(data.frame(CA_tuitiondata, row.names = NULL), data.frame(TX_tuitiondata, row.names = NULL), by = 0, all = TRUE)[-1]
tuition_data = rbind.fill(CA_tuitiondata, TX_tuitiondata, NY_tuitiondata, IL_tuitiondata, FL_tuitiondata)
tuition_data_dropped = droplevels(tuition_data)
boxplot(tuition_data_dropped$tuition~tuition_data_dropped$state, xlab = "Tuition", ylab = "State", horizontal = TRUE)

########################

#7
#a
max(scorecard$avg_sat, na.rm = TRUE)
which(scorecard$avg_sat == 1534)
highest_avg_sat = scorecard$name[105]

#b
max(scorecard$undergrad_pop, na.rm = TRUE)
which(scorecard$undergrad_pop == 166816)
open_admission_status = scorecard$open_admissions[2371]

#c
public_schools = scorecard[scorecard$ownership == "Public",]
public_avg_family_inc = public_schools$avg_family_inc
which.min(public_avg_family_inc)
scorecard$zip[348]

#d
max(scorecard$grad_pop, na.rm = TRUE)
scorecard$grad_pop[2371]
identical(max(scorecard$grad_pop, na.rm = TRUE), scorecard$grad_pop[2371])

###############

#8
#a
forprofit_schools = scorecard[scorecard$ownership == "For Profit",]
forprofit_bachelors = forprofit_schools[scorecard$primary_degree == "Bachelor",]
smoothScatter(forprofit_bachelors$revenue_per_student, forprofit_bachelors$spend_per_student)
boxplot(forprofit_bachelors$revenue_per_student, forprofit_bachelors$spend_per_student, names = c("rev", "spend"), horizontal = TRUE)

#b
subsetted_data = subset(scorecard, ownership == "For Profit" & primary_degree == "Bachelor")
smoothScatter(subsetted_data$revenue_per_student, subsetted_data$spend_per_student,
              xlab = "revenue per student", ylab = "spending per student")
net_income = (scorecard$revenue_per_student - scorecard$spend_per_student)
sum(net_income, na.rm = TRUE)

total_net_income = subsetted_data$revenue_per_student - subsetted_data$spend_per_student

sum(total_net_income)
