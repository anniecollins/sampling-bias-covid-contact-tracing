library(tidyverse)
library(scales)

# Import Toronto Public Health Data
COVID19_cases <- read_csv("data/COVID19 cases.csv")
toronto_tests <- read_csv("data/r83g2f38.csv")


# Clean and summarize source of infection data
COVID19_cases$`Source of Infection`[COVID19_cases$`Source of Infection` == 'N/A - Outbreak associated'] <- 'Outbreak associated'
source_sum <- COVID19_cases %>% group_by(`Source of Infection`) %>% summarise(count=n())


# Clean and summarize daily case, outbreak, and unknown data
daily_sum <- COVID19_cases %>% group_by(`Reported Date`) %>% 
  summarise(total=n(), outbreak=sum(`Outbreak Associated`=="Outbreak Associated"), unknown=sum(`Source of Infection`=="Unknown/Missing"))
daily_sum <- daily_sum %>% mutate(prop_outbreak=outbreak * 100/total)
daily_sum <- daily_sum %>% mutate(prop_unknown=unknown * 100/total)
daily_sum <- daily_sum[-c(289),]


# Clean and summarize daily case and unknown source data
daily_unknown <- COVID19_cases %>% group_by(`Reported Date`) %>% 
  summarise(total=n(), unknown=sum(`Source of Infection`=="Unknown/Missing"))
daily_unknown <- daily_unknown %>% mutate(prop_unknown=unknown/total)
daily_unknown <- daily_unknown[-c(289),]


# Summarize data for the second wave (September-December)
second_wave <- daily_sum %>% filter(`Reported Date` >= "2020-09-01")


# Clean and summarize test data
toronto_tests$category <- substr(toronto_tests$category, 5, 7)
month_tests <- toronto_tests %>% group_by(category) %>% summarise(mean=mean(`Positive results`))
month_tests$category <- factor(month_tests$category, levels=c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


# Plot source of infection
source <- ggplot(source_sum, aes(x=reorder(`Source of Infection`, -count), y=count)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  xlab("") + 
  ylab("Total Count") +
  ggtitle("Source of COVID-19 Infection, All Cases, Toronto Public Health")
source
ggsave("outputs/figures/source_of_infection_graph.pdf")


# Plot daily case numbers
cases <- ggplot(daily_sum, aes(x=`Reported Date`, y=outbreak)) + 
  geom_line(colour="red") + 
  geom_line(aes(x=`Reported Date`, y=total), colour="black") +
  geom_line(aes(x=`Reported Date`, y=unknown), colour="blue") +
  ylab("Number of Cases") + 
  xlab("Reported Date") +
  ggtitle("Daily Reported COVID-19 Cases, Toronto Public Health", "January 23, 2020 to December 6, 2020") +
  annotate("text", as.Date("2020-11-15"), 120, label = "Outbreak", color = "red", alpha = 0.75, hjust = 0) +
  annotate("text", as.Date("2020-10-25"), 550, label = "Total", color = "black", alpha = 0.75, hjust = 0) +
  annotate("text", as.Date("2020-11-15"), 320, label = "Unknown", color = "blue", alpha = 0.75, hjust = 0)
cases
ggsave("outputs/figures/daily_cases_all.pdf")


# Plot daily case numbers (second wave)
cases2 <- ggplot(second_wave, aes(x=`Reported Date`, y=outbreak)) + 
  geom_line(colour="red") + 
  geom_line(aes(x=`Reported Date`, y=total), colour="black") +
  geom_line(aes(x=`Reported Date`, y=unknown), colour="blue") +
  ylab("Number of Cases") +
  ggtitle("Daily Reported COVID-19 Cases, Toronto Public Health", "September 1, 2020 to December 6, 2020") +
  annotate("text", as.Date("2020-11-27"), 100, label = "Outbreak", color = "red", alpha = 0.75, hjust = 0) +
  annotate("text", as.Date("2020-11-15"), 530, label = "Total", color = "black", alpha = 0.75, hjust = 0) +
  annotate("text", as.Date("2020-11-12"), 290, label = "Unknown", color = "blue", alpha = 0.75, hjust = 0)
cases2
ggsave("outputs/figures/daily_cases_sept.pdf")


# Plot proportion from outbreak
outbreak_prop <- ggplot(daily_sum) + 
  geom_line(aes(`Reported Date`, prop_outbreak), colour="red") + 
  geom_line(aes(`Reported Date`, prop_unknown), colour="blue") +
  scale_x_date(date_labels = "%b") +
  ylim(0, 80) +
  ylab("% of Total") +
  ggtitle("Proportion of Outbreak Associated Cases, Toronto Public Health", "January 23, 2020 to December 6, 2020") +
  annotate("text", as.Date("2020-10-15"), 63, label = "Unknown", color = "blue", alpha = 0.75, hjust = 0) +
  annotate("text", as.Date("2020-11-12"), 30, label = "Outbreak", color = "red", alpha = 0.75, hjust = 0)
outbreak_prop
ggsave("outputs/figures/outbreak_prop.pdf")


# Plot proportion from outbreak (second wave)
outbreak_prop2 <- ggplot(second_wave) + 
  geom_line(aes(`Reported Date`, prop_outbreak), colour="red") + 
  geom_line(aes(`Reported Date`, prop_unknown), colour="blue") +
  ylim(0, 80) +
  ylab("% of Total") +
  ggtitle("Proportion of Outbreak Associated Cases, Toronto Public Health", "September 1, 2020 to December 6, 2020") +
  annotate("text", as.Date("2020-11-17"), 60, label = "Unknown", color = "blue", alpha = 0.75, hjust = 0) +
  annotate("text", as.Date("2020-11-20"), 25, label = "Outbreak", color = "red", alpha = 0.75, hjust = 0)
outbreak_prop2
ggsave("outputs/figures/outbreak_prop_sept.pdf")


# Plot positive rate of tests
tests <- ggplot(month_tests, aes(x=category, y=mean, group=1)) + 
  geom_line() + 
  xlab("Month") + 
  ylab("Mean % of Positive Test Results") + 
  ggtitle("Positive COVID Test Results, Toronto Public Health", "May 1, 2020 to December 3, 2020")
ggsave("outputs/figures/positive_tests.pdf")