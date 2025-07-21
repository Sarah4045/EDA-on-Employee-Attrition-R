
# Load necessary libraries
# import library
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(knitr)

#  Dataset Collection 
df <- read.csv("Employee.csv")
view(df)
head(df)
# Check for missing values 
missing<- sapply(df, function(col) sum(is.na(col)))
cat("\nMissing values in data:\n")
print(kable(as.data.frame(missing)))

str(df)
summary(df)

# 2. Data Preprocessing and Normalization
# IQR method to address outliers in 
Q1 <- quantile(df$PaymentTier, 0.25)
Q3 <- quantile(df$PaymentTier, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df <- df[!(df$PaymentTier < lower_bound | df$PaymentTier > upper_bound), ]
cat("Data Processing Completed.\n\n")
#  Descriptive Statistics
df$LeaveOrNot_char <- str_replace_all(df$LeaveOrNot,
                                      c('0'='Leave','1'='Not Leave'))

group_Gender <- df %>% 
  group_by(Gender) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
View(group_Gender)


ggplot(group_Gender, aes(x="", y=perc, fill= Gender))+
  geom_bar(width = 1, stat = "identity")  +
  coord_polar("y", start = 0) +
  labs(x = "", y ="", title = "Proportion of Male and Female Employee") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#7ED3B2", "#BE8C63"))



ggplot(df) +
  geom_bar(mapping = aes(x = Age), fill = "brown")


ggplot(df) +
  geom_bar(mapping = aes(x =LeaveOrNot_char ), fill = "#2B4F60")

ggplot(df) +
  geom_bar(mapping = aes(x = Age, fill = LeaveOrNot_char), position = 'dodge') +
  labs(x = "Age", fill = "Leave or Not Leave") +
  scale_fill_manual(values=c("brown", "green"))



ggplot(df) +
  geom_bar(mapping = aes(x=City, fill = Gender), position="dodge") +
  labs(title = "Count of Employee By Gender of Every City") +
  scale_fill_manual(values=c("#EF6C57", "#7ED3B2"))

ggplot(df) + 
  geom_bar(mapping = aes(x = City, fill = LeaveOrNot_char), position="dodge") +
  labs(fill = "Leave or Not Leave") +
  scale_fill_manual(values=c("#BE8C63", "#8FBDD3"))


ggplot(data = df) +
  geom_bar(mapping = aes(x=JoiningYear, fill = LeaveOrNot_char), position = "dodge") +
  labs(fill = "Leave or Not Leave") +
  scale_fill_manual(values=c("#C98474", "#A7D2CB"))

group_Education <- df %>% 
  group_by(Education) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

View(group_Education)

ggplot(group_Education, aes(x="", y=perc, fill= Education))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = " ", y = " ", fill = "Educational Level") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#7ED3B2", "#354259", "#C05555"))

ggplot(df) + 
  geom_bar(aes(x = Education, fill = LeaveOrNot_char), position = "dodge") +
  labs(x = "Education Level", fill = "Leave or Not Leave") +
  scale_fill_manual(values=c("#7ED3B2", "#354259"))

ggplot(df) +
  geom_density(aes(x=JoiningYear, color = City), size = 1) +
  labs(x = "Year of Join", title = "Joining Year of Employee of Every City") +
  scale_color_manual(values=c("#04322E", "#C93D1B", "#35D0BA"))



EverBenched <- df %>% filter(EverBenched == "Yes") #filter only male data

LeaveNot_EverBenched <- EverBenched %>% 
  group_by(LeaveOrNot_char) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

View (LeaveNot_EverBenched)

ggplot(LeaveNot_EverBenched, aes(x="", y= perc, fill= LeaveOrNot_char))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(x = " ", y = "Ever Benched Employee", fill = "Leave or Not Leave", title = "Percentage of Leave or Not Leave Ever Benched") +
  geom_text(aes(label = labels),
            position = position_stack (vjust = 0.5)) +
  scale_fill_manual(values=c("#ECB390", "#94B49F"))

#Never Benched Employees
NeverBenched <- df %>% filter(EverBenched == "No") #filter only male data

LeaveNot_NeverBenched <- NeverBenched %>% 
  group_by(LeaveOrNot_char) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

View (LeaveNot_EverBenched)

ggplot(LeaveNot_NeverBenched, aes(x="", y= perc, fill= LeaveOrNot_char))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(x = " ", y = "Never Benched Employee", fill = "Leave or Not Leave", title = "Percentage of Leave or Not Leave Never Benched") +
  geom_text(aes(label = labels),
            position = position_stack (vjust = 0.5)) +
  scale_fill_manual(values=c("#ECB390", "#94B49F"))





maleEmp <- df %>% filter(Gender == "Male") #filter only male data

LeaveNot_Male <- maleEmp %>% 
  group_by(LeaveOrNot_char) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

View(LeaveNot_Male)

ggplot(LeaveNot_Male, aes(x="", y= perc, fill= LeaveOrNot_char))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(x = "", y = "Male Employee", fill = "Leave or Not Leave", title = "Percentage of Leave or Not Leave Male Employee") +
  geom_text(aes(label = labels),
            position = position_stack (vjust = 0.5)) +
  scale_fill_manual(values=c("#B4CDE6", "#628E90"))

#Female Employee
femaleEmp <- df %>% filter(Gender == "Female") #filter only male data

LeaveNot_Female <- femaleEmp %>% 
  group_by(LeaveOrNot_char) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

View(LeaveNot_Female)

ggplot(LeaveNot_Female, aes(x="", y= perc, fill= LeaveOrNot_char))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(x = "", y = "Female Employee", fill = "Leave or Not Leave", title = "Percentage of Leave or Not Leave Female Employee") +
  geom_text(aes(label = labels),
            position = position_stack (vjust = 0.5)) +
  scale_fill_manual(values=c("#F3C5C5", "#C1A3A3"))                  
    







                