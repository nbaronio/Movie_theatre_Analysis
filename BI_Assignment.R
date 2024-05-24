library(ggplot2)
library(stats)
library(moments)
library(readxl)
library(dplyr)
library(gridExtra)
library(vcd)
library(knitr)


setwd("/Users/Niccolo/Desktop/Master/BI/Assignment")

# import the dataset
dataset = read_excel("Dataset_New_2022.xlsx")
attach(dataset)

############################
######## Functions #########
############################

create_histogram <- function(data, main_title, x, y) {
  data_table <- as.data.frame(table(data))
  data_table$percentage <- (data_table$Freq / sum(data_table$Freq)) * 100
  
  stats <- data.frame(
    stat = c("Mean", "Median", "Min", "Max", "1st Qu.", "3rd Qu.", "Skewness", "Kurtosis"),
    value = c(
      mean(data),
      median(data),
      min(data),
      max(data),
      quantile(data, 0.25),
      quantile(data, 0.75),
      skewness(data),
      kurtosis(data)
    )
  )
  
  ggplot(data_table, aes(x = as.numeric(as.character(data)), y = Freq)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5) +
    geom_text(aes(label = Freq), vjust = 1.5, color = "white", size = 5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = main_title, x = "Rates", y = "Frequency") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    ) +
    annotation_custom(
      grob = grid::textGrob(
        label = paste0(apply(stats, 1, function(x) paste(x, collapse = ": ")), collapse = "\n"),
        x = x, y = y, hjust = 1, vjust = 1, gp = grid::gpar(col = "black", fontsize = 10)
      ),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )
}
create_histogram_custom_labels <- function(data, main_title, x,y) {
  data_table <- as.data.frame(table(data))
  data_table$percentage <- (data_table$Freq / sum(data_table$Freq)) * 100
  
  
  stats <- data.frame(
    stat = c("Mean", "Median", "Min", "Max", "1st Qu.", "3rd Qu.", "Skewness", "Kurtosis"),
    value = c(
      mean(data),
      median(data),
      min(data),
      max(data),
      quantile(data, 0.25),
      quantile(data, 0.75),
      skewness(data),
      kurtosis(data)
    )
  )
  
  
  ggplot(data_table, aes(x = as.numeric(as.character(data)), y = Freq)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5) +
    geom_text(aes(label = Freq), vjust = 1.5, color = "white", size = 5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5), labels = c("< 250€", "250€ - 500 €", "500€ - 750€", "750€ - 900€", "900€ +")) + # Define custom breaks and labels
    labs(title = main_title, x = "Money Spent (€)", y = "Frequency") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
}
create_tables <- function(data1, data2, main_title) {
  
  dataframe <- data.frame(
    data1,
    data2
  )
  
  # Create the summary table
  summary_table <- dataframe %>%
    group_by(data1) %>%
    summarise(
      Mean = mean(data2),
      N = n(),
      Std_Deviation = sd(data2)
    )
  
  # Add the total row
  total_row <- df %>%
    summarise(
      data1 = "Total",
      Mean = mean(data2),
      N = n(),
      Std_Deviation = sd(data2)
    )
  
  summary_table <- bind_rows(summary_table, total_row)
  
  knitr::kable(summary_table, col.names = c(main_title, "Mean", "N", "Std. Deviation"), caption = main_title)
  
}

############################
## 1. Univariate Analysis ##
############################

#### PIE CHART FOR GENDER ####
table(Q18) # 1 man, 2 woman

ggplot(data = data.frame(x = 1, y = c(52,48), Sex = c("Female", "Male")),
       aes(x, y, fill = Sex)) +
  geom_col(color = "white", size = 0.7) + # Adding border to the pie slices
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = paste0(y, "%")), colour = "white", 
            position = position_stack(vjust = 0.5), size = 6) +
  scale_fill_manual(values = c("#FF69B4","#87CEFA")) +
  ggtitle("Gender of sample") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b=10))
  )

#### BARPLOT OF WHAT VERSION OF MOVIE DO CUSTOMER PREFER (Q4) ####

table(Q4)
df_Q4 = data.frame(Q4)
# create labels that are gonna replace the old ones
labels_Q4 = c("1" = "Original Language Movies",
           "2" = "Dubbed Movies",
           "3" = "No preference")

# create dataframe
df_summary_Q4 = as.data.frame(table(df_Q4))
# create percentage
df_summary_Q4$Percentage = df_summary_Q4$Freq / sum(df_summary_Q4$Freq) * 100
# replace labels
df_summary_Q4$Q4 = factor(df_summary_Q4$Q4, labels = labels_Q4)
# plot + percentages on top of them
par(mar=c(10, 10, 10, 10))
barplot(height = df_summary_Q4$Freq,
        names.arg = df_summary_Q4$Q4,
        col = c("#fde725", "#21918c", "#440154"),
        ylim = c(0,70),
        main = "Preference for type of movies")
text(x = barplot(height = df_summary_Q4$Freq, plot = FALSE), 
     y = df_summary_Q4$Freq, 
     labels = paste0(round(df_summary_Q4$Percentage, digits = 2), "%"), 
     pos = 3, cex = 0.8, col = "black")

#### HISTOGRAM OF HOW MANY TIMES A CUSTOMER WENT TO SEE A MOVIE IN THE LAST 3 MONTHS ####
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(1,8))
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Q1, horizontal = TRUE, xaxt="n", frame=F)
par(mar = c(5, 4, 4, 2) + 0.1)
hist(Q1, xlab="N°Times/last 3 months", main="", labels=TRUE, freq = FALSE)
summary(Q1) # 75% of the sample went 3 times or less to the movie theatre
skewness(Q1)
kurtosis(Q1)

#### HISTOGRAM OF FEATURES WHEN CHOOSING A MOVIE THEATRE ####

# important variables
create_histogram(Q2_3, "Discounts", 0.25, 0.95) # IMPORTANT
create_histogram(Q2_5, "Price of ticket", 0.25, 0.95) # IMPORTANT
create_histogram(Q2_6, "Timetable", 0.25, 0.95) # PRETTY IMPORTANT
create_histogram(Q2_9, "Choice of films shown", 0.25, 0.95) # IMPORTANT

# not important variables
create_histogram(Q2_4, "Snack Offer", 0.95, 0.95) # NOT IMPORTANT
create_histogram(Q2_2, "Online purchase", 0.95, 0.95) # NOT IMPORTANT

#### HISTOGRAM OF AGE (Q19) ####
par(mfrow=c(1,1))
create_histogram(Q19, "Distribution of ages", 0.95, 0.95)
summary(Q19) # 75% of this sample has an age < 23
skewness(Q19)
kurtosis(Q19)

#### PIE CHART WITH PERCENTAGES OF GEOGRAPHIC AREAS (Q20) ##########

df <- data.frame(Q20)

labels = c("1" = "Italy",
           "2" = "Europe",
           "3" = "Asia",
           "4" = "Rest of the World")

custom_colors <- c("Italy" = "#2A788EFF", "Europe" = "#22A884FF", 
                   "Asia" = "#7AD151FF", "Rest of the World" = "#FDE725FF")

# Group the values 5, 6, and 7 into a single category "4-5-6-7"
df$Q20 <- factor(ifelse(df$Q20 %in% c(4, 5, 6, 7), "4", as.character(df$Q20)))
# Replace numbers with labels
df$Q20 <- factor(df$Q20, levels = names(labels), labels = labels)

# Create a summary table
df_summary <- as.data.frame(table(df))
colnames(df_summary) <- c("Q20", "Count")
# Create percentages
df_summary$Percentage <- df_summary$Count / sum(df_summary$Count) * 100

# Plot using ggplot2
ggplot(df_summary, aes(x = "", y = Count, fill = Q20)) +
  geom_col(color = "white") +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label=paste0(round(Percentage), "%")), color = "black", position = position_stack(vjust = 0.5), size = 6) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Pie Chart of Q20 with Grouped Categories", fill = "Q20", size = 100) +
  theme_void()
 ####
#### PIE CHART WITH PERCENTAGES OF PEOPLE STUDYING IN WHICH UNIVERSITY (Q21) #####

df_Q21 = data.frame(Q21)

labels_Q21 = c("1" = "Bocconi",
               "2" = "Politecnico",
               "3" = "IULM")

custom_colors_Q21 = c("Bocconi" = "#fde725", "Politecnico" = "#21918c", 
                       "IULM" = "#440154")

# IT WOULD BE GOOD TO PUT IN THE SLIDES A MILAN GEOMAPS IMAGE WITH POINTS ON THE 
# UNIVERSITIES THAT WE ARE CONSIDERING FOR THE ANALYSIS, SOMETHING LIKE THE THING 
# THAT IS SEEN IN PAGE 19 OF SLIDES 3
 
# create percentages
df_summary_Q21 <- as.data.frame(table(df_Q21))
df_summary_Q21$Percentage <- df_summary_Q21$Freq / sum(df_summary_Q21$Freq) * 100
df_summary_Q21$Q21 <- factor(df_summary_Q21$Q21, levels = names(labels_Q21), labels = labels_Q21)
colnames(df_summary_Q21) = c("Q21", "Freq", "Percentage")

# Create Pie Chart 
ggplot(df_summary_Q21, aes(x = "", y = Freq, fill = Q21)) +
  geom_col(color = "white") +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label=paste0(round(Percentage), "%")), color = "black", position = position_stack(vjust = 0.5), size = 6) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors_Q21) +
  labs(title = "Universities among students", fill = "Universities", size = 100) +
  theme_void()

#### BARPLOT OF UNIVERSITY TITLES #### 

df_summary_Q22 = as.data.frame(table(Q22))
df_summary_Q22$Percentage = df_summary_Q22$Freq / sum(df_summary_Q22$Freq) * 100
df_summary_Q22$Q22 = factor(df_summary_Q22$Q22, labels = c("Bachelor's Degree","Master's Degree"))

par(mar=c(5, 5, 5, 5))
barplot(height = df_summary_Q22$Freq, 
        names.arg = df_summary_Q22$Q22,
        col = c("#2A788EFF","#22A884FF"),
        ylim = c(0, 135),
        main = "University titles",
        xlab = "Variables",
        ylab = "Frequency")
percentages <- round(df_summary_Q22$Freq / sum(df_summary_Q22$Freq) * 100, 1)
text(x = barplot(height = df_summary_Q22$Freq, plot = FALSE), 
     y = df_summary_Q22$Freq, 
     labels = paste0(percentages, "%"), 
     pos = 3, cex = 0.8, col = "black")

#### HISTOGRAM MONTH SPENDING (Q23) ####

create_histogram_custom_labels(Q23, "Amount of money spent in a month", 0.95, 0.95)

#### HISTOGRAM HOW MANY TIMES AN OV MOVIE HAS BEEN SEEN ####

df_Q7 = as.data.frame(Q7)

df_Q7$Q7 <- factor(df_Q7$Q7, levels = 0:max(Q7))  # Ensure all levels are represented
percentages_Q7 <- df_Q7 %>%
  group_by(Q7) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Plot the histogram with a bar for each value and percentages
ggplot(percentages_Q7, aes(x = Q7, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Frequency of Watched Movie in Original Language in the last Year",
       x = "Number of Times",
       y = "Frequency") +
  scale_x_discrete(drop = FALSE, breaks = as.character(0:max(Q7))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

## Is it ok to keep the outliers? let's see how many outliers there are (we are gonna consider outliers those values that are greater than 10)
# Count the number of people who watched more than 10 movies and 10 or fewer
more_than_10 <- sum(as.numeric(df_Q7$Q7) >= 10)
less_or_equal_10 <- sum(as.numeric(df_Q7$Q7) < 10)

# Create a data frame for the pie chart
pie_data_Q7 <- data.frame(
  category = c("More than 10", "10 or fewer"),
  count = c(more_than_10, less_or_equal_10)
)
pie_data_Q7$percentage <- round(pie_data_Q7$count / sum(pie_data_Q7$count) * 100, 1)

# Plot the pie chart
ggplot(pie_data_Q7, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.title = element_blank()) +
  labs(title = "Number of People Watching More Than 10 Movies vs 10 or Fewer in the Last Year") +
  scale_fill_manual(values = c("skyblue", "lightgreen")) +
  geom_text(aes(label = paste0(pie_data_Q7$percentage, "%")), 
      position = position_stack(vjust = 0.5))




#### PIE CHART OF WHETHER THE CUSTOMER SAW AN OV MOVIE IN MILAN OR NOT ####

df_summary_Q8 = as.data.frame(table(Q8))
labels_Q8 = c("1" = "Yes",
              "2" = "No")
df_summary_Q8$Percentages = df_summary_Q8$Freq / sum(df_summary_Q8$Freq) * 100
df_summary_Q8$Q8 = factor(df_summary_Q8$Q8, levels = names(labels_Q8), labels = labels_Q8)

# create pie plot
ggplot(df_summary_Q8, aes(x = "", y = Freq, fill = Q8)) +
  geom_col(color = "white") +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label=paste0(round(Percentages), "%")), color = "black", position = position_stack(vjust = 0.5), size = 6) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#2A788EFF","#22A884FF")) +
  labs(title = "Have you seen a original language movie in Milan", fill = "Answer", size = 100) +
  theme_void()

#### PIE CHART OF PEOPLE THAT ARE INTERESTED IN A MOVIE THEATRE THAT SHOWS ONLY OV MOVIES ####

df_summary_Q13 = as.data.frame(table(Q13))
labels_Q13 = c("1" = "Yes",
              "2" = "No")
df_summary_Q13$Percentages = df_summary_Q13$Freq / sum(df_summary_Q13$Freq) * 100
df_summary_Q13$Q13 = factor(df_summary_Q13$Q13, levels = names(labels_Q13), labels = labels_Q13)

# create pie plot
ggplot(df_summary_Q13, aes(x = "", y = Freq, fill = Q13)) +
  geom_col(color = "white") +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label=paste0(round(Percentages), "%")), color = "black", position = position_stack(vjust = 0.5), size = 6) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#2A788EFF","#22A884FF")) +
  labs(title = "Are you interested in watching a OL movie", fill = "Answer", size = 100) +
  theme_void()




#### HISTOGRAM OF HOW MUCH THE CUSTOMER WOULD SPEND IF NORMAL TICKET = 10$ ####

# Calculate the frequency and percentage
df_summary_Q14 <- as.data.frame(table(Q14))
df_summary_Q14$percentage <- (df_summary_Q14$Freq / sum(df_summary_Q14$Freq)) * 100

# Create the histogram
ggplot(df_summary_Q14, aes(x = as.numeric(as.character(Q14)), y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  geom_text(aes(label = Freq), vjust = 1.5, color = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Histogram of Euros Spent on Original Version Movies",
       x = "Euros Spent",
       y = "Frequency",subtitle = "if a Normal Movie ticket is 10€") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


#### HISTOGRAM WITH REASONS TO CHOOSE AND NOT CHOOSE AN OV MOVIE THEATRE ####

create_histogram(Q10_1, "Avoid poor dubbing", 0.25, 0.95)
create_histogram(Q10_2, "Improve language knowledge", 0.25, 0.95)
create_histogram(Q10_3, "Understand untranslatable content", 0.25, 0.95)

create_histogram(Q11_3, "Few cinemas offer original versions", 0.25, 0.95)
create_histogram(Q11_4, "Limited movie sessions per day", 0.25, 0.95)




#############################
### 2. Bivariate Analysis ###
#############################

#### QUALI - QUALI BETWEEN UNIVERSITY AND ARE YOU INTERESTED IN GOING TO OV MOVIE THEATRE ####

value_map_Q13 = c("Yes", "No")
value_map_Q21 = c("Bocconi", "Politecnico", "IULM")

Q13_r = value_map_Q13[Q13]
Q21_r = value_map_Q21[Q21]

crosstable_Q13_Q21 = table(Q13_r, Q21_r)

# Create mosaic plot
mosaicplot(crosstable_Q13_Q21, color = c("#fde725", "#21918c", "#440154"),
           main = "Which students are more interested in OV cinemas?",
           xlab = "Are you interested in going to an OV cinema?", 
           ylab = "Which university are you studying in?",
           )

### OUTCOME

# The plot suggests that students from Bocconi and Politecnico universities 
# are generally more inclined towards attending OV cinemas, while IULM students 
# show a more mixed level of interest. This information can help cinemas tailor 
# their marketing strategies to better target university students based on their 
# interest in OV films.

# I DONT KNOW HOW TO ADD PERCENTAGES ON BOXES PLEASE DO WHILE DOING THE PRESENTATION

# strength of association
fisher_Q13_Q21 = fisher.test(crosstable_Q13_Q21)
fisher_Q13_Q21$p.value
# strong association (0.0242)

# Fisher's Exact Test: The p-value of 0.0242 indicates a statistically significant 
# association between university affiliation and interest in OV cinemas. This low 
# p-value suggests that the observed differences in interest across universities 
# are unlikely due to random chance.

# chi-squared test to see if we can reject the null-hypotesis that these 2 are correlated or not
chisq.test(crosstable_Q13_Q21)$expected
chisq.test(crosstable_Q13_Q21, correct = TRUE)
# p-value < 0.05, we can conclude that we have correlation

# Pearson's Chi-Squared Test: The test statistic is 7.1148 with a p-value of 0.02851. 
# This p-value is below the conventional threshold of 0.05, allowing us to reject 
# the null hypothesis of no association. Therefore, there is a significant 
# correlation between university affiliation and interest in OV cinemas.

# TO UNDERSTAND IF UNIVERSITIES TRY TO INCREASE OV CINEMA AWARENESS INDIRECTLY
# AND ALSO TO UNDERSTAND WHERE WE CAN OPEN THE MOVIE THEATRE (NEAR UNIVERSITIES?)

#### QUALI - QUALI Q4 - Q14 ####

value_map_Q4 = c("Original Language", "Dubbed", "No preference")
Q4_r = value_map_Q4[Q4]

crosstable_Q4_Q13 = table(Q4_r, Q13_r)

mosaicplot(crosstable_Q4_Q13, color = c("#fde725", "#21918c", "#440154"),
           main = "Do people that prefer OV want an OV movie theatre?",
           xlab = "Are you interested in going to an OV cinema?", 
           ylab = "What is your preference?",
)

# strength of association
fisher_Q4_Q13 = fisher.test(crosstable_Q4_Q13)
fisher_Q4_Q13$p.value
# 

# chi-squared test to see if we can reject the null-hypotesis that these 2 are correlated or not
chisq.test(crosstable_Q4_Q13)$expected
chisq.test(crosstable_Q4_Q13, correct = TRUE)
# p-value < 0.05, we can conclude that we have correlation

#### QUANT - QUANT PLEASE DO SOMETHING REGARDING THIS
#### QUALI - QUANT BETWEEN AVERAGE SPENDING Q23 AND HOW MUCH WOULD THE CUSTOMER SPEND FOR OV TICKET ####

value_map_Q23 = c("0€ - 250€", "250€ - 500€", "500€ - 750€", "750€ - 900€", "900€ +")

Q23_r = value_map_Q23[dataset$Q23]

boxplot(Q14~Q23_r,
        main = "",
        xlab = "Classes of Average spending",
        ylab = "How much money the customer would spend in an OV ticket")

## I WANT TO DO SOME COMPARISON BETWEEN MEANS LIKE IN THE SLIDES (PAGE 103),
# WE CAN DO THAT CONSIDERING "IMPORTANCE OF FACTORS" (so Q2, Q10 and Q11) WITH 
# WHICH UNIVERSITY Q21 OR LEVEL OF STUDY Q22 OR AVERAGE SPENDING Q23


#### QUALI - QUANT SPENDING FOR TICKET - CLASSES OF MONEY ####

# the outcome is in the terminal
dataframe_1 <- data.frame(
  Q23_r,
  Q14
)

# Create the summary table
summary_table <- dataframe_1 %>%
  group_by(Q23_r) %>%
  summarise(
    Mean = mean(Q14),
    N = n(),
    Std_Deviation = sd(Q14)
  )

# Add the total row
total_row <- df %>%
  summarise(
    Q23_r = "Total",
    Mean = mean(Q14),
    N = n(),
    Std_Deviation = sd(Q14)
  )

summary_table <- bind_rows(summary_table, total_row)

knitr::kable(summary_table, col.names = c("Classes of money spent", "Mean", "N", "Std. Deviation"), caption = "Amount of money that they are willing to spend for a OV ticket")

#### QUALI - QUANT UNIVERSITY ~ AMOUNT OF SPENDING ####

create_tables(df_summary_Q21$Q21, Q14, "Price ticket based on the university")

### OUTCOME

# The data indicates slight variations in the willingness to pay for an OV ticket 
# among students from different universities. The differences in average prices 
# are relatively small. The standard deviations are also quite similar across 
# universities, suggesting a comparable level of variability in the willingness 
# to pay within each group. This information could be useful for cinema operators 
# in setting targeted pricing strategies based on the university affiliation of 
# their audience.

#### QUANT - QUANT Q7 ~ Q14 ####

# we can analyze the correlation between the willingness to spend for an OV ticket, 
# and how many times the person went to see a OV movie

one.way <- aov(Q7 ~ Q14, data = dataset)
summary(one.way)

# Q7 is "how many times did u go last year in an OV movie theatre
# Q14 is "how much would u spend for a OV ticket if a normal ticket is 10€

### OUTCOME

# We found a statistically significant difference in the number of times 
# individuals went to an OV (original version) movie theatre last year based 
# on how much they would spend for an OV ticket if a normal ticket costs 10€ 
# (F(1, 178) = 6.055, p = 0.0148).

# This suggests that the willingness to pay a higher price for an OV ticket 
# is associated with the frequency of attending OV movie theatres. Specifically, 
# those willing to spend more than the normal ticket price tend to go to OV movie 
# theatres more frequently. This relationship underscores the potential influence 
# of perceived value or preference for OV movies on cinema-going behavior.



# Q1 ~ Q3
# Q4 ~ Q13
# Q7 ~ Q14