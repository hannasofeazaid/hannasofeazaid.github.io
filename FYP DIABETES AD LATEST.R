data <- read.delim("clipboard") #copy the data from excel
View(data)



#check the structure of the data
str(data)
summary(data)


#check for missing values 
sum(is.na(data)) #check missing value
colSums(is.na(data))
which(rowSums(is.na(data)) > 0)
data[!complete.cases(data), ]

 
colSums(is.na(data)) #check missing value by column 
which(rowSums(is.na(data)) > 0) #check missing value by row 



#descriptive statistics 
str(data) #view dataset structure
summary(data) #summary statistics for all variables 
summary(data[, sapply(data, is.numeric)]) #descriptive statistics for numerical variables 
table(data$Status.of.Diabetes)
prop.table(table(data$Status.of.Diabetes)) #frequency table for dependent variables 

# Contingency table for Gender and Status.of.Diabetes
table(data$Gender, data$Status.of.Diabetes)

# To include proportions (e.g., row percentages)
prop.table(table(data$Gender, data$Status.of.Diabetes), margin = 1)

# Or column percentages
prop.table(table(data$Gender, data$Status.of.Diabetes), margin = 2)

# For a more detailed table with both counts and percentages, you can use the gmodels package
# install.packages("gmodels")  # if not installed
library(gmodels)
CrossTable(data$Gender, data$Status.of.Diabetes, prop.t = TRUE, prop.r = TRUE, prop.c = TRUE)

# Create the contingency table
cont_table <- table(data$Gender, data$Status.of.Diabetes)

# Convert to a data frame for a table-like format
as.data.frame(cont_table)


library(ggplot2)
library(scales)

ggplot(data, aes(x = Cholesterol.Level, fill = Status.of.Diabetes)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count",
            aes(label = percent(after_stat(prop)),
                group = Status.of.Diabetes),
            position = position_fill(vjust = 0.5),
            color = "black",
            size = 4) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Proportion of Diabetes Status by Cholesterol Level",
       x = "Cholesterol Level",
       y = "Proportion",
       fill = "Diabetes Status") +
  theme_minimal() +
  coord_flip()  # Add this to make the bars horizontal


library(ggplot2)

# Assuming Cholesterol.Level is categorical (e.g., Low, Normal, High)
# Create a contingency table
cont_table <- table(data$Gender, data$Cholesterol.Level)

# Convert to data frame for plotting
df <- as.data.frame(cont_table)

# Rename columns for clarity
names(df) <- c("Gender", "Cholesterol.Level", "Freq")

# Create the heatmap
ggplot(df, aes(x = Cholesterol.Level, y = Gender, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "red", high = "green") +  # Mimicking the color scheme
  labs(title = "Heatmap of Cholesterol Level by Gender",
       x = "Cholesterol Level",
       y = "Gender",
       fill = "Frequency") +
  theme_minimal()

install.packages("psych") #descriptive statistics using psych package 
library(psych)
describe(data)

install.packages("skimr") #Descriptive Table (Nice Formatting) 
library(skimr)
skim(data)

#distribution of dependent variable 
diabetes_freq <- table(data$Status.of.Diabetes) # Frequency count
library(ggplot2)
diabetes_freq



# Create count + percent table

library(dplyr)
library(plotrix)  

# Prepare the data summary
per <- data %>%
  dplyr::group_by(Status.of.Diabetes) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(
    pct = round(n / sum(n) * 100, 1),
    label = paste0(ifelse(Status.of.Diabetes == 0, "No", "Yes"), "\n", n, " (", pct, "%)")  
  )

# Create 3D pie chart with labels outside
pie3D(
  x = per$n,  
  labels = per$label, 
  col = c("green", "red"),  
  main = "Status of Diabetes",  
  explode = 0.1,  
  theta = pi/3,  
  labelcex = 0.8,  
  radius = 1  
)

# Allow plotting outside the plot region
par(xpd = TRUE)

# Add a legend beside the pie chart (to the right)
legend(x = 1.5, y = 0,  
       legend = c("No", "Yes"),  
       fill = c("green", "red"),  
       title = "Status of Diabetes",  
       cex = 0.8,  
       bty = "n"  
)

par(xpd = FALSE)



#frequency table and percentage for categorical variables 
# Gender
table(data$Gender)
prop.table(table(data$Gender)) * 100

# Cholesterol Level
table(data$Cholesterol.Level)
prop.table(table(data$Cholesterol.Level)) * 100


# Load necessary packages
library(dplyr)
library(plotly)

gender_summary <- data %>%
  dplyr::group_by(Gender) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(
    pct = round(n / sum(n) * 100, 1),
    label = paste0(Gender, ": ", n, " (", pct, "%)")
  )

# Create 3D Donut Chart using Plotly
fig_gender <- plot_ly(
  data = gender_summary,
  labels = ~Gender,
  values = ~n,
  type = 'pie',
  hole = 0.4,  
  text = ~label,
  textposition = 'inside',
  textinfo = 'text',
  insidetextfont = list(color = 'black'),
  marker = list(colors = c('#FF69B4', 'blue'))  
) %>%
  layout(
    title = "Patient's Gender",
    showlegend = TRUE,
    legend = list(title = list(text = "Gender"))
  )

fig_gender

# Load necessary packages
library(dplyr)
library(plotly)

# Prepare data for Cholesterol.Level
chol_summary <- data %>%
  dplyr::group_by(Cholesterol.Level) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(
    pct = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", pct, "%)")  
  )

fig_chol <- plot_ly()

categories <- unique(chol_summary$Cholesterol.Level)
colors <- c("High" = "red", "Average" = "orange", "Low" = "green")  # Map colors to categories

for (cat in categories) {
  subset_data <- chol_summary %>% filter(Cholesterol.Level == cat)
  fig_chol <- fig_chol %>%
    add_trace(
      data = subset_data,
      x = ~Cholesterol.Level,
      y = ~n,
      type = 'bar',
      name = cat,  
      marker = list(color = colors[cat]),
      text = ~label,  
      textposition = 'auto'  
    )
}

# Final layout for 3D effect
fig_chol <- fig_chol %>%
  layout(
    title = "Cholesterol Level",
    scene = list(
      xaxis = list(title = "Cholesterol Level"),
      yaxis = list(title = "Frequency"),
      zaxis = list(title = "")  
    ),
    showlegend = TRUE,
    legend = list(title = list(text = "Cholesterol Level"))
  )

fig_chol








