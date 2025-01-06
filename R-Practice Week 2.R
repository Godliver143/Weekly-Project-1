#GODLIVER ALANGYAM AWONLIE
#ALY 6010
# WEEK 2 R-PRACTICE
#NOVEMBER 11,2024

#PACKAGES NEEDED
library(pacman)
library(tidyverse)
library(janitor)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(psych)
library(tableone)

#READING DATASET INTO R
Student_survey <- read_csv("Module 1 Student Survey Survey Student Analysis Report (1).csv")

glimpse(Student_survey)

#Recreating the dataset
df <- data.frame( Major = c("Biology with a concentration in Biochemistry", "Accounting", "History and Spanish", "English",
                            "Economics","Economics", "Biology : Ecology and Evolutionary Biology", "Psychology", "Bachelor of Science in Nutrition",
                            "Interdisciplinary: Civil Engineering/ Environmental Engineering", "Applied Mathematics", "Computer Science", "Biology", "Biology"),
                  Excel_Skill = c(3,3,3,1,2,2,3,2,3,4,2,4,4,3),
                  R_Skill = c(2,3,2,2,3,2,2,2,2,2,3,3,2,2),
                  Python_Skill = c(1,2,1,1,2,1,2,1,1,2,1,4,1,1),
                  #Categorizing all majors recorded
                  Major_group = factor(c(1,2,3,3,2,2,1,3,1,4,4,4,1,1))  # 1=Bio/Science, 2=Business, 3=Humanities, 4=STEM
                  )

#DISCRIPTIVE STATISTICS

#Performing a descriptive statistics on the overall dataset
df_stats <- describe(df[,c("Excel_Skill","R_Skill","Python_Skill")])
write.csv(df_stats,"df_stats.")

# Group-wise descriptive statistics
group_stats <- by(df[,c("Excel_Skill","R_Skill","Python_Skill")], 
                  df$Major_group, 
                  describe)
print(group_stats)


# Create three-line table format
three_line_table <- data.frame(
  Statistic = c("Mean (SD)", "Range", "N"),
  Excel = c(
    sprintf("%.2f (%.2f)", mean(data$excel_skill), sd(data$excel_skill)),
    sprintf("%.0f-%.0f", min(data$excel_skill), max(data$excel_skill)),
    length(data$excel_skill)
  ),
  R = c(
    sprintf("%.2f (%.2f)", mean(data$r_skill), sd(data$r_skill)),
    sprintf("%.0f-%.0f", min(data$r_skill), max(data$r_skill)),
    length(data$r_skill)
  ),
  Python = c(
    sprintf("%.2f (%.2f)", mean(data$python_skill), sd(data$python_skill)),
    sprintf("%.0f-%.0f", min(data$python_skill), max(data$python_skill)),
    length(data$python_skill)
  )
)


#VISUALISATIONS

# Set up plotting parameters
pdf("visualizations.pdf", width=12, height=8)
par(mfrow=c(2,2), mar=c(4,4,2,1))

# Scatter plot
plot(df$Excel_Skill, df$R_Skill, 
     main="Scatter Plot: Excel vs R Skills",
     xlab="Excel Skill Level", 
     ylab="R Skill Level",
     pch=19)
abline(lm(r_skill ~ excel_skill, data=data), col="red")

# Jitter plot
plot(jitter(df$Excel_Skill), jitter(df$Python_Skill),
     main="Jitter Plot: Excel vs Python Skills",
     xlab="Excel Skill Level", 
     ylab="Python Skill Level",
     pch=19)
abline(lm(python_skill ~ excel_skill, data=data), col="blue")

# Boxplots
boxplot(df[,c("Excel_Skill", "R_Skill", "Python_Skill")],
        main="Boxplot of Skill Levels",
        ylab="Skill Level",
        col=c("lightblue", "lightgreen", "lightpink"))

# Boxplot by major group
boxplot(excel_skill ~ major_group, data=data,
        main="Excel Skills by Major Group",
        xlab="Major Group",
        ylab="Excel Skill Level",
        col="lightblue")
dev.off()



# Calculate outliers using boxplot.stats
excel_outliers <- boxplot.stats(df$Excel_Skill)$out
r_outliers <- boxplot.stats(df$R_Skill)$out
python_outliers <- boxplot.stats(df$Python_Skill)$out

