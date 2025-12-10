library(readr)
#install.packages("dplyr")
#install.packages("ggplot2")
uni <- read.csv("World University Rankings 2023.csv", stringsAsFactors = FALSE)
head(uni)
uni[] <- lapply(uni, as.numeric)

uni[] <- lapply(uni, function(x) as.numeric(as.character(x)))

str(uni)

summary(uni)

#checking for any missing values
anyNA(uni)
sum(is.na(uni))
colSums(is.na(uni))

#handling missing values
numeric_cols <- c("No.of.student.per.staff", "OverAll.Score", "Teaching.Score",
                  "Research.Score", "Citations.Score", "Industry.Income.Score",
                  "International.Outlook.Score")

for(col in numeric_cols) {
  mean_val <- mean(uni[[col]], na.rm = TRUE)
  uni[[col]][is.na(uni[[col]])] <- mean_val
}

uni$International.Student[is.nan(uni$International.Student)] <- NA
uni$International.Student[is.na(uni$International.Student)] <- mean(uni$International.Student, na.rm = TRUE)

uni$Name.of.University <- NULL
uni$Location <- NULL
uni$No.of.student <- NULL
uni$Female.Male.Ratio <- NULL

colSums(is.na(uni))

uni$University.Rank[is.na(uni$University.Rank)] <- 
  mean(uni$University.Rank, na.rm = TRUE)

uni$International.Student <- NULL

colSums(is.na(uni))

head(uni)
summary(uni)
#handled the missing values

