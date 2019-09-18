library(data.table)
library(ggplot2)
library(magrittr)

student <- fread("data/student-data.csv")

summary(student)

student[, .N, male]
student[, .N, age][order(age)]

student <- student[beer != 2.11212e+11 | is.na(beer)]

convert_meter_height_to_cm <- function(student) {
  studnet[height >= 1.3 & height <= 2.1, heights:= height * 100]
}

clear_unreliable_height <- function(student) {
  student[height < 100 | height > 230, height := NA]
}

clear_unreliable_food <- function(student) {
  student[food < 0 | food > 10^6, food := NA]
}

clear_unreliable_beer <- function(student) {
  student[beer < 0 | beer > 100, beer := NA]
}

convert_meter_height_to_cm
clear_unreliable_height(student)
clear_unreliable_food(student)
clear_unreliable_beer(student)
#student[height < 100, height := NA]
#student[food > 10^6, food := NA]
#student[food == 45, food := NA]

student$television <- as.numeric(student$television)

ggplot(student, aes(height, weight)) + geom_point()
ggplot(student, aes(food)) + geom_histogram()
ggplot(student, aes(television)) + geom_histogram(bins = 10)

long_data <- melt(student, measure.vars = c("food", "beer", "television"))
ggplot(long_data, aes(value)) + geom_histogram(bins = 10) +
  facet_wrap(~ variable, scales = "free")

# melt(student, measure.vars = c("food", "beer", "television")) %>%
#   ggplot(long_data, aes(value)) + geom_histogram() +
#     facet_wrap(~ variable, scales = "free_x")



