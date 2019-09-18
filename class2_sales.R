library(data.table)
library(ggplot2)
library(magrittr)
library(fasttime)
options(datatable.print.class = TRUE)

sales <- fread("data/sales_sample.csv")
summary(sales)

ggplot(sales, aes(sales_amount)) + geom_histogram()

sales[, purchase_date := as.Date(fastPOSIXct(purchase_date))]
sales[, sales_amount2 := floor(sales_amount/10)*10]
sales[, .N, sales_amount2][order(sales_amount2)]

ggplot(sales, aes(sales_amount2)) + geom_histogram(bins = 15)
