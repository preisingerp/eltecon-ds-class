library(data.table)
library(magrittr)
library(ggplot2)

data <- fread("data/astra_h_ads_20190912_cleaned.csv", encoding = "UTF-8")

# Inspect data
View(head(data))
names(data)

data[, .('Hirdetések száma' = .N,
         "Medián Vételár" = median("Vételár"),
         "Medián KM" = median('Kilométeróra állása'),
         "Medián Teljesítmény" = median("Teljesítmény"))]

# Histogram
ggplot(data, aes(x = `Vételár`)) + geom_histogram()
ggplot(data, aes(x = `Teljesítmény`)) + geom_histogram()
ggplot(data, aes(x = `Kilométeróra állása`)) + geom_histogram()

# Scatterplot
ggplot(data, aes(y = `Vételár`, x = `Kilométeróra állása`)) + geom_point() + geom_smooth(method = "lm")
  scale_x_continuous(labels =  scales::comma) + scale_y_continuous(labels = scales::comma)
ggplot(data[Kivitel %in% c("Ferdehátú", "Kombi")],
       aes(y = `Vételár`, x = `Kilométeróra állása`, alpha = 0.75)) +
  geom_point(aes(color = Kivitel)) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "lm") +
  labs(title = "Teszt_cím", subtitle = "Teszt_alcím", caption = "Source: you wish") +
  theme_minimal() +
  theme(plot.title = element_text(color = "deeppink", hjust = 0.5), legend.position = "left") +
  facet_grid(Kivitel~`Évjárat`)

# fgv
# ggplot(data, aes(y = `Vételár`, x = Kivitel)) + geom_boxplot()
# str(data)
plot_against_price <- function(car_ads, selected_variable){
  ggplot(car_ads, aes_string(y = "Vételár", x = selected_variable)) +
    geom_boxplot()
}

plot_against_price(data, "Kivitel")
plot_against_price(data, "Üzemanyag")
