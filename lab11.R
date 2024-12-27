data <- read.csv("C:\\Users\\Alina\\Desktop\\datasetCountriesNezaleg.csv", sep = ",")
View(data)

# Фільтрація даних для України
ukraine_data <- data[data$Country.Name == "Ukraine", ]

# Перетворення даних з широкого формату в довгий
library(tidyr)
ukraine_data_long <- pivot_longer(ukraine_data, cols = starts_with("X"), 
                                  names_to = "Year", 
                                  values_to = "Value")

# Видалення "X" з назви року
ukraine_data_long$Year <- as.numeric(sub("X", "", ukraine_data_long$Year))
# Лінійний графік у декартовій системі координат
plot(ukraine_data_long$Year, ukraine_data_long$Value, type = "o", col = "black", pch = 16,
     xlab = "Рік", ylab = "Значення показника (% ВВП)", main = "Динаміка прямих іноземних інвестицій в Україну")
library(ggplot2)

# Побудова полярного графіка
ggplot(ukraine_data_long, aes(x = Year, y = Value)) +
  geom_line(color = "black") +
  coord_polar() +
  labs(title = "Динаміка прямих іноземних інвестицій в Україну (Полярний графік)", 
       x = "Рік", y = "Значення показника (% ВВП)")

# Описова статистика
library(tidyr)
library(e1071) # Для обчислення асиметрії та ексцесу
mean_value <- mean(ukraine_data_long$Value, na.rm = TRUE)  # Середнє арифметичне
std_error <- sd(ukraine_data_long$Value, na.rm = TRUE) / sqrt(length(na.omit(ukraine_data_long$Value)))  # Стандартна помилка
median_value <- median(ukraine_data_long$Value, na.rm = TRUE)  # Медіана
mode_value <- as.numeric(names(sort(table(ukraine_data_long$Value), decreasing = TRUE)[1]))  # Мода
sd_value <- sd(ukraine_data_long$Value, na.rm = TRUE)  # Стандартне відхилення
variance_value <- var(ukraine_data_long$Value, na.rm = TRUE)  # Дисперсія
kurtosis_value <- kurtosis(ukraine_data_long$Value, na.rm = TRUE)  # Ексцес
skewness_value <- skewness(ukraine_data_long$Value, na.rm = TRUE)  # Асиметрія
range_value <- max(ukraine_data_long$Value, na.rm = TRUE) - min(ukraine_data_long$Value, na.rm = TRUE)  # Розмах
min_value <- min(ukraine_data_long$Value, na.rm = TRUE)  # Мінімум
max_value <- max(ukraine_data_long$Value, na.rm = TRUE)  # Максимум
sum_value <- sum(ukraine_data_long$Value, na.rm = TRUE)  # Сума значень
count <- sum(!is.na(ukraine_data_long$Value))  # Кількість значень

# Виведення результатів
cat("Середнє арифметичне:", mean_value, "\n")
cat("Стандартна помилка:", std_error, "\n")
cat("Медіана:", median_value, "\n")
cat("Мода:", mode_value, "\n")
cat("Стандартне відхилення:", sd_value, "\n")
cat("Дисперсія:", variance_value, "\n")
cat("Ексцес:", kurtosis_value, "\n")
cat("Асиметрія:", skewness_value, "\n")
cat("Розмах:", range_value, "\n")
cat("Мінімум:", min_value, "\n")
cat("Максимум:", max_value, "\n")
cat("Сума значень:", sum_value, "\n")
cat("Кількість значень:", count, "\n")

#ПОБУДОВА ГІСТОГРАМИ
values <- ukraine_data_long$Value
ggplot(data.frame(Value = values), aes(x = Value)) +
  geom_histogram(binwidth = h_scott, fill = "gray", color = "black") +
  labs(title = "Гістограма значень прямих іноземних інвестицій в Україну",
       x = "Значення інвестицій (% ВВП)",
       y = "Частота значень") +
  theme_minimal()

#ПОБУДОВА КУМУЛЯТИ
# Встановлення кількості інтервалів відповідно до n - 1
n <- length(values)
num_bins <- n - 1

# Побудова гістограми та обчислення кумуляти
hist_data <- hist(values, breaks = num_bins, plot = FALSE)  # Створюємо гістограму без візуалізації
cum_freq <- cumsum(hist_data$counts) / sum(hist_data$counts)  # Кумулятивна частота

# Графік кумуляти
plot(hist_data$mids, cum_freq, type = "s", col = "black", lwd = 2,
     xlab = "Інтервали (кишені)", ylab = "Частість (імовірність)",
     main = "Кумулята за даними гістограми")

# Сортування даних та обчислення кумулятивної суми
sorted_values <- sort(values)
cum_percentage <- cumsum(sorted_values) / sum(sorted_values)

# Графік кумуляти за інтегральним процентом
plot(1:length(cum_percentage), cum_percentage, type = "s", col = "black", lwd = 2,
     xlab = "Інтервали (кишені)", ylab = "Частість (імовірність)",
     main = "Кумулята за інтегральним процентом")

# Метод ковзної середньої
cleaned_values <- na.omit(ukraine_data_long$Value)# Видалення NA значень із даних
require(smooth) 
#обчислення ковзного середнього для вказаного часу h
sma(cleaned_values, h=3, silent=FALSE)
sma(cleaned_values, h=5, silent=FALSE)
sma(cleaned_values, h=7, silent=FALSE)
sma(cleaned_values, h=9, silent=FALSE)
sma(cleaned_values, h=11, silent=FALSE)
sma(cleaned_values, h=13, silent=FALSE)
sma(cleaned_values, h=15, silent=FALSE)

#МЕДІАННА ФІЛЬТРАЦІЯ
# Підключаємо бібліотеку для фільтрації
library(zoo)

# Створюємо функцію для візуалізації результатів медіанної фільтрації
medf <- function(name, fit) {
  plot(fit, type = "o", col = "blue", main = name, xlab = "Рік", ylab = "Інвестиції (% ВВП)")
}

# Виконуємо медіанну фільтрацію з різними значеннями вікна
fit1 <- rollmedian(ukraine_data_long$Value, k = 3, fill = NA)
medf("w = 3", fit1)

fit2 <- rollmedian(ukraine_data_long$Value, k = 5, fill = NA)
medf("w = 5", fit2)

fit3 <- rollmedian(ukraine_data_long$Value, k = 7, fill = NA)
medf("w = 7", fit3)

fit4 <- rollmedian(ukraine_data_long$Value, k = 9, fill = NA)
medf("w = 9", fit4)

fit5 <- rollmedian(ukraine_data_long$Value, k = 11, fill = NA)
medf("w = 11", fit5)

#КОРЕЛЯЦІЙНИЙ АНАЛІЗ
# Завантаження бібліотек
library(ggplot2)
library(dplyr)
library(tidyr)

# Завантаження даних
data <- read.csv("C:\\Users\\Alina\\Desktop\\corelationInvestInflation.csv", sep = ",")

# Фільтрація даних для України
ukraine_data <- data[data$Country.Name == "Ukraine", ]

# Перетворення даних з широкого формату в довгий
ukraine_data_long <- pivot_longer(ukraine_data, cols = starts_with("X"), 
                                  names_to = "Year", 
                                  values_to = "Value")

# Видалення "X" з назви року та перетворення на числовий формат
ukraine_data_long$Year <- as.numeric(sub("X", "", ukraine_data_long$Year))

# Додавання стовпця для позначення періоду
ukraine_data_long <- ukraine_data_long %>%
  mutate(Period = case_when(
    Year <= 2018 ~ "До COVID-19",
    Year >= 2019 & Year <= 2021 ~ "Під час COVID-19",
    Year >= 2022 ~ "Під час війни"
  ))

#  інфляція і інвестиції є різними рядками в даних, потрібно їх розділити і об’єднати
investment_data <- filter(ukraine_data_long, Indicator.Name == "Foreign direct investment, net inflows (% of GDP)")
inflation_data <- filter(ukraine_data_long, Indicator.Name == "Inflation, GDP deflator (annual %)")

# Об’єднання даних інфляції та інвестицій за роком та періодом
combined_data <- merge(investment_data, inflation_data, by = c("Year", "Period"), suffixes = c("_investment", "_inflation"))
#View(combined_data)
# Побудова кореляційного поля загального та за періодами
plot(combined_data$Value_inflation,combined_data$Value_investment,type = "p")
ggplot(combined_data, aes(x = Value_inflation, y = Value_investment, color = Period)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Лінія тренду
  labs(title = "Кореляційне поле між інвестиціями та інфляцією в Україні",
       x = "Інфляція (значення)", y = "Інвестиції (значення)") +
  theme_minimal()

# Об’єднання даних інфляції та інвестицій за роком та періодом
combined_data <- merge(investment_data, inflation_data, by = c("Year", "Period"), suffixes = c("_investment", "_inflation"))
# View(combined_data)
#коефіцієнти кореляції
cor(combined_data$Value_investment, combined_data$Value_inflation)
cor(combined_data$Value_investment, combined_data$Value_inflation, method = 'kendall')

#Кореляційне відношення
# Визначимо кількість інтервалів (L) для розбиття значень X (інфляція)
L <- 5  # інтервали

# Визначення меж інтервалів на основі діапазону значень X (інфляція)
breaks <- quantile(combined_data$Value_inflation, probs = seq(0, 1, length.out = L + 1), na.rm = TRUE)
combined_data$interval <- cut(combined_data$Value_inflation, breaks = breaks, include.lowest = TRUE)

# Крок 1: Обчислення частинних математичних сподівань для кожної групи
group_means <- aggregate(Value_investment ~ interval, data = combined_data, FUN = mean, na.rm = TRUE)
colnames(group_means) <- c("interval", "m_yj")  # Позначимо середнє для кожної групи як m_yj

# Додаємо обчислені середні до основних даних
combined_data <- merge(combined_data, group_means, by = "interval")

# Крок 2: Обчислення загального математичного сподівання на основі частинних середніх
n_j <- table(combined_data$interval)
m_y <- sum(n_j * group_means$m_yj) / sum(n_j)

# Крок 3: Обчислення групової дисперсії вихідної змінної Y (інвестицій)
sigma_m_y <- sum(n_j * (group_means$m_yj - m_y)^2) / sum(n_j)

# Крок 4: Обчислення дисперсії для незгрупованих даних Y
sigma_y <- var(combined_data$Value_investment, na.rm = TRUE)

# Крок 5: Обчислення кореляційного відношення
correlation_ratio <- sigma_m_y / sigma_y
correlation_ratio

#ЕКСПОНЦІАЛЬНЕ ЗГЛАДЖУВАННЯ
# Завантаження пакету для експоненціального згладжування, якщо потрібно
if (!require(TTR)) install.packages("TTR")
library(TTR)

# Набір параметрів згладжування
alpha_values <- c(0.1, 0.15, 0.2, 0.25, 0.3)

# Функція для підрахунку поворотних точок
count_turning_points <- function(series) {
  sum(diff(sign(diff(series))) != 0)
}

# Зберігатимемо результати у списки
turning_points <- list()
correlations <- list()

# Згладжування ряду та обчислення поворотних точок і коефіцієнтів кореляції
for (alpha in alpha_values) {
  # Експоненціальне згладжування
  smoothed_series <- EMA(ukraine_data_long$Value, n = 1 / alpha)
  
  # Видалення NA для порівняння
  valid_index <- !is.na(smoothed_series)
  original_series <- ukraine_data_long$Value[valid_index]
  smoothed_series <- smoothed_series[valid_index]
  
  # Кількість поворотних точок
  turning_points[[as.character(alpha)]] <- count_turning_points(smoothed_series)
  
  # Коефіцієнт кореляції
  correlations[[as.character(alpha)]] <- cor(original_series, smoothed_series)
}

# Виведення результатів
results <- data.frame(
  Alpha = alpha_values,
  TurningPoints = unlist(turning_points),
  Correlation = unlist(correlations)
)
print(results)

#ВИВЕДЕННЯ ГРАФІКУ
# Збільшуємо поля графіку
par(mar = c(5, 5, 4, 2) + 0.1)
library(ggplot2)
if (!require(TTR)) install.packages("TTR")
library(TTR)

# Завантаження бібліотеки для експоненціального згладжування
if (!require(TTR)) install.packages("TTR")
library(TTR)

# Очищення активних графічних пристроїв
while (!is.null(dev.list()))  dev.off()

# Цикл для виведення графіків
for (alpha in alpha_values) {
  # Відкриття нового графічного вікна
  x11(width = 10, height = 6)
  
  # Експоненціальне згладжування
  smoothed_series <- EMA(ukraine_data_long$Value, n = 1 / alpha)
  
  # Побудова графіка
  plot(smoothed_series, type = "o", col = "blue", pch = 16,
       main = paste("Alpha =", alpha), xlab = "Час", ylab = "Значення")
  lines(smoothed_series, col = "blue")
  
  # Пауза для перегляду кожного графіка
  readline(prompt="Press [Enter] to see the next plot")
}


# Завантаження необхідних бібліотек
library(dplyr)


# Обчислення матриці відстаней для значень інвестицій
Mdist <- dist(data_long$Investment, method = "euclidean")

# Виконання ієрархічного кластерного аналізу
hc <- hclust(Mdist, method = "single")

# Побудова дендрограм для різних кількостей кластерів

# Загальний графік
plot(hc, cex = 0.6, main = "Дендрограма інвестицій в Україну")

# 2 кластери
plot(hc, cex = 0.6, main = "2 кластери")
rect.hclust(hc, k = 2, border = "red")

# 4 кластери
plot(hc, cex = 0.6, main = "4 кластери")
rect.hclust(hc, k = 4, border = "blue")

# 5 кластерів
plot(hc, cex = 0.6, main = "5 кластерів")
rect.hclust(hc, k = 5, border = "green")

