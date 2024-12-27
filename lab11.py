import pandas as pd


# Завантаження даних з CSV-файлу
data = pd.read_csv("C:\\Users\\Alina\\Desktop\\corelationInvestInflation.csv")


# Фільтрація рядків для інвестицій та інфляції
filtered_data = data[data["Indicator Name"].isin(["Foreign direct investment, net inflows (% of GDP)", "Inflation, GDP deflator (annual %)"])]


# Видалення перших 4 колонок і перший рядок (індекси 0:4 для колонок і 0 для рядка)
filtered_data = filtered_data.iloc[:, 4:]  # Залишаємо тільки колонки з 1991 року
filtered_data = filtered_data.reset_index(drop=True)  # Скидаємо індекси


# Додаємо колонку "Indicator" зі значеннями "Investment" та "Inflation"
filtered_data["Indicator"] = ["Investment", "Inflation"]


# Переміщення колонки "Indicator" на першу позицію
filtered_data = filtered_data[["Indicator"] + [col for col in filtered_data.columns if col != "Indicator"]]


# Транспонування таблиці, щоб зробити роки як рядки та "Investment"/"Inflation" як колонки
transposed_data = filtered_data.set_index("Indicator").T


# Обчислення кореляційної матриці тільки для "Investment" та "Inflation"
correlation_matrix = transposed_data[["Investment", "Inflation"]].corr()
print(correlation_matrix)

# Побудова корелограми
plt.figure(figsize=(10, 6))
plot_acf(df['investment'], lags=3)  # Встановлюємо кількість лагів, як у прикладі (8)
plt.title("Корелограма для ряду інвестицій")
plt.xlabel("Лаг")
plt.ylabel("Коефіцієнт автокореляції")
plt.show()
