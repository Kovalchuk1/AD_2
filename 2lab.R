
library(tidyverse)
library(ggplot2)
library(dplyr)
library(boot)
guns <- read_csv("C:/Users/user/Desktop/AD/guns.csv")
incidents <- read_csv("C:/Users/user/Desktop/AD/incidents.csv")
participants <- read_csv("C:/Users/user/Desktop/AD/participants.csv")
population <- read_csv("C:/Users/user/Desktop/AD/sub-est2019_all.csv")

#ДОВІРЧІ ДО 1 ЛАБ
arrested_mean_age <- filter(participants, (!is.na(participant_age) & arrested))
injured_mean_age <- filter(participants, (!is.na(participant_age) & injured))
killed_mean_age <- filter(participants, (!is.na(participant_age) & killed))
unharmed_mean_age <- filter(participants, (!is.na(participant_age) & unharmed))

arrested_mean_age <- subset(arrested_mean_age , select = c("incident_id", "participant_age"))
injured_mean_age <- subset(injured_mean_age, select = c("incident_id", "participant_age"))
killed_mean_age <- subset(killed_mean_age, select = c("incident_id", "participant_age"))
unharmed_mean_age <-subset(unharmed_mean_age, select = c("incident_id", "participant_age"))

dev.new(width = 10, height = 5)
hist(arrested_mean_age$participant_age, main="arrested", xlab="Freq", ylab="Count", col="blue")
dev.new(width = 10, height = 5)
hist(injured_mean_age$participant_age, main="injured", xlab="Freq", ylab="Count", col="blue")
dev.new(width = 10, height = 5)
hist(killed_mean_age$participant_age, main="killed", xlab="Freq", ylab="Count", col="blue")
dev.new(width = 10, height = 5)
hist(unharmed_mean_age$participant_age, main="unharmed", xlab="Freq", ylab="Count", col="blue")

mean_intervals <- function(group){
  mean_x_max <- mean(group)
  sd_x_max <- sd(group)

  #ДОВІРЧИЙ ДЛЯ СТАНДАРТНОГО НОРМ РОЗПОДІЛУ
  #рівень значущості 0.05 і  за допомогою таблиць інтегральної функції Лапласа знаходимо точку t=1.96
  cat("(", mean_x_max-(sd_x_max/sqrt(10))*1.96,";", mean_x_max+(sd_x_max/sqrt(10))*1.96, ")\n")

}

#довірчі середніх
print("Довірчі бутстреп-інтервали для arrested")
mean_intervals(arrested_mean_age$participant_age) 
print("Довірчі бутстреп-інтервали для injured")
mean_intervals(injured_mean_age$participant_age) 
print("Довірчі бутстреп-інтервали для killed")
mean_intervals(killed_mean_age$participant_age) 
print("Довірчі бутстреп-інтервали для unharmed")
mean_intervals(unharmed_mean_age$participant_age) 

plt_intervals(21.47676, 35.76224, 20.02631, 35.28413, 24.09815 , 42.38408, 21.03038, 36.31567)

#довірчі медіани
print("Довірчі бутстреп-інтервали для arrested")
wilcox.test(arrested_mean_age$participant_age, conf.int = TRUE)$conf.int 
print("Довірчі бутстреп-інтервали для injured")
wilcox.test(injured_mean_age$participant_age, conf.int = TRUE)$conf.int 
print("Довірчі бутстреп-інтервали для killed")
wilcox.test(killed_mean_age$participant_age, conf.int = TRUE)$conf.int 
print("Довірчі бутстреп-інтервали для unharmed")
wilcox.test(unharmed_mean_age$participant_age, conf.int = TRUE)$conf.int 

plt_intervals(26.99996, 27.00001, 25.99998, 26.00002, 31.50003, 31.99992, 26.99999, 26.99992)


#КОРЕЛЯЦІЯ

state_1 <- c("Nevada")

count_1 <-  table(incidents$state[incidents$state %in% state_1])
x_1 <- as.data.frame(count_1)
x_1


state_2 <- c("Nevada")

count_2 <-  table(incidents$state[incidents$state %in% state_2])
x_2 <- as.data.frame(count_2)
x_2


library(tidyverse)
install.packages("lubridate")
library(lubridate)
library(magrittr)

library(dplyr)
library(lubridate)

# фільтруємо тільки дані для штату California
df_ca <- incidents %>% filter(state == "New York")

# дістаємо день, місяць і рік з колонки date
df_ca$date <- as.Date(df_ca$date, format = "%Y-%m-%d")
df_ca$day <- day(df_ca$date)
df_ca$month <- month(df_ca$date)
df_ca$year <- year(df_ca$date)

# групуємо incident_id за днем, місяцем і роком
df_ca_grouped <- df_ca %>% 
  group_by(year, month, day) %>% 
  summarize(incident_id_count = n())

df_ca_grouped <- df_ca_grouped %>%
  arrange(day, month, year)

Populations <- read_csv("C:/Users/user/Desktop/AD/population_ratio.csv")

df_ca_grouped <- df_ca_grouped %>%
  mutate(date = make_date(year, month, day),  # Створюємо нову колонку з датою
         week = week(date)) %>%              # Створюємо нову колонку з номером тижня
  group_by(year, month, week) %>%            # Групуємо дані за роком, місяцем та тижнем
  summarize(incident_id_count = sum(incident_id_count)/39054676) 



df_ca_grouped <- df_ca_grouped %>% 
  mutate(date = paste(month, week, sep = "-")) %>% 
  select(year, date, incident_id_count)

# Побудова графіків точкової кореляції для кожного року
ggplot(df_ca_grouped, aes(x = date, y = incident_id_count)) + 
  geom_point() +
  facet_wrap(~year, ncol = 3)




#--------------------------------------------------------------
# Об'єднання колонок month та day у нову колонку date
df_ca_grouped <- df_ca_grouped %>% 
  mutate(date = paste(month, day, sep = "-")) %>% 
  select(year, date, incident_id_count)
#--------------------------------------------------------------

#ПІКОВИЙ МІСЯЦЬ
t <- mutate(incidents, year = as.integer(year), month = as.integer(month))
a <- filter(t, (year >= 2014) & (year<=2017) & (month >= 3) & (month <= 7))
b <- filter(t, (year >= 2014) & (year<=2017) & (month >= 8))

injured_by_month_a <- aggregate(a$n_injured, 
                                by = list(month = a$month), 
                                FUN = sum)


injured_by_month_b <- aggregate(b$n_injured, 
                                by = list(month = b$month), 
                                FUN = sum)

cor(injured_by_month_a$month, injured_by_month_a$x)
cor(injured_by_month_b$month, injured_by_month_b$x)


#1. Чи впливає темношкіре населення на злочинність?

#Список 10 штатів США, де темношкірі займають найбільшу частину населення 
state_max <- c("Mississippi", "Louisiana", "Alabama", "South Carolina",
               "Georgia", "Florida", "Maryland", "Virginia", "Delaware", "New York")
#Список 10 штатів США, де темношкірі займають найменшу частину населення 
state_min <- c("Montana", "Vermont", "Maine", "New Hampshire", "Idaho",
               "Wyoming","North Dakota" , "West Virginia", "Alaska", "Iowa")

count_max <-  table(incidents$state[incidents$state %in% state_max])
x_max <- as.data.frame(count_max)
x_max


count_min <-  table(incidents$state[incidents$state %in% state_min])
x_min <- as.data.frame(count_min)
x_min
#--------------------------------------------------------------  



#BOOTSTRAP FOR MEAN
boot_mean_var_with_sd <- function(x, indices, estimate_var = TRUE, R_for_sd = 200){
  n <- length(x)
  mean_bar <- mean(x[indices])
  var_bar <- (n - 1)/n * var(x[indices])
  
  if (estimate_var){
    boot_out <- boot(x[indices], statistic = boot_mean_var_with_sd,
                     R = R_for_sd, estimate_var = FALSE)
    
    return(c(mean_bar, var(boot_out$t[, 1]), var_bar, var(boot_out$t[, 2])))
  }
  else {
    return(c(mean_bar, var_bar))
  }
}

count_intervals <- function(group1){
  B <- 100
  boot_result_meanvar_max <- boot(group1, statistic = boot_mean_var_with_sd, R = B)
  boot.ci(boot_result_meanvar_max, index = c(1, 2))

}

#передаєм стовпчики
print("Довірчі бутстреп-інтервали для середнього кількості злочинів для штатів, де темношкірі займають найбільшу частину населення ")
count_intervals(x_max$Freq) 
print("Довірчі бутстреп-інтервали для середнього кількості злочинів для штатів, де темношкірі займають найменшу частину населення :")
count_intervals(x_min$Freq)  
#--------------------------------------------------------------  



#BOOTSTRAP FOR MEDIANA
foo <- function(data, indices){
  dt<-data[indices,]
  median(dt[,2])
}

bootstrap_median <- function(group){
  set.seed(12345)
  MBS <- boot(group, foo, R=100)
  print(MBS)
  boot.ci(MBS, index=1)
}

#передаєм тільки таблицю
print("Довірчі бутстреп-інтервали для середнього кількості злочинів для штатів, де темношкірі займають найбільшу частину населення ")
bootstrap_median(x_max) 
print("Довірчі бутстреп-інтервали для середнього кількості злочинів для штатів, де темношкірі займають найменшу частину населення :")
bootstrap_median(x_min)  

#---------------------------------------------------------------


#побудова довірчих інтервалів
plt_intervals <- function(a1, a2, b1, b2){
  df <- data.frame(
    group = c("Група 1", "Група 2"),
    mean = c((a1+a2)/2,(b1+ b2)/2),
    ymin = c(a1,b1),
    ymax = c(a2, b2)
  )


  ggplot(df, aes(x = group, y = mean)) +
    geom_point(size = 5, shape = 23, fill = "red") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 1, color = "black") +
    ggtitle("Графік довірчих інтервалів") +
    ylab("Середнє значення") +
    theme_bw()
}

plt_intervals(747, 1562, 5260, 9707)
plt_intervals(573, 1349, 4535, 8925)
#---------------------------------------------------------------



#TEST VOLD FOR MEAN
test_vold_mean <- function(group1, group2){
  mean_hat_x <- mean(group1)
  mean_hat_y <- mean(group2)

  var_hat_x <- var(group1) / length(group1)
  var_hat_y <- var(group2) / length(group2)

  se <- sqrt(var_hat_x + var_hat_y)
  T <- (mean_hat_x - mean_hat_y) / se
  p_value <- pnorm(T, lower.tail = FALSE)

  mean_hat_x
  mean_hat_y
  p_value
  conf.int
}

#передаєм колонки
test_vold_mean(x_max$Freq, x_min$Freq)
#---------------------------------------------------------------
#TEST VOLD FOR MEDIAN

library('boot')

boot_median_diff_with_sd <- function(x, indices, n_sample1,
                                     estimate_var = TRUE, R_for_sd = 200){
  indices_sample1 <- indices[1:n_sample1]
  indices_sample2 <- indices[-(1:n_sample1)]
  
  m1 <- median(x[indices_sample1, 1])
  m2 <- median(x[indices_sample2, 1])
  
  median_diff_bar <- m1 - m2
  
  if (estimate_var){
    boot_out <- boot(x[indices, ], statistic = boot_median_diff_with_sd,
                     R = R_for_sd, strata = x[, 2],
                     n_sample1 = n_sample1, estimate_var = FALSE)
    return(c(median_diff_bar, var(boot_out$stat[, 1])))
  }
  else {
    return(median_diff_bar)
  }
}



test_vold_median <- function(group1, group2){
  B <- 200
  dat <- cbind(c(group1, group2), rep(0:1, each = 10))
  boot_result <- boot(dat,statistic = boot_median_diff_with_sd, R = B,
                      n_sample1 = 10,  strata = dat[, 2], estimate_var = FALSE)   
  
  median_hat_x <- median(group1)
  median_hat_y <- median(group2)


  se <- sd(boot_result$t)  
  T <- (median_hat_x - median_hat_y) / se
  p_value = 2 * pnorm(abs(T), lower.tail = FALSE) 
  conf.int <- c( median_hat_y - median_hat_x - qnorm(0.95)*se, Inf) 
  print('Медіани вибірок:')
  print(median_hat_x)
  print(median_hat_y)
  print('Значення p-value')
  print(p_value)
  print(conf.int)
}

#передаєм колонки

test_vold_median(x_max$Freq, x_min$Freq)
#---------------------------------------------------------------


#2. Чи впливає соціально-економічний статус на злочинність у штатах?

#Список 10 штатів США з найвищим HDI в США 
state_max <- c("Mississippi", "Arkansas", "West Virginia", "Kentucky", "Alabama", "Louisiana", 
            "Oklahoma", "Tennessee", "South Carolina", "Idaho")
#Список 10 штатів США з найнижчим HDI в США 
state_min <- c("Massachusetts", "Connecticut", "New Jersey", "Minnesota", "Maryland", 
            "New Hampshire", "Pennsylvania", "New York", "Colorado", "Hawaii")

count_max <-  table(incidents$state[incidents$state %in% state_max])
x_max <- as.data.frame(count_max)
x_max

count_min <-  table(incidents$state[incidents$state %in% state_min])
x_min <- as.data.frame(count_min)
x_min
#---------------

mean_x_max <- mean(x_max$Freq)
sd_x_max <- sd(x_max$Freq)
cat("Середнє значення Freq:", mean_x_max, "\n")
cat("Квадратичне відхилення Freq:", sd_x_max)

mean_x_min <- mean(x_min$Freq)
sd_x_min <- sd(x_min$Freq)
cat("Середнє значення Freq:", mean_x_min, "\n")
cat("Квадратичне відхилення Freq:", sd_x_min)

#ДОВІРЧИЙ ДЛЯ СТАНДАРТНОГО НОРМ РОЗПОДІЛУ
#рівень значущості 0.05 і  за допомогою таблиць інтегральної функції Лапласа знаходимо точку t=1.96
cat("Довірчий інтервал кількості злочинів штатів США з найвищим HDI в США   (",
    mean_x_max-(sd_x_max/sqrt(10))*1.96,";", mean_x_max+(sd_x_max/sqrt(10))*1.96, ")\n")
cat("Довірчий інтервал кількості злочинів штатів США з найнижчим HDI в США  (",
    mean_x_min-(sd_x_min/sqrt(10))*1.96,";", mean_x_min+(sd_x_min/sqrt(10))*1.96, ")\n")

df <- 10 - 1
alpha <- 0.05  
qt_alpha <- qt(1 - alpha / 2, df)
qt_neg_alpha <- qt(alpha / 2, df)

se_max <- sd_x_max / sqrt(10)
CI_max <- c(mean_x_max  + qt_neg_alpha * se_max, mean_x_max  + qt_alpha * se_max)


se_min <- sd_x_min / sqrt(10)
CI_min <- c(mean_x_min  + qt_neg_alpha * se_min, mean_x_min  + qt_alpha * se_min)
#ДОВІРЧИЙ ДЛЯ T-РОЗПОДІЛУ
cat("Довірчий інтервал кількості злочинів штатів США з найвищим HDI в США  (", CI_max, ")\n")
cat("Довірчий інтервал кількості злочинів  штатів США з найнижчим HDI в США (", CI_min, ")\n")

plt_intervals(2616.38, 6530.82 , 2873.762, 6011.838)
plt_intervals(2314.651, 6832.549, 2631.876, 6253.724)

cat(var(x_min$Freq))
cat(var(x_max$Freq))
cat("Дисперсії виборок значно відрізняються, тому використовуємо Welch t-test")
t.test(x_min$Freq, x_max$Freq, var.equal = FALSE)


dev.new(width = 10, height = 5)
hist(x_min$Freq, main="Histogram of Freq", xlab="Freq", ylab="Count", col="blue")
dev.new(width = 10, height = 5)
hist(x_max$Freq, main="Histogram of Freq", xlab="Freq", ylab="Count", col="blue")





#4 Чи впливає на вбивство отримання зробої більш легше на важче?
#штати у яких легше отримати зброю
states_min <- c("Alaska", "Arizona", "Vermont", "Montana", "Kansas", "Arkansas", "Wyoming", "Maine", "Idaho", "New Mexico")
#штати де важче отримати зброю
states_max <- c("California", "Connecticut", "New Jersey", "Maryland", "Massachusetts", "New York", "Washington", "Illinois", "Minnesota", "Delaware")

library(dplyr)

x_min <- incidents %>%
  filter(state %in% states_min) %>%
  group_by(state) %>%
  summarize(Freq = sum(n_killed)+sum(n_injured))
x_min$Freq <- as.numeric(x_min$Freq)
x_min

x_max <- incidents %>%
  filter(state %in% states_max) %>%
  group_by(state) %>%
  summarize(Freq = sum(n_killed)+sum(n_injured))
x_max$Freq <- as.numeric(x_max$Freq)
x_max

#-------

#ДОВІРЧИЙ ДЛЯ медіани
print("Довірчий інтервал кількості вбитих і постраждалих штатів США де важче отримати зброю")
bootstrap_median(x_max) 
print("Довірчий інтервал кількості вбитих і постраждалих штатів США де легше отримати зброю")
bootstrap_median(x_min) 

#побудова
plt_intervals(187, 1473, 1522, 6764)

#test volda median
test_vold_median(x_max$Freq, x_min$Freq)

dev.new(width = 10, height = 5)
hist(x_min$Freq, main="Histogram of Freq", xlab="Freq", ylab="Count", col="blue")
dev.new(width = 10, height = 5)
hist(x_max$Freq, main="Histogram of Freq", xlab="Freq", ylab="Count", col="blue")


#5 
population$population_count <- rowMeans(population[, c("POPESTIMATE2014", "POPESTIMATE2015", "POPESTIMATE2016", "POPESTIMATE2017")])

library(dplyr)

df <- population %>%
  select(NAME, STNAME, population_count)

unique(gsub(".+\\s(\\w+)$", "\\1", df$NAME))

df_city <- df[grepl("city", df$NAME), c("NAME", "STNAME", "population_count")]
df_town <- df[grepl("town", df$NAME), c("NAME", "STNAME", "population_count")]
df_village <- df[grepl("village", df$NAME), c("NAME", "STNAME", "population_count")]

df_city$NAME <- sub(" city", "", df_city$NAME)
df_city$NAME <- sub(" City", "", df_city$NAME)
df_city$NAME <- sub(" \\(pt\\.\\)", "", df_city$NAME)

df_town$NAME <- sub(" town", "", df_town$NAME)
df_town$NAME <- sub(" Town", "", df_town$NAME)
df_town$NAME <- sub(" \\(pt\\.\\)", "", df_town$NAME)

df_village$NAME <- sub(" village", "", df_village$NAME)
df_village$NAME <- sub(" City", "", df_village$NAME)
df_village$NAME <- sub(" \\(pt\\.\\)", "", df_village$NAME)

unique(gsub(".+\\s(\\w+)$", "\\1", df_village$NAME))
unique(gsub(".+\\s(\\w+)$", "\\1", df_town$NAME))
unique(gsub(".+\\s(\\w+)$", "\\1", df_city$NAME))

df_city_unique <- subset(df_city, !duplicated(df_city))



df_count <- incidents %>%
            group_by(city_or_county, state) %>%
            summarize(incident_count = n())
df_count <- df_count %>%
  rename(NAME = city_or_county,
         STNAME = state)


df_village_count <- merge(df_village, df_count, by=c("NAME", "STNAME"))
df_village_count$result <-  df_village_count$incident_count / df_village_count$population_count 
dev.new(width = 10, height = 5)
hist(df_village_count$result, main="village", xlab="result", ylab="Count", col="blue")


df_town_count <- merge(df_town, df_count, by=c("NAME", "STNAME"))
df_town_count$result <-  df_town_count$incident_count / df_town_count$population_count
dev.new(width = 10, height = 5)
hist(df_town_count$result, main="town", xlab="result", ylab="Count", col="blue")

df_city_count <- merge(df_city, df_count, by=c("NAME", "STNAME"))
df_city_count$result <-  df_city_count$incident_count / df_city_count$population_count
library(dplyr)
df_city_count <- df_city_count %>% distinct()
dev.new(width = 10, height = 5)
hist(df_city_count$result, main="city", xlab="result", ylab="Count", col="blue")



foo <- function(data, indices){
  dt<-data[indices,]
  median(dt[,2])
}

bootstrap_median <- function(group){
  set.seed(12345)
  MBS <- boot(group, foo, R=200)
  print(MBS)
  boot.ci(MBS, type = "basic", index = 1)
}


print("Довірчий інтервал кількості злочинів великих міст США ")
bootstrap_median(df_city_count %>%
                   select(NAME, result))
print("Довірчий інтервал кількості злочинів малих міст США ")
bootstrap_median(df_town_count %>%
                   select(NAME, result)) 
print("Довірчий інтервал кількості злочинів сел США ")
bootstrap_median(df_village_count %>%
                   select(NAME, result)) 



plt_intervals <- function(a1, a2, b1, b2, c1, c2){
  df <- data.frame(
    group = c("Група 1", "Група 2", "Група 3"),
    mean = c((a1+a2)/2,(b1+ b2)/2,(c1+ c2)/2), 
    ymin = c(a1,b1,c1),
    ymax = c(a2, b2, c2)
  )
  
  
  ggplot(df, aes(x = group, y = mean)) +
    geom_point(size = 5, shape = 23, fill = "red") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 1, color = "black") +
    ggtitle("Графік довірчих інтервалів") +
    ylab("Середнє значення") +
    theme_bw()
}

plt_intervals(0.0007,  0.0008, 0.0012,  0.0013, 0.0010,  0.0012 )  


boot_median_diff_with_sd <- function(x, indices, n_sample1, n_sample2,
                                     estimate_var = TRUE, R_for_sd = 200){
  indices_sample1 <- indices[1:n_sample1]
  indices_sample2 <- indices[-(1:n_sample2)]
  
  m1 <- median(x[indices_sample1, 1])
  m2 <- median(x[indices_sample2, 1])
  
  median_diff_bar <- m1 - m2
  
  if (estimate_var){
    boot_out <- boot(x[indices, ], statistic = boot_median_diff_with_sd,
                     R = R_for_sd, strata = x[, 2],
                     n_sample1 = n_sample1, n_sample2 = n_sample2, estimate_var = FALSE)
    return(c(median_diff_bar, var(boot_out$stat[, 1])))
  }
  else {
    return(median_diff_bar)
  }
}


test_vold_median <- function(group1, group2){
  B <- 100
  group_indicator <- rep(0:1, each = length(c(group1, group2)))
  dat <- cbind(c(group1, group2), group_indicator)
  
  boot_result <- boot(dat,statistic = boot_median_diff_with_sd, R = B,
                      n_sample1 = length(group1), n_sample2 = length(group2),  strata = dat[, 2], estimate_var = FALSE)   
  
  median_hat_x <- median(group1)
  median_hat_y <- median(group2)
  
  
  se <- sd(boot_result$t)  
  T <- (median_hat_x - median_hat_y) / se
  p_value = 2 * pnorm(abs(T), lower.tail = FALSE) 
  conf.int <- c( median_hat_y - median_hat_x - qnorm(0.95)*se, Inf) 
  print('Медіани вибірок:')
  print(median_hat_x)
  print(median_hat_y)
  print('Значення p-value')
  print(p_value)
  print(conf.int)
}

print(' Великі і малі міста')
test_vold_median(df_town_count$result, df_city_count$result)
print(' Великі міста і села')
test_vold_median( df_village_count$result, df_city_count$result)
print(' Малі міста і села')
test_vold_median(df_town_count$result, df_village_count$result)


vec_finite1 <- df_town_count$result[is.finite(df_town_count$result)]
vec_finite2 <- df_village_count$result[is.finite(df_village_count$result)]
vec_finite3 <- df_city_count$result[is.finite(df_city_count$result)]

print(' Великі і малі міста')
t.test(vec_finite3, vec_finite1 , var.equal = FALSE)
print(' Великі міста і села')
t.test(vec_finite3, vec_finite2 , var.equal = FALSE)
print(' Малі міста і села')
t.test(vec_finite2, vec_finite1 , var.equal = FALSE)


#6 Чи впливає к-сть поліцейських у штатах на злочинність
#де найбільше к-сть поліцейських на 1000- осіб
state_max <- c("Mississippi", "Georgia", "Alabama", "Arkansas", "Louisiana", "Tennessee", "Kentucky", "Wyoming", "Alaska", "Florida")
#де найменше к-сть поліцейських на 1000- осіб
state_min <- c("Massachusetts", "Hawaii", "Rhode Island", "New York", "Connecticut", "California", "Michigan", "New Jersey", "Maine", "New Hampshire")

count_max <-  table(incidents$state[incidents$state %in% state_max])
x_max <- as.data.frame(count_max)
x_max

count_min <-  table(incidents$state[incidents$state %in% state_min])
x_min <- as.data.frame(count_min)
x_min
#---------------


#ДОВІРЧИЙ ДЛЯ медіани
print("Довірчий інтервал кількості злочинів штатів США де найбільша к-сть поліцейських")
bootstrap_median(x_max) 
print("Довірчий інтервал кількості злочинів штатів США де найменша к-сть поліцейських")
bootstrap_median(x_min) 

#побудова
plt_intervals(902, 7923, 1349, 7626)

#test volda median
test_vold_median(x_max$Freq, x_min$Freq)

dev.new(width = 10, height = 5)
hist(x_min$Freq, main="Histogram of Freq", xlab="Freq", ylab="Count", col="blue")
dev.new(width = 10, height = 5)
hist(x_max$Freq, main="Histogram of Freq", xlab="Freq", ylab="Count", col="blue")


#7 довірчний інтервал для кількості інцидентів для різних частинн Сша(зх,cх, пд,пн)
state_north <- c("Vermont", "Connecticut", "Massachusetts", "Maine", "New Hampshire", "Rhode Island", "New Jersey", "New York", "Pennsylvania") 

state_midwest <- c("Wisconsin", "Illinois", "Indiana", "Michigan", "Ohio", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") 

state_south <- c("Virginia", "Delaware", "Georgia", "West Virginia", "Maryland", "North Carolina", "Florida", "South Carolina", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana, Oklahoma", "Texas") 

state_west <- c("Idaho", "Arizona", "Wyoming", "Colorado", "Montana", "Nevada", "New Mexico", "Utah", "Alaska", "Washington", "Hawaii", "California", "Oregon")


count_north <-  table(incidents$state[incidents$state %in% state_north])
x_north <- as.data.frame(count_north)
x_north

count_midwest <-  table(incidents$state[incidents$state %in% state_midwest])
x_midwest <- as.data.frame(count_midwest)
x_midwest

count_south <-  table(incidents$state[incidents$state %in% state_south])
x_south <- as.data.frame(count_south)
x_south

count_west <-  table(incidents$state[incidents$state %in% state_west])
x_west <- as.data.frame(count_west)
x_west

Populations <- read_csv("C:/Users/user/Desktop/AD/population_ratio.csv")

merged_data <- merge(x_north, Populations, by.x = "Var1", by.y = "state")
merged_data$Freq_div_Population <- merged_data$Freq / merged_data$`2018 Population`
fruits_subset <- subset(merged_data, select = c("Var1", "Freq_div_Population"))
names(fruits_subset)[names(fruits_subset) == "Freq_div_Population"] <- "Freq"
x_north <- fruits_subset 

merged_data <- merge(x_midwest, Populations, by.x = "Var1", by.y = "state")
merged_data$Freq_div_Population <- merged_data$Freq / merged_data$`2018 Population`
fruits_subset <- subset(merged_data, select = c("Var1", "Freq_div_Population"))
names(fruits_subset)[names(fruits_subset) == "Freq_div_Population"] <- "Freq"
x_midwest <- fruits_subset 

merged_data <- merge(x_south, Populations, by.x = "Var1", by.y = "state")
merged_data$Freq_div_Population <- merged_data$Freq / merged_data$`2018 Population`
fruits_subset <- subset(merged_data, select = c("Var1", "Freq_div_Population"))
names(fruits_subset)[names(fruits_subset) == "Freq_div_Population"] <- "Freq"
x_south <- fruits_subset 

merged_data <- merge(x_west, Populations, by.x = "Var1", by.y = "state")
merged_data$Freq_div_Population <- merged_data$Freq / merged_data$`2018 Population`
fruits_subset <- subset(merged_data, select = c("Var1", "Freq_div_Population"))
names(fruits_subset)[names(fruits_subset) == "Freq_div_Population"] <- "Freq"
x_west <- fruits_subset 



dev.new(width = 10, height = 5)
hist(x_north$Freq, main="north state", xlab="Freq", ylab="Count", col="blue")

dev.new(width = 10, height = 5)
hist(x_midwest$Freq, main="midwest state", xlab="Freq", ylab="Count", col="blue")

dev.new(width = 10, height = 5)
hist(x_south$Freq, main="south state", xlab="Freq", ylab="Count", col="blue")

dev.new(width = 10, height = 5)
hist(x_west$Freq, main="West State", xlab="Freq", ylab="Count", col="blue")

#BOOTSTRAP FOR MEDIANA
foo <- function(data, indices){
  dt<-data[indices,]
  median(dt[,2])
}

bootstrap_median <- function(group){
  set.seed(12345)
  MBS <- boot(group, foo, R=200)
  boot.ci(MBS, index=1)
}

print("Довірчий інтервал кількості злочинів для north штатів США ")
bootstrap_median(x_north) 
print("Довірчий інтервал кількості злочинів для midwest штатів США ")
bootstrap_median(x_midwest) 
print("Довірчий інтервал кількості злочинів для south штатів США ")
bootstrap_median(x_south) 
print("Довірчий інтервал кількості злочинів для West штатів США ")
bootstrap_median(x_west) 


#побудова довірчих інтервалів
plt_intervals <- function(a1, a2, b1, b2, c1, c2, d1, d2){
  df <- data.frame(
    group = c("arrested", "injured", "killed", "unharmed"),
    mean = c((a1+a2)/2,(b1+ b2)/2,(c1+ c2)/2,(d1+ d2)/2),
    ymin = c(a1,b1, c1, d1),
    ymax = c(a2, b2, c2, d2)
  )
  
  
  ggplot(df, aes(x = group, y = mean)) +
    geom_point(size = 5, shape = 23, fill = "red") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 1, color = "black") +
    ggtitle("Графік довірчих інтервалів") +
    ylab(" значення медіани") +
    theme_bw()
}

plt_intervals(0.0003,  0.0006, 0.0008,  0.0011, 0.0007,  0.0009, 0,  0)

print(' midwest і south')
test_vold_median(x_midwest$Freq, x_south$Freq)

print(' midwest і west')
test_vold_median(x_midwest$Freq, x_west$Freq)

print(' midwest і north')
test_vold_median(x_midwest$Freq, x_north$Freq)

print(' west і south')
test_vold_median(x_west$Freq, x_south$Freq)

print(' north і south')
test_vold_median(x_north$Freq, x_south$Freq)

print(' west і north')
test_vold_median(x_west$Freq, x_north$Freq)

