naikaku <- read.csv("cabsup.csv")
attach(naikaku)
cabsup2 <- cabsup
cabsup2[cabsup == 0 | cabsup == 9] <-NA
cabsup2[cabsup == 1] <- 5
cabsup2[cabsup == 2] <- 4
cabsup2[cabsup == 3] <- 3
cabsup2[cabsup == 4] <- 2
cabsup2[cabsup == 5] <- 1
mean(cabsup2, na.rm = TRUE)
#t.test(cabsup2, mu = 3)
age2 <- age
age2[age <= 4] <- 0
age2[age >= 5] <- 1
by(cabsup2, age2, mean, na.rm = TRUE)
#分散が等しいと仮定できるかどうか
var.test(cabsup2 ~ age2)
#分散が等しくないと仮定する場合
t.test(cabsup2 ~ age2, var.equal = FALSE)