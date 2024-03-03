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
#分散が等しいと仮定できるかどうか(等分散性のF検定)
var.test(cabsup2 ~ age2)
#分散が等しくないと仮定する場合
t.test(cabsup2 ~ age2, var.equal = FALSE)

#2.3割合に関する推定
cabsup3 <- cabsup
cabsup3[cabsup == 0 | cabsup == 9] <-NA
cabsup3[cabsup == 1 | cabsup == 2] <- 1
cabsup3[cabsup >= 3 & cabsup <= 5] <- 0
mean(cabsup3, na.rm = TRUE)
prop.test(c(1026), c(1907))
prop.test(c(269, 757), c(565, 1342))

#2.5決定的選挙の検証
election <- read.csv("turnout.csv")
attach(election)
var.test(turnout ~ post1896)
#F検定の結果、p値が0.05よりも大きいので、等分散性が仮定できる
t.test(turnout ~ post1896, var.equal = TRUE)



