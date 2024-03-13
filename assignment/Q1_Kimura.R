minwage <- read.csv("minwage.csv")
attach(minwage)
NJ <- wageBefore
NJ[location == 'PA'] <- 0
NJ[location != 'PA'] <- 1
# 各店舗の賃金
wageChange <- wageAfter - wageBefore
# 各店舗の常勤労働者数とパートタイム労働者数の合計
staffChange <- (fullAfter + partAfter) - (fullBefore + partBefore)
# 各店舗の常勤労働者割合
fullPropChange <- (fullAfter / (fullAfter + partAfter)) - (fullBefore / (fullBefore + partBefore))

# wageChangeの二つの平均値の差のt検定
var.test(wageChange ~ NJ)
# F検定の結果、p値が0.05よりも大きいので、等分散性が仮定できる
t.test(wageChange ~ NJ, var.equal = TRUE)

# staffChangeの二つの平均値の差のt検定
var.test(staffChange ~ NJ)
# F検定の結果、p値が0.05よりも小さいので、等分散性が仮定できない
t.test(staffChange ~ NJ, var.equal = FALSE)

# fullPropChangeの二つの平均値の差のt検定
var.test(fullPropChange ~ NJ)
# F検定の結果、p値が0.05よりも大きいので、等分散性が仮定できる
t.test(fullPropChange ~ NJ, var.equal = TRUE)
