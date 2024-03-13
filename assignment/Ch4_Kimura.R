fair <- read.table("presnew.txt", header = TRUE)
attach(fair)
result_fair <- lm(VOTESHR ~ PARTY + PRES + DUR + WAR + GROWTH + INFL + GOODNEWS)
summary(result_fair)
data2008 <- data.frame(PARTY = c(-1), PRES = c(0), DUR = c(1), WAR = c(0), GROWTH = c(-2.260), INFL = c(3.052), GOODNEWS = c(1))
predict(result_fair, data2008, interval = "prediction", level = 0.95)
predict(result_fair, data2008, interval = "prediction", level = 0.90)

erikson <- read.csv("erikson.csv")
attach(erikson)
result_erikson <- lm(V ~ I + C)
summary(result_erikson)
IC <- I*C
result_erikson2 <- lm(V ~ I + C + IC)
summary(result_erikson2)
#(β3 + β4I)の信頼区間を求めて、Iの値による変化を見る
##分散共分散行列の計算
vcov(result_erikson2)
##Iの値が変化したときの(β3 + β4I)の変化
i2 <- seq(-0.8, 4.8, by = 0.2)
##Iに代わって(β3 + β4I)に代入
int <- result_erikson2$coef[3] + result_erikson2$coef[4] * i2
se_int <- sqrt(vcov(result_erikson2)[3, 3] + i2^2 * vcov(result_erikson2)[4, 4] + 2 * i2 * vcov(result_erikson2)[3, 4])
##信頼区間の計算
int_ll <- int - 2.447 * se_int
int_ul <- int + 2.447 * se_int
idata <- data.frame(I2 = i2, INT = int, INTLL = int_ll, INTUL = int_ul)
idata
jpeg (filename = "erikson.jpg")
matplot(idata[,1], idata[,2:4], type = "l", xlab = "I", ylab = "CのVに対する影響", lty = c(1,2,2), lwd = c(2,1,1), col = c("black", "black", "black"))
abline(h = 0, lty = 1, lwd = 1)
legend(1,max(idata), legend = colnames <- c("推定値", "95%信頼区間"), lty = c(1,2), lwd = c(2,1))
dev.off()
        
        
        
        