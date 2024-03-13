STAR <- read.csv("STAR.csv")
attach(STAR)
white <- race
white[race == 1] <- 1
white[race != 1] <- 0
g1classsize_white <- g1classsize*white
result_lm <- lm(g1treadss ~ g1classsize + white)
summary(result_lm)

# 交互作用項
result_lm2 <- lm(g1treadss ~ g1classsize + white + g1classsize_white)
summary(result_lm2)
vcov(result_lm2)
clean_data <- na.omit(STAR$g1classsize)
min_g1classsize <- min(clean_data)
max_g1classsize <- max(clean_data)
g1classsize_seq <- seq(min_g1classsize, max_g1classsize, by = 1)
int <- result_lm2$coef[3] + result_lm2$coef[4] * g1classsize_seq
se_int <- sqrt(vcov(result_lm2)[3, 3] + g1classsize_seq^2 * vcov(result_lm2)[4, 4] + 2 * g1classsize_seq * vcov(result_lm2)[3, 4])
# 120よりも多くの標本があるため、t値を1.96とする
int_ll <- int - 1.96 * se_int
int_ul <- int + 1.96 * se_int
idata <- data.frame(g1classsize_seq = g1classsize_seq, INT = int, INTLL = int_ll, INTUL = int_ul)
jpeg(filename = "STAR.jpg")
matplot(idata[,1], idata[,2:4], type = "l", xlab = "学級規模", ylab = "白人と非白人の生徒の読解のSATテストの点数の差の平均", lty = c(1,2,2), lwd = c(2,1,1), col = c("black", "black", "black"))
legend(1,max(idata), legend = colnames <- c("推定値", "95%信頼区間"), lty = c(1,2), lwd = c(2,1))
dev.off()