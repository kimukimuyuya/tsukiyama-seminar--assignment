putnam <- read.csv("putnam.csv")
attach(putnam)
jpeg(filename = "ip_cc.jpg")
plot(cc, ip, main = "The Civic Community and Institutional Performance", xlab = "Civic Community", ylab = "Institutional Performance")
dev.off()

jpeg(filename = "ip_cc2.jpg")
plot(cc, ip, main = "The Civic Community and Institutional Performance", xlab = "Civic Community", ylab = "Institutional Performance", type = "n")
text(cc, ip, region)
dev.off()

#相関係数の計算
result_cor <- cor.test(ip, cc)
result_cor

#回帰分析
result_lm <- lm(ip ~ cc)
summary(result_lm)

#政府のパフォーマンスと南北地域
t.test(ip ~ ns, var.equal = TRUE)
#政府パフォーマンスと近代化の度合いの単回帰分析
result_lm2 <- lm(ip ~ em)
summary(result_lm2)

#政府パフォーマンスと近代化の度合い+南北地域の重回帰分析
result_lm6 <- lm(ip ~ em + ns)
summary(result_lm6)

#政府パフォーマンスと社会資本関係+南北地域の重回帰分析
result_lm7 <- lm(ip ~ cc + ns)
summary(result_lm7)