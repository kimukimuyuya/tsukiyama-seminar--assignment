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