crime <- read.csv("crime.csv")
attach(crime)
unemployedChange <- unemployed10 - unemployed05
crimeChange <- crime10 - crime05

# 散布図の作成
jpeg(filename = "crime.jpeg")
plot(unemployedChange, crimeChange, type = "n")
text(unemployedChange, crimeChange, pref, cex = 0.6)
dev.off()

# 単回帰分析
result_lm <- lm(crimeChange ~ unemployedChange)
summary(result_lm)