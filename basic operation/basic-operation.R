childcare <- read.csv('childcare.csv')
childcare$認可保育園内定率 <- childcare$認可保育園内定者数 / childcare$認可保育園申込者数
childcare$内定困難区 <- 0
childcare$内定困難区[childcare$認可保育園内定率 < 0.6] <- 1
childcare$内定困難区[is.na(childcare$認可保育園内定率)] <- NA
##childcare.1 <- subset(childcare, 内定困難区 == 1)
##潜在的保育サービス利用者数
childcare$潜在的保育サービス利用者数 <- childcare$保育サービス利用児童数 + childcare$待機児童数
##待機児童率
childcare$待機児童率 <- childcare$待機児童数 / childcare$潜在的待機児童数
##隠れ待機児童率
childcare$隠れ待機児童率 <- (childcare$待機児童数 + childcare$隠れ待機児童数) / childcare$潜在的保育サービス利用者数