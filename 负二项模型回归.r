
##零膨胀负二项回归
train_data <- new_merge[train,]
test_data <- new_merge[-train,]

nbi_zip <- gamlss(
  出险次数           ~            上年出险次数+
    上年浮动原因码merge +
    使用性质+
    车辆已使用年限+
    续保年限+
    行驶区域代码merge +
    分公司代码+
    业务渠道+
    实际价值+
    被保人性别+
    连续被续保年限+
    被续保标志+
    车辆生产国别+
    被保人年龄+
    是否约定驾驶员标志+
    被保人与车辆关系+
    号牌种类代码merge +
    本车是否投保不计免赔标识 +
    车损险附加险个数+
    车损险保费 ,data = train_data, family = ZINBI
)

负二项回归 <-
  predict(nbi_zip, newdata = test_data, type = "response")
quantile(负二项回归)
plot(负二项回归)
负二项回归[which(负二项回归 > 0 &
                   负二项回归 < 0.72)] <- 0
负二项回归[which(负二项回归 >= 0.72 &
                   负二项回归 < 1.5)] <- 1
负二项回归 <- ceiling(负二项回归)

length(which(abs(new_merge$出险次数[-train] - 负二项回归) >= 1))##58589


##零膨胀负二项回归_显著性检验后_自变量取对数
train_data_test <- test[train,]
test_data_test <- test[-train,]


nbi_zip_显著性检验后_自变量取对数 <- gamlss(
                                出险次数           ~            上年出险次数+
                                  上年浮动原因码merge +
                                  使用性质+
                                  车辆已使用年限+
                                  续保年限+
                                  行驶区域代码merge +
                                  分公司代码+
                                  业务渠道+
                                  实际价值+
                                  被保人性别+
                                  连续被续保年限+
                                  被续保标志+
                                  车辆生产国别+
                                  被保人年龄+
                                  是否约定驾驶员标志+
                                  被保人与车辆关系+
                                  号牌种类代码merge +
                                  本车是否投保不计免赔标识 +
                                  车损险附加险个数+
                                  车损险保费,data = train_data_test, family = ZINBI
)

负二项回归_显著性检验后_自变量取对数 <-
  predict(nbi_zip_显著性检验后_自变量取对数, newdata = test_data_test, type = "response")
quantile(负二项回归_显著性检验后_自变量取对数)
plot(负二项回归_显著性检验后_自变量取对数)
负二项回归_显著性检验后_自变量取对数[which(负二项回归_显著性检验后_自变量取对数 > 0 &
                            负二项回归_显著性检验后_自变量取对数 < 0.72)] <- 0
负二项回归_显著性检验后_自变量取对数[which(负二项回归_显著性检验后_自变量取对数 >= 0.72 &
                            负二项回归_显著性检验后_自变量取对数 < 1.5)] <- 1
负二项回归_显著性检验后_自变量取对数 <- ceiling(负二项回归_显著性检验后_自变量取对数)

length(which(abs(test_data_test$出险次数 - 负二项回归_显著性检验后_自变量取对数) >= 1))##58295


##零膨胀负二项回归_显著性检验后_自变量平方后
train_data_test_sqr <- test_sqr[train,]
test_data_test_sqr <- test_sqr[-train,]

零膨胀负二项回归_显著性检验后_自变量取平方后      <- gamlss(
                                       出险次数           ~            上年出险次数+
                                         上年浮动原因码merge +
                                         使用性质+
                                         车辆已使用年限+
                                         续保年限+
                                         行驶区域代码merge +
                                         分公司代码+
                                         业务渠道+
                                         实际价值+
                                         被保人性别+
                                         连续被续保年限+
                                         被续保标志+
                                         车辆生产国别+
                                         被保人年龄+
                                         是否约定驾驶员标志+
                                         被保人与车辆关系+
                                         号牌种类代码merge +
                                         本车是否投保不计免赔标识 +
                                         车损险附加险个数+
                                         车损险保费 ,data = train_data_test_sqr, family = ZINBI
)
显著性检验后预测_自变量取平方后      <-
  predict(零膨胀负二项回归_显著性检验后_自变量取平方后, newdata = test_data_test, type = "response")
quantile(显著性检验后预测_自变量取平方后)

显著性检验后预测_自变量取平方后[which(显著性检验后预测_自变量取平方后      > 0 &
                         显著性检验后预测_自变量取平方后      < 0.66)] <- 0
显著性检验后预测_自变量取平方后[which(显著性检验后预测_自变量取平方后      >= 0.66 &
                         显著性检验后预测_自变量取平方后      < 1.1)] <- 1
显著性检验后预测_自变量取平方后      <- ceiling(显著性检验后预测_自变量取平方后)
length(which(abs(test_data_test_sqr$出险次数 -      显著性检验后预测_自变量取平方后) >= 1))##59727



##零膨胀负二项回归_显著性检验后_自变量标准化后
train_data_test_standarlize <- test_standarlize[train,]
test_data_test_standarlize <- test_standarlize[-train,]
零膨胀负二项回归_显著性检验后_自变量标准化后      <- gamlss(
                                       出险次数           ~            上年出险次数+
                                         上年浮动原因码merge +
                                         使用性质+
                                         车辆已使用年限+
                                         续保年限+
                                         行驶区域代码merge +
                                         分公司代码+
                                         业务渠道+
                                         实际价值+
                                         被保人性别+
                                         连续被续保年限+
                                         被续保标志+
                                         车辆生产国别+
                                         被保人年龄+
                                         是否约定驾驶员标志+
                                         被保人与车辆关系+
                                         号牌种类代码merge +
                                         本车是否投保不计免赔标识 +
                                         车损险附加险个数+
                                         车损险保费 ,data = train_data_test_standarlize, family = ZINBI
)
显著性检验后预测_自变量标准化后      <-
  predict(零膨胀负二项回归_显著性检验后_自变量标准化后, newdata = test_data_test_standarlize, type = "response")

quantile(显著性检验后预测_自变量标准化后)
显著性检验后预测_自变量标准化后[which(显著性检验后预测_自变量标准化后      > 0 &
                         显著性检验后预测_自变量标准化后      < 0.72)] <- 0
显著性检验后预测_自变量标准化后[which(显著性检验后预测_自变量标准化后      >= 0.72 &
                         显著性检验后预测_自变量标准化后      < 1.5)] <- 1
显著性检验后预测_自变量标准化后      <- ceiling(显著性检验后预测_自变量标准化后)
length(which(abs(new_merge$出险次数[-train] -      显著性检验后预测_自变量标准化后) >= 1))##58589

