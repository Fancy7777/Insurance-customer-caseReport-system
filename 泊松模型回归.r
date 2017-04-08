##零膨胀泊松回归
train_data <- new_merge[train,]
test_data <- new_merge[-train,]

possion_zip <- gamlss(
  出险次数         ~          上年出险次数+
    上年浮动原因码merge +
    机动车大类+
    标的车辆种类merge +
    险种代码+
    使用性质+
    交管车辆类型merge +
    车辆已使用年限+
    续保年限+
    续保标志+
    行驶区域代码merge +
    分公司代码+
    业务渠道+
    实际价值+
    车牌底色代码+
    投保人性别+
    被保人性别+
    连续被续保年限+
    被续保标志+
    车辆生产国别+
    被保人年龄+
    是否约定驾驶员标志+
    是否投保新增设备标志+
    被保人与车辆关系+
    号牌种类代码merge +
    本车是否投保不计免赔标识 +
    车损险附加险个数+
    车损险保额+
    车损险保费 ,data = train_data, family = ZIP
)
predict_zip <-
  predict(possion_zip, newdata = test_data, type = "response")
quantile(predict_zip)
predict_zip[which(predict_zip > 0 &
                    predict_zip < 0.7)] <- 0
predict_zip[which(predict_zip >= 0.7 &
                    predict_zip < 1.2)] <- 1
predict_zip <- ceiling(predict_zip)
length(which(abs(new_merge$出险次数[-train] - predict_zip) >= 1))
table(predict_zip,new_merge$出险次数[-train])

length(which(abs(new_merge$出险次数[-train] - predict_zip) >= 1))##58525

##零膨胀泊松回归_显著性检验后
零膨胀泊松回归_显著性检验后 <- gamlss(
                         出险次数      ~       上年出险次数+
                           上年浮动原因码merge +
                           使用性质+
                           车辆已使用年限+
                           续保年限+
                           续保标志+
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
                           是否投保新增设备标志+
                           被保人与车辆关系+
                           号牌种类代码merge +
                           本车是否投保不计免赔标识 +
                           车损险附加险个数+
                           车损险保额+
                           车损险保费 ,data = train_data, family = ZIP
)
summary(零膨胀泊松回归_显著性检验后)
显著性检验后预测 <-
  predict(零膨胀泊松回归_显著性检验后, newdata = test_data, type = "response")
quantile(显著性检验后预测)
predict_zip[which(predict_zip > 0 &
                    predict_zip < 0.72)] <- 0
predict_zip[which(predict_zip >= 0.72 &
                    predict_zip < 1.5)] <- 1
predict_zip <- ceiling(predict_zip)
length(which(abs(test_data - 显著性检验后预测) >= 1))
length(which(abs(test_data$出险次数 - 显著性检验后预测) >= 1))
table(显著性检验后预测,new_merge$出险次数[-train])
length(which(abs(new_merge$出险次数[-train] - 显著性检验后预测) >= 1))




##零膨胀泊松回归_显著性检验后_自变量取对数后
train_data_test <- test[train,]
test_data_test <- test[-train,]
零膨胀泊松回归_显著性检验后_自变量取对数后      <- gamlss(
                                      出险次数           ~            上年出险次数+
                                        上年浮动原因码merge +
                                        使用性质+
                                        车辆已使用年限+
                                        续保年限+
                                        续保标志+
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
                                        是否投保新增设备标志+
                                        被保人与车辆关系+
                                        号牌种类代码merge +
                                        本车是否投保不计免赔标识 +
                                        车损险附加险个数+
                                        车损险保额+
                                        车损险保费 ,data = train_data_test, family = ZIP
)
显著性检验后预测_自变量取对数后      <-
  predict(零膨胀泊松回归_显著性检验后_自变量取对数后, newdata = test_data_test, type = "response")
quantile(显著性检验后预测_自变量取对数后)
显著性检验后预测_自变量取对数后[which(显著性检验后预测_自变量取对数后      > 0 &
                         显著性检验后预测_自变量取对数后      < 0.72)] <- 0
显著性检验后预测_自变量取对数后[which(显著性检验后预测_自变量取对数后      >= 0.72 &
                         显著性检验后预测_自变量取对数后      < 1.5)] <- 1
显著性检验后预测_自变量取对数后      <- ceiling(显著性检验后预测_自变量取对数后)
length(which(abs(new_merge$出险次数[-train] -      显著性检验后预测_自变量取对数后) >= 1))##58279


##零膨胀泊松回归_显著性检验后_自变量平方后
train_data_test_sqr <- test_sqr[train,]
test_data_test_sqr <- test_sqr[-train,]

零膨胀泊松回归_显著性检验后_自变量取平方后      <- gamlss(
                                      出险次数           ~            上年出险次数+
                                        上年浮动原因码merge +
                                        使用性质+
                                        车辆已使用年限+
                                        续保年限+
                                        续保标志+
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
                                        是否投保新增设备标志+
                                        被保人与车辆关系+
                                        号牌种类代码merge +
                                        本车是否投保不计免赔标识 +
                                        车损险附加险个数+
                                        车损险保额+
                                        车损险保费 ,data = train_data_test_sqr, family = ZIP
)
显著性检验后预测_自变量取平方后      <-
  predict(零膨胀泊松回归_显著性检验后_自变量取平方后, newdata = test_data_test, type = "response")
quantile(显著性检验后预测_自变量取对数后)

显著性检验后预测_自变量取平方后[which(显著性检验后预测_自变量取平方后      > 0 &
                         显著性检验后预测_自变量取平方后      < 0.72)] <- 0
显著性检验后预测_自变量取平方后[which(显著性检验后预测_自变量取平方后      >= 0.72 &
                         显著性检验后预测_自变量取平方后      < 1.5)] <- 1
显著性检验后预测_自变量取平方后      <- ceiling(显著性检验后预测_自变量取平方后)
length(which(abs(new_merge$出险次数[-train] -      显著性检验后预测_自变量取平方后) >= 1))##55135
table(显著性检验后预测_自变量取平方后,new_merge$出险次数[-train])



##零膨胀泊松回归_显著性检验后_自变量标准化后
train_data_test_standarlize <- test_standarlize[train,]
test_data_test_standarlize <- test_standarlize[-train,]
零膨胀泊松回归_显著性检验后_自变量标准化后      <- gamlss(
                                      出险次数           ~            上年出险次数+
                                        上年浮动原因码merge +
                                        使用性质+
                                        车辆已使用年限+
                                        续保年限+
                                        续保标志+
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
                                        是否投保新增设备标志+
                                        被保人与车辆关系+
                                        号牌种类代码merge +
                                        本车是否投保不计免赔标识 +
                                        车损险附加险个数+
                                        车损险保额+
                                        车损险保费 ,data = train_data_test_standarlize, family = ZIP
)
显著性检验后预测_自变量标准化后      <-
  predict(零膨胀泊松回归_显著性检验后_自变量标准化后, newdata = test_data_test_standarlize, type = "response")
quantile(显著性检验后预测_自变量标准化后)
显著性检验后预测_自变量标准化后[which(显著性检验后预测_自变量标准化后      > 0 &
                         显著性检验后预测_自变量标准化后      < 0.72)] <- 0
显著性检验后预测_自变量标准化后[which(显著性检验后预测_自变量标准化后      >= 0.72 &
                         显著性检验后预测_自变量标准化后      < 1.5)] <- 1
显著性检验后预测_自变量标准化后      <- ceiling(显著性检验后预测_自变量标准化后)
length(which(abs(new_merge$出险次数[-train] -      显著性检验后预测_自变量标准化后) >= 1))##58558





##改字段

train_data_test_standarlize <- test_standarlize[train,]
test_data_test_standarlize <- test_standarlize[-train,]

零膨胀泊松回归_显著性检验后_自变量标准化后      <- gamlss(
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
                                        车损险保费 ,data = train_data_test_standarlize, family = ZIP
)
显著性检验后预测_自变量标准化后      <-
  predict(零膨胀泊松回归_显著性检验后_自变量标准化后, newdata = test_data_test_standarlize, type = "response")
quantile(显著性检验后预测_自变量标准化后)
显著性检验后预测_自变量标准化后[which(显著性检验后预测_自变量标准化后      > 0 &
                         显著性检验后预测_自变量标准化后      < 0.72)] <- 0
显著性检验后预测_自变量标准化后[which(显著性检验后预测_自变量标准化后      >= 0.72 &
                         显著性检验后预测_自变量标准化后      < 1.5)] <- 1
显著性检验后预测_自变量标准化后      <- ceiling(显著性检验后预测_自变量标准化后)
length(which(abs(new_merge$出险次数[-train] -      显著性检验后预测_自变量标准化后) >= 1))##58558

