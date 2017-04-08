##车牌底色代码-- 号牌种类代码,使用性质，机动车大类,机动车细类
da <-
  current3[which(current3$车牌底色代码        != "NA"),c("车牌底色代码","号牌种类代码","使用性质","机动车大类")]
##挑出为NA的数据最后最为测试集
db <-
  current3[which(current3$车牌底色代码        == "NA"),c("车牌底色代码","号牌种类代码","使用性质","机动车大类")]
##载入rpart包
library(rpart)
##训练模型，控制枝干，剔除训练用的属性为na的项
rpart_tree <-
  rpart(
    车牌底色代码     ~ .,control = rpart.control(cp = 0.004),data = da, na.action = na.omit
  )
##训练集中没有号牌种类代码为19的项，剔除测试集中19的项,替换为占比最大的“02”类
db$号牌种类代码[which(db$号牌种类代码      == "19")] <- "02"
##利用测试集做预测，输出种类为对应的类别，type中prob为每类的对应概率
pred1 <- predict(rpart_tree, db, type = "class")
##填充
current3[which(current3$车牌底色代码      == "NA"),"车牌底色代码"] <-
  as.character(pred1)


current3$投保人年龄 <- as.numeric(current3$投保人年龄)
##投保人年龄 33-47均值填充
##检查空值
length(current3[which(is.na(current3$投保人年龄)),"投保人年龄"])   ##384359
##用被保人年龄不为空的填充空的投保人年龄
current3[which(is.na(current3$投保人年龄) &
                 !is.na(current3$被保人年龄)),"投保人年龄"] <-
  current3[which(is.na(current3$投保人年龄) &
                   !is.na(current3$被保人年龄)),"被保人年龄"]
##用投保人年龄不为空的填充空的被保人年龄
current3[which(!is.na(current3$投保人年龄) &
                 is.na(current3$被保人年龄)),"被保人年龄"] <-
  current3[which(!is.na(current3$投保人年龄) &
                   is.na(current3$被保人年龄)),"投保人年龄"]
#检查投保人空值数量
length(current3[which(is.na(current3$投保人年龄)),"投保人年龄"]) ##363109

##填充
len <- length(which(is.na(current3$投保人年龄)))
c <- sample(33:47,len,replace = TRUE)
current3[which(is.na(current3$投保人年龄)),"投保人年龄"] <- c


##被保人年龄 33-47均值填充
len <- length(which(is.na(current3$被保人年龄)))
c <- sample(33:47,len,replace = TRUE)
current3[which(is.na(current3$被保人年龄)),"被保人年龄"] <- c


##实际价值 填充
length(current3[which(current3$实际价值   == current3$新车购置价),"实际价值"])
##训练
da <-
  current3[which(current3$实际价值   != 0),c("实际价值","新车购置价","车辆已使用年限","机动车大类")]
##预测
db <-
  current3[which(current3$实际价值   == 0),c("实际价值","新车购置价","车辆已使用年限","机动车大类")]
##建立模型
lm_value <-
  lm(
    实际价值      ~ .,data = da[c(1:2000000),], na.action = na.omit)
pred1 <- predict(lm_value, da[c(2000001:2524716),])
##评估模型
length(which(abs(da[c(2000001:2524716),"实际价值"] - as.integer(pred1)) > 10000))
##建立完整模型
lm_value <- lm(实际价值        ~ .,data = da, na.action = na.omit)
pred1 <- predict(lm_value,db)
current3[which(current3$实际价值   == 0),"实际价值"] <- pred1


##填充座位数
##da <-
##current3[which(current3$座位数  != 0),c("座位数","机动车大类")]
##预测
##db <-
##current3[which(current3$座位数  == 0),c("座位数","机动车大类")]

##library(rpart)
##r_test_tree<-rpart( 座位数~ .,control = rpart.control(cp = 0.004),data = da[c(1:2000000),])
##pred1 <- predict(r_test_tree, da[c(2000001:2518890),])
##测评
##length(which(abs(da[c(2000001:2518890),"座位数"] - as.integer(pred1)) > 1))

##预测
##r_real_tree<-rpart( 座位数~ .,control = rpart.control(cp = 0.004),data = da)
##pred1 <- as.integer(predict(r_real_tree, db))
##填充
##current3[which(current3$座位数  == 0),"座位数"] <- pred1

current3[which(current3$座位数   == 0),"座位数"] <- 5




##填充排量

##填充
length(current3[which(current3$排量   == 0),"排量"])
current3[which(current3$排量   == 0),"排量"] <-
  round(runif(437118,0.993,1.798),3)

current3$车辆厂家代码 <- NULL
current3$机动车子类 <- NULL

##填充交管车辆类型
da <-
  current3[which(current3$交管车辆类型   != "NA"),c("交管车辆类型","使用性质","机动车大类")]
##预测
db <-
  current3[which(current3$交管车辆类型   == "NA"),c("交管车辆类型","使用性质","机动车大类")]
##训练
r_test_tree <-
  rpart( 交管车辆类型 ~ .,control = rpart.control(cp = 0.004),data = da[c(1:1300000),])
da$机动车大类[which(da$机动车大类      == "J")] <- "A"
pred1 <-
  as.character(predict(r_test_tree, da[c(1300001:1687597),], type = "class"))
table(pred1, da[c(1300001:1687597),"交管车辆类型"])

##预测--837297空
db$机动车大类[which(db$机动车大类      == "J")] <- "A"
pred1 <- as.character(predict(r_test_tree, db, type = "class"))
##填充
current3[which(current3$交管车辆类型   == "NA"),"交管车辆类型"] <- pred1


##填充行驶里程
##da <-
##  current3[which(current3$行使里程  != 0),c("行使里程","车辆已使用年限")]
##预测
##db <-
##current3[which(current3$行使里程  == 0),c("行使里程","车辆已使用年限")]


##r_test_tree<-rpart( 行使里程~ .,control = rpart.control(cp = 0.004),data = da[c(1:1000000),], method = "anova")
##pred1 <- predict(r_test_tree, da[c(1000001:1321910),])
##length(which(abs(da[c(1000001:1321910),"行使里程"] - pred1) > 5000))

##填充
##r_real_tree<-rpart( 行使里程~ .,control = rpart.control(cp = 0.004),data = da)
##pred1 <- predict(r_test_tree, db)
##current3[which(current3$行使里程  == 0),"行使里程"] <- pred1

current3$报案周期 <- as.numeric(current3$报案周期)
current3$理赔周期 <- as.numeric(current3$理赔周期)
current3$支付周期 <- as.numeric(current3$支付周期)

current3$行使里程 <- NULL

##填充空户忠诚度系数
length(temp[which(temp$空户忠诚度系数  == "NA"),"空户忠诚度系数"]) ##-- 47858
c <- sample(c(-0.1,0,0.9,1),47858, replace = TRUE)
temp[which(temp$空户忠诚度系数  == "NA"),"空户忠诚度系数"] <- c



##填充上年浮动原因码
length(temp[which(temp$上年浮动原因码  == "NA"),"上年浮动原因码"]) ##-- 30683
c <-
  sample(
    c(
      "B01", "B02","B1","B11","B12","B13","B14","B15","B2","B3","B31","B32","B33","B34","B35","B4","B5","B6","B7","B7"
    ),30683, replace = TRUE
  )
temp[which(temp$上年浮动原因码  == "NA"),"上年浮动原因码"] <- c


