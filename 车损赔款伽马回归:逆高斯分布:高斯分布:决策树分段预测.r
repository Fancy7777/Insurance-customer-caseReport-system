
##高斯模型
##检验是否符合高斯模型
车损赔款 <- new_train[,"车损赔款"]
ks.test(车损赔款,"pnorm")
qqnorm(车损赔款)
qqline(车损赔款)

train_data <- new_train[train,]
test_data <- new_train[-train,]

##高斯分布，因变量为y
mod1 <- glm(
  车损赔款  ~  分公司代码+
    车损险保额+
    保单是否有免赔额+
    车损险附加险个数+
    是否批改关系人+
    是否批改驾驶人+
    实际价值+
    车辆已使用年限+
    车辆生产国别+
    机动车大类+
    是否投保新增设备标志+
    座位数+
    排量+
    使用性质+
    出险次数+
    被保人性别+
    被保人年龄, data = train_data, family = gaussian)
plot(train_data$车损赔款~fitted(mod1), xlab = "mod1的拟合")
abline(0,1)


train_data_temp2 <- temp2[train,]
test_data_temp2 <- temp2[-train,]
##高斯分布，因变量为log（y）
mod2 <- glm(
  车损赔款  ~  分公司代码+
    车损险保额+
    保单是否有免赔额+
    车损险附加险个数+
    是否批改关系人+
    是否批改驾驶人+
    实际价值+
    车辆已使用年限+
    车辆生产国别+
    机动车大类+
    是否投保新增设备标志+
    座位数+
    排量+
    使用性质+
    出险次数+
    被保人性别+
    被保人年龄, data = train_data_temp2, family = gaussian)
plot(train_data$车损赔款~fitted(mod2), xlab = "mod2的拟合")
abline(0,1)

train_data_temp21 <- temp2[train,]
test_data_temp21 <- temp2[-train,]
##高斯分布，因变量为log（y）
mod3 <- glm(
  车损赔款  ~  分公司代码+
    车损险保额+
    保单是否有免赔额+
    车损险附加险个数+
    是否批改关系人+
    是否批改驾驶人+
    实际价值+
    车辆已使用年限+
    车辆生产国别+
    机动车大类+
    是否投保新增设备标志+
    座位数+
    排量+
    使用性质+
    出险次数+
    被保人性别+
    被保人年龄, data = train_data_temp21, family = gaussian)
plot(train_data$车损赔款~fitted(mod3), xlab = "mod3的拟合")
abline(0,1)




##伽马分布
second_train<-new_train
second_train[which(second_train$车损赔款<0),]<-0

third_train<-new_train[which(new_train$车损赔款>0),]

train_data <- new_train[train,]
test_data <- new_train[-train,]

##伽马分布，自变量因变量都取对数

train<- sample((1:193598),154872)

train_data<-test2[train,]
test_data<-test2[-train,]

gamma1 <- glm(
  车损赔款  ~  上年出险次数+
    上年浮动原因码+
    机动车细类+
    机动车大类+
    标的车辆种类+
    险种代码+
    三者险附加险个数+
    保单是否有免赔额+
    三者险是否投保不计免赔标识+
    本车是否投保不计免赔标识+
    使用性质+
    车损险保费+
    被保人是否有电话+
    车损险附加险个数+
    交管车辆类型+
    被保人是否有身份证号+
    车辆已使用年限+
    总保费+
    被保人年龄+
    行驶区域代码+
    车损险保额+
    总折扣金额+
    新车购置价+
    分公司代码+
    业务渠道+
    实际价值+
    手续费比例+
    号牌种类代码+
    三者险限额+
    是否约定驾驶员标志+
    车牌底色代码+
    上年赔款金额+
    被保人性别+
    附加乘员险保额+
    附加乘员险保费+
    被保人与车辆关系+
    座位数+
    总保额+
    排量+
    车辆生产国别+
    是否投保新增设备标志+
    三者险保费
    , data = train_data, family =Gamma(link= log))

pred1<-exp(predict(gamma1,newdata = test_data))
pred<-exp(predict(gamma1, test_data))
pred<-exp(predict(gamma1, newdata = test_data))
quantile(pred1)
pred<-pred[which(pred<20000)]
length(which(abs(test_data$车损赔款 - pred)>2000))
max(abs(test_data$车损赔款 - pred))
plot(gamma1)


##逆高斯分布
second_train<-new_train
second_train[which(second_train$车损赔款<0),]<-0
second_train[which(second_train$车损险附加险个数<0),]<-0
second_train[which(second_train$实际价值<0),]<-0
second_train[which(second_train$车损险附加险个数<0),]<-0

third_train<-new_train[which(new_train$车损赔款>0),]

train_data <- temp2[train,]
test_data <- temp2[-train,]

##逆高斯分布，自变量因变量都取对数
train_data <- new_train[train,]
test_data <- new_train[-train,]

train_data[which(train_data$车损赔款<=0),]<-1
test<-new_train[which(new_train$车损赔款>0),]

gamma1 <- glm(
  车损赔款  ~  分公司代码+
    车损险保额+
    保单是否有免赔额+
    车损险附加险个数+
    上年出险次数+
    实际价值+
    车辆已使用年限+
    车辆生产国别+
    机动车大类+
    是否投保新增设备标志+
    座位数+
    排量+
    使用性质+
    被保人性别+
    被保人年龄, data = test, family = Gamma(link = log))

plot(gamma1)


inverse_gaus <- glm(
  车损赔款  ~  分公司代码+
    车损险保额+
    保单是否有免赔额+
    车损险附加险个数+
    上年出险次数+
    实际价值+
    车辆已使用年限+
    车辆生产国别+
    机动车大类+
    是否投保新增设备标志+
    座位数+
    排量+
    使用性质+
    被保人性别+
    被保人年龄, data = test, family = inverse.gaussian(link = log))

plot(gamma2)

test<-new_train

fenduan<-new_merge
fenduan[which(fenduan$车损赔款 < 0),"车损赔款"]<-"0"
fenduan[which(fenduan$车损赔款 == 0),"车损赔款"]<-"0"
fenduan[which(fenduan$车损赔款 > 0 & fenduan$车损赔款<=1000),"车损赔款"]<-"0-1000"
fenduan[which(fenduan$车损赔款 > 0 & fenduan$车损赔款<=1000),"车损赔款"]<-"0-1000"
fenduan[which(fenduan$车损赔款 > 1000 & fenduan$车损赔款<=2000),"车损赔款"]<-"1000-2000"
fenduan[which(fenduan$车损赔款 > 2000 & fenduan$车损赔款<=4000),"车损赔款"]<-"2000-4000"
fenduan[which(fenduan$车损赔款 > 4000),"车损赔款"]<-"4000+"


##决策树分段预测
train_data<-fenduan[train,]
test_data<-fenduan[-train,]

rpart_tree <-
  rpart(车损赔款~
    上年出险次数+
      上年浮动原因码merge +
      机动车大类+
      标的车辆种类merge+
      险种代码+
      使用性质+
      交管车辆类型merge+
      车辆已使用年限+
      续保年限+
      续保标志+
      行驶区域代码merge+
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
      号牌种类代码merge+
      本车是否投保不计免赔标识 +
      车损险附加险个数+
      车损险保额+
      车损险保费,data = train_data
  )
tree_pred<-predict(rpart_tree,test_data, type = "class")

##决策树分0和不分0预测
fenduan_0_not0<-fenduan
fenduan_0_not0[which(fenduan$车损赔款 != "0"),"车损赔款"]<-"1"
fenduan_0_not0[which(fenduan$车损赔款 == "0"),"车损赔款"]<-"0"

train_0<-fenduan_0_not0[train,]
test_0<-fenduan_0_not0[-train,]

tree_zero_predict <-
  glm(
    车损赔款  ~  分公司代码+
      车损险保额+
      保单是否有免赔额+
      车损险附加险个数+
      上年出险次数+
      实际价值+
      车辆已使用年限+
      车辆生产国别+
      机动车大类+
      是否投保新增设备标志+
      座位数+
      排量+
      使用性质+
      被保人性别+
      被保人年龄,data = train_0, family = binomial(link = logit)
  )
zero_pred<-predict(tree_zero_predict, test_0,type = "response")
zero_pred[which(zero_pred>0.5)]<-1
zero_pred[which(zero_pred<=0.5)]<-0
