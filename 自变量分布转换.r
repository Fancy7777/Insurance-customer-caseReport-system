
##取对数
test<-new_merge

hist(test$上年出险次数)
test$上年出险次数<-log(test$上年出险次数)
hist(test$上年出险次数)

hist(test$车辆已使用年限)
test$车辆已使用年限<-log(test$车辆已使用年限)
hist(test$车辆已使用年限)


hist(test$续保年限)
test$续保年限<-log(test$续保年限)
hist(test$续保年限)

hist(test$实际价值)
test$实际价值<-log(test$实际价值)
hist(test$实际价值)

hist(test$连续被续保年限)
test$连续被续保年限<-log(test$连续被续保年限)
hist(test$连续被续保年限)

hist(test$车损险附加险个数)
test$车损险附加险个数<-log(test$车损险附加险个数)
hist(test$车损险附加险个数)

hist(test$车损险保额)
test$车损险保额<-log(test$车损险保额)
hist(test$车损险保额)

hist(test$车损险保费)
test$车损险保费<-log(test$车损险保费)
hist(test$车损险保费)


##取一列里面是真的附上0
for(i in 1:length(test)){
  test[,i][is.infinite(test[,i])]=0
}

for(i in 1:length(test)){
  test[,i][is.nan(test[,i])]=0
}


##取平方
test_sqr<-new_merge

test_sqr$上年出险次数<-(test_sqr$上年出险次数^2)
test_sqr$车辆已使用年限<-(test_sqr$车辆已使用年限^2)
test_sqr$续保年限<-(test_sqr$续保年限^2)
test_sqr$实际价值<-(test_sqr$实际价值^2)
test_sqr$连续被续保年限<-(test_sqr$连续被续保年限^2)
test_sqr$车损险附加险个数<-(test_sqr$车损险附加险个数^2)
test_sqr$车损险保额<-(test_sqr$车损险保额^2)
test_sqr$车损险保费<-(test_sqr$车损险保费^2)

##标准化
test_standarlize<-new_merge

for(i in 1:length(test_standarlize)){
  if(class(test_standarlize[,i]) == "numeric" & names(test_standarlize)[i] !="出险次数"){
    test_standarlize[,i]<-scale(test_standarlize[,i], center = T, scale = T)
  }
}

