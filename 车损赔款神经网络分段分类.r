temp<-new_train
fenduan<-temp
fenduan[which(fenduan$车损赔款 < 0),"车损赔款"]<-"0"
fenduan[which(fenduan$车损赔款 == 0),"车损赔款"]<-"0"
fenduan[which(fenduan$车损赔款 > 0 & fenduan$车损赔款<=1000),"车损赔款"]<-"0-1000"
fenduan[which(fenduan$车损赔款 > 0 & fenduan$车损赔款<=1000),"车损赔款"]<-"0-1000"
fenduan[which(fenduan$车损赔款 > 1000 & fenduan$车损赔款<=2000),"车损赔款"]<-"1000-2000"
fenduan[which(fenduan$车损赔款 > 2000 & fenduan$车损赔款<=4000),"车损赔款"]<-"2000-4000"
fenduan[which(fenduan$车损赔款 > 4000),"车损赔款"]<-"4000+"

temp<-fenduan

temp$保单号<-NULL

transf <- function(table_use) {
  ##run throught every column
  for (i in (1:length(table_use))) {
    ## run through every column which is not numeric type except "claimno_claim"
    if (class(table_use[,i]) == "factor" &&
        names(table_use)[i] != "车损赔款") {
      ##run through every element in this column
      for (j in (1:length(table(table_use[,i])))) {
        pos <-
          which(levels(table_use[,i]) == names(table(table_use[,i]))[j])
        value <-
          take_freq(table_use[,i],names(table(table_use[,i]))[j]) / length(table_use[,i])
        levels(temp[,i])[pos] <<- value
      }
    }
  }
  return(table_use)
}


##take the frequency out from one column
take_freq <- function(column, elem) {
  temp <- as.data.frame(table(column))
  ##loop the table() and return the corresponding the frequence
  for (i in (1:nrow(temp))) {
    if (elem == temp[i,1]) {
      return(temp[i,2])
    }
  }
}

##换成factor形式
transfactor <- function(table_use){
  for (i in (1:length(table_use))) {
    if(is.character(table_use[,i])){
      temp[,i]<<-as.factor(table_use[,i])
    }
  }
}


transnumeric <- function(table_use){
  for (i in (1:length(table_use))) {
    if(is.factor(table_use[,i]) && names(table_use)[i] != "车损赔款"){
      temp[,i]<<-as.numeric(as.character(table_use[,i]))
    }
  }
}

temp$车损赔款<-as.character(temp$车损赔款)

##用最大值、最小值函数归一化
temp <- as.data.frame(lapply(temp,function(x) {
  if (is.character(x))
    x
  else
    scale(x)
}))

temp$保单号<-NULL
temp$被续保标志<-NULL
temp$连续被续保年限<-NULL
temp$续保标志<-NULL
temp$续保年限<-NULL
temp$批改次数<-NULL
temp$是否批改关系人<-NULL
temp$是否批改驾驶人<-NULL
temp$车损赔款<-NULL
temp$赔付金额<-NULL
temp$案均赔款<-NULL
temp$报案周期<-NULL
temp$理赔周期<-NULL
temp$支付周期<-NULL

temp2<-temp
temp2$车损赔款<-as.character(temp2$车损赔款)
temp2[which(temp2$车损赔款 != "0"),"车损赔款"]<-">0"
temp2$车损赔款<-as.factor(temp2$车损赔款)
temp<-temp2

library(RSNNS)
name<-names(temp)
name<-name[which(name != "车损赔款")]
dataValues= temp[,c(name)]
#定义网络输出，并将数据进行格式转换

dataTargets = decodeClassLabels(temp$车损赔款)
head(dataTargets )

#从中划分出训练样本和检验样本
temp2 = splitForTrainingAndTest(dataValues, dataTargets, ratio=0.25)
#数据标准化
temp2 = normTrainingAndTestSet(temp2)
#利用mlp命令执行前馈反向传播神经网络算法
model = mlp(temp2$inputsTrain,temp2$targetsTrain, size=c(10,5),learnFuncParams=c(0.2,0),
            maxit=200, inputsTest=temp2$inputsTest, targetsTest=temp2$targetsTest)

predictions = predict(model,temp2$inputsTest)
#生成混淆矩阵，观察预测精度
tab1<-confusionMatrix(temp2$targetsTest,predictions)
tab2<-confusionMatrix(temp2$targetsTrain,fitted.values(model))
confusionMatrix(temp2$targetsTrain,fitted.values(model)) 
tab1[2,2]/apply(tab1,1,sum)[2]
tab1[2,2]/apply(tab1,2,sum)[2]
(tab1[1,1]+tab1[2,2])/92222#AUC
