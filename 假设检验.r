##正态性检验
shapiro.test(new$车损赔款[which(new$险种代码 == '0520')])#--只适合于小样本

qqnorm(new$车损赔款[which(new$险种代码 == '0520')]); 
qqline(new$车损赔款[which(new$险种代码 == '0520')])#QQ图


ks.test(new$车损赔款[which(new$险种代码 == '0502')],"pnorm",10000,50799)#不能有重复的数
ks.test(jitter(new$车损赔款[which(new$险种代码 == '0502')]),"pnorm")#加噪声即可
ks.test(new$车损赔款[which(new$险种代码 == '0502')] + runif(length(new$车损赔款[which(new$险种代码 ==
                                                                            '0502')]),-0.05, 0.05),"pnorm")

library(nortest)
a <- as.data.frame(table(new[,62]));
ad.test(new[which(new[,2] == a[1,1]),"车损赔款"])
lillie.test(new[which(new[,62] == a[5,1]),"车损赔款"])

##方差齐次性检验
bartlett.test(new$车损赔款, new$险种代码)#方差齐次性检验

##方差分析
v <- aov(车损赔款  ~  使用性质, data = new)
summary(v)

ks <- list()
bt <- list()
v <- list()
com <- function(x) {
  for (i in 1:nrow(x)) {
    if (is.character(x[,i]) & names(x)[i] != "保单号") {
      a <- as.data.frame(table(x[,i]))
      
      for (j in 1:nrow(a)) {
        ks[[paste(names(x)[i],a[j,1])]] <<-
          lillie.test(x[which(x[,i] == a[j,1]),"车损赔款"])#正态性检验
      }
      
      bt[[names(x)[i]]] <<- bartlett.test(new[,"车损赔款"], new[,i])#方差齐次性检验
      v[[names(x)[i]]] <<- summary(aov(车损赔款  ~ x[,i], data = x))#方差分析
    }
  }
}
com(new)
