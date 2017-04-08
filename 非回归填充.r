##填充车辆生产国别
c<-c(names(table(current2$车辆生产国别[current$车辆生产国别 != "NA"])))
vec<-as.vector(prop.table(table(current2$车辆生产国别[current$车辆生产国别 != "NA"])))
len<-length(which(current2$车辆生产国别 == "NA"))
tian<-sample(c,len, prob = vec, replace = TRUE)
current2[which(current2$车辆生产国别   == "NA"),"车辆生产国别"] <-tian
#检查
table(current2$车辆生产国别)a

##填充机动车大类
table(current2$机动车大类)
current2[which(current2$机动车大类 == "NA"),"机动车大类"]<-"A"
##检查
table(current2$机动车大类)

##填充机动车细类
table(current2$机动车细类)
current2[which(current2$机动车细类 == "NA"),"机动车细类"]<-"01"
##检查
table(current$机动车细类)

##填充号牌种类代码
c<-c(names(table(current2$号牌种类代码[current$号牌种类代码 != "NA"])))
vec<-as.vector(prop.table(table(current2$号牌种类代码[current$号牌种类代码 != "NA"])))
len<-length(which(current2$号牌种类代码 == "NA"))
tian<-sample(c,len, prob = vec, replace = TRUE)
current2[which(current2$号牌种类代码   == "NA"),"号牌种类代码"] <-tian
#检查
table(current2$号牌种类代码)

##填充是否约定驾驶员标志
c<-c(names(table(current2$是否约定驾驶员标志[current$是否约定驾驶员标志 != "NA"])))
vec<-as.vector(prop.table(table(current2$是否约定驾驶员标志[current$是否约定驾驶员标志 != "NA"])))
len<-length(which(current2$是否约定驾驶员标志 == "NA"))
tian<-sample(c,len, prob = vec, replace = TRUE)
current2[which(current2$是否约定驾驶员标志   == "NA"),"是否约定驾驶员标志"] <-tian
#检查
table(current2$是否约定驾驶员标志)

##填充是否投保新增设备标志
c<-c(names(table(current2$是否投保新增设备标志[current$是否投保新增设备标志 != "NA"])))
vec<-as.vector(prop.table(table(current2$是否投保新增设备标志[current$是否投保新增设备标志 != "NA"])))
len<-length(which(current2$是否投保新增设备标志 == "NA"))
tian<-sample(c,len, prob = vec, replace = TRUE)
current2[which(current2$是否投保新增设备标志   == "NA"),"是否投保新增设备标志"] <-tian
#检查
table(current2$是否投保新增设备标志)

##填充出险次数
table(current2$出险次数)
current2[which(is.na(current2$出险次数)),"出险次数"]<-0
##检查
current2[which(is.na(current2$出险次数)),"出险次数"]

##填充赔付金额
table(current2$赔付金额)
current2[which(is.na(current2$赔付金额)),"赔付金额"]<-0
##检查
current2[which(is.na(current2$赔付金额)),"赔付金额"]

##填充案均赔款
table(current2$案均赔款)
current2[which(is.na(current2$案均赔款)),"案均赔款"]<-0
##检查
current2[which(is.na(current2$案均赔款)),"案均赔款"]

##填充报案周期
table(current2$报案周期)
current2[which(current2$报案周期 == "NA"),"报案周期"]<-0
##检查
current2[which(is.na(current2$报案周期)),"报案周期"]

##填充理赔周期
table(current2$理赔周期)
current2[which(current2$理赔周期 == "NA"),"理赔周期"]<-0
##检查
current2[which(is.na(current2$理赔周期)),"理赔周期"]

##填充支付周期
table(current2$支付周期)
current2[which(current2$支付周期 == "NA"),"支付周期"]<-0
##检查
current2[which(is.na(current2$支付周期)),"支付周期"]

##填充是否注销拒赔
current2$是否注销拒赔<-NULL

##填充投保人性别
c<-c(names(table(current2$投保人性别[current$投保人性别 != "NA"])))
vec<-as.vector(prop.table(table(current2$投保人性别[current$投保人性别 != "NA"])))
len<-length(which(current2$投保人性别 == "NA"))
tian<-sample(c,len, prob = vec, replace = TRUE)
current2[which(current2$投保人性别   == "NA"),"投保人性别"] <-tian
#检查
table(current2$投保人性别)

##填充被保人性别
c<-c(names(table(current2$被保人性别[current$被保人性别 != "NA"])))
vec<-as.vector(prop.table(table(current2$被保人性别[current$被保人性别 != "NA"])))
len<-length(which(current2$被保人性别 == "NA"))
tian<-sample(c,len, prob = vec, replace = TRUE)
current2[which(current2$被保人性别   == "NA"),"被保人性别"] <-tian
#检查
table(current2$被保人性别)

##填充上年出险次数
table(current2$上年出险次数)
current2[which(is.na(current2$上年出险次数)),"上年出险次数"]<-0
##检查
current2[which(is.na(current2$上年出险次数)),"上年出险次数"]

##填充上年赔款金额
table(current2$上年赔款金额)
current2[which(is.na(current2$上年赔款金额)),"上年赔款金额"]<-0
##检查
current2[which(is.na(current2$上年赔款金额)),"上年赔款金额"]

##删除上年浮动原因码
current2$上年浮动原因码<-NULL
##删除客户忠诚度系数
current2$客户忠诚度系数<-NULL

##填充车损赔款
table(current2$车损赔款)
current2[which(is.na(current2$车损赔款)),"车损赔款"]<-0
##检查
current2[which(is.na(current2$车损赔款)),"车损赔款"]

