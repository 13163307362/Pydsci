library(plyr)
library(dplyr)
library(data.table)
library(Rmisc)
library(e1071)#求数据偏态
library(gsubfn)
library(proto)
library(RSQLite)
library(partykit)
library(grid)
library(smbinning)#分箱
library(Formula)#求数据偏态
library(psych)
library(ggplot2)
#------------------------读入数据------------------------
df<-read.csv('C:\\Users\\86103554\\Desktop\\实习\\custdet1.csv',stringsAsFactors = F)
#df$PURCHASE<-as.factor(df$PURCHASE)
head(df)
str(df)
summary(df)
table(df$PURCHASE)  #正负样本占比 999：967
table(df$NTITLE)
table(df$GENDER)
table(df$SEX)

#删除变量SEX，CUSTDATE，ACCTNUM
rm_var1<-c('SEX','CUSTDATE','ACCTNUM')

df1<-df[,!names(df) %in% rm_var1]
str(df1)
#------------------------查看数据质量------------------------
#缺失统计
v_name<-c();missing_cnt<-data.frame()
for (i in names(df1)){
  temp<-nrow(df[is.na(df1[,i]) | df1[,i]=='' ,])/nrow(df1)
  v_name<-c(v_name,i)
  missing_cnt<-rbind(missing_cnt,temp)
}
missing_cnt$variable<-v_name
names(missing_cnt)[1]<-'missing_cnt'
str(missing_cnt)
table(missing_cnt$missing_cnt)                #0 变量均无缺失

#唯一值统计
v_name<-c();unique_cnt<-data.frame()
for (i in names(df1)){
  temp<-n_distinct(df1[,i])
  v_name<-c(v_name,i)
  unique_cnt<-rbind(unique_cnt,temp)
}
unique_cnt$variable<-v_name
names(unique_cnt)[1]<-'unique_cnt'
str(unique_cnt)
min(unique_cnt$unique_cnt)                    #2 变量唯一值数均大于一


#------------------------查看数据分布------------------------
#查看数据偏态 skewness
temp_name<-c()
for(i in names(df1)){
  if(is.numeric(df1[,i])){
    temp_name<-c(temp_name,i)
  }
}
temp_name<-temp_name[-1]
skew<-data.frame();skew_temp<-c()
for(i in temp_name){
  skew_temp<-skewness(df1[,i])
  skew<-rbind(skew,skew_temp)
}
head(skew)
str(skew)
names(skew)[1]<-'skew'
skew$variable<-temp_name
#删除偏度绝对值大于10的变量
remove_var2<-skew[abs(skew$skew)>10,]$variable
df2<-df1[,!names(df1) %in% remove_var2]
str(df2)                                                      #45
#数据去偏度（log化,log化之前将变量加上一个极小值避免对0 log化）

v<-skew(df$DPM12+0.01)
skew(log(df$DPM12+0.0000001))
skew(log(df$AMOUNT+0.0000001))

#查看原始数值型变量分布
ggplot(df, aes(x = RETURN, fill = PURCHASE)) +
  geom_density(alpha = 0.3)

#查看类别最大占比
v_name<-c();type_max<-data.frame()
for (i in names(df2)){
  temp<-max(table(df2[,i])/length(df2[,i]))
  v_name<-c(v_name,i)
  type_max<-rbind(type_max,temp)
}
type_max$variable<-v_name
names(type_max)[1]<-'type_max'
max(type_max$type_max)
str(type_max)
#删除最大类别占比在0.9及以上的变量
remove_var3<-type_max[type_max$type_max>0.9,]$variable
df3<-df2[,!names(df2) %in% remove_var3]
str(df3)                                                    #42
df4<-df3

#------------------------分箱--------------------------
#smbinning数值型数据类别大于10可分箱，小于10应转化为因子
#数值型变量
numeric_var<-c()
for(i in names(df4)){
  if(is.numeric(df4[,i])){
    numeric_var<-c(numeric_var,i)
  }
}
numeric_var1<-numeric_var[-1]
#唯一值统计
v_name<-c();numeric_cnt<-data.frame()
for (i in numeric_var1){
  temp<-n_distinct(df4[,i])
  v_name<-c(v_name,i)
  numeric_cnt<-rbind(numeric_cnt,temp)
}
numeric_cnt$variable<-v_name
names(numeric_cnt)[1]<-'numeric_cnt'
str(numeric_cnt)
#数值型变量取值类别小于10，转化为因子

for (i in (numeric_cnt[numeric_cnt$numeric_cnt<10,'variable'])){
  df4[,i]<-as.factor(df4[,i])
}

#character型变量转化为因子变量
for(i in names(df4)){
  if(is.character(df4[,i])){
    df4[,i]<-as.factor(df4[,i])
  }
}



#df2$PURCHASE<-as.numeric(df2$PURCHASE)#smbinning要求目标变量为数值型，自变量为数值型或因子型
str(df)
#分箱，smbinning.factor要求至少有两个以上的因子，factor水平过多时smbinning.factor会报错，需剔除水平过多的因子
str(df4)
table(df4$RETURN)
rm_var4<-c('STATECOD')
df5<-df4[,!names(df4) %in% rm_var4]
str(df5)                                   #41个varbable

#分箱
bin_iv<-data.frame();bin_var<-c()
var_name<-names(df5)
for(i in var_name){
  if(is.numeric(df5[,i]) & i!='PURCHASE'){
    bin_tbl<-smbinning(df5,y='PURCHASE',x=i,p=0.1)
    if(list(bin_tbl) != 'No Bins' & bin_tbl != 'No significant splits'){
      bin_iv<-rbind(bin_iv,data.frame(bin_tbl$ivtable,variable=i))
      new_var<-paste('bin',i,sep='_')
      bin_var<-c(bin_var,new_var)
      df5<-smbinning.gen(df5,bin_tbl,new_var)
    }
  }
  if(is.factor(df5[,i]) & i !='PURCHASE'){
    bin_tbl<-smbinning.factor(df5,y='PURCHASE',x=i)
    bin_iv<-rbind(bin_iv,data.frame(bin_tbl$ivtable,variable=i))
    bin_var<-c(bin_var,i)
  }
}


?smbinning.factor

str(df5)                                                      #62
head(df5)
data_bin<-df5[,c('PURCHASE',bin_var)]       #选取bin以后的变量 30个(31)
str(data_bin)                                            
str(bin_iv)
head(bin_iv)
unique(bin_iv$variable)
#woe
library(MASS)
library(klaR)
for (i in names(data_bin)){
  ifelse(!is.factor(data_bin[,i]),data_bin[,i]<-as.factor(data_bin[,i]),data_bin[,i]<-data_bin[,i])
}#woe需要因子型变量

#利用iv值筛选变量
woe_model<-woe(PURCHASE~.,data = data_bin,zeroadj = 0.5,appont = T)
names(woe_model)
plot(woe_model)
str(woe_model)
iv_table<-woe_model$IV[order(woe_model$IV,decreasing = T)]
iv_var<-names(iv_table[iv_table>=0.01]) #24个
length(iv_table[iv_table>=0.01])
rm_var5<-as.vector(bin_iv[which(bin_iv$CntGood > bin_iv$CntBad),]$variable)
rm_var5<-unique(rm_var5)  #30个
iv_var<- iv_var[!iv_var %in% rm_var5]  #21个
length(iv_var)


#利用lasso筛选变量
woe_model<-woe(PURCHASE~.,data = data_bin[,c('PURCHASE',iv_var)],zeroadj = 0.5,appont = T)
data6<-predict(woe_model,newdata = data_bin[,c('PURCHASE',iv_var)],replace = T)
str(data6)
library(glmnet)
x<-as.matrix(data6[,-1])
y<-data6$PURCHASE
lasso1<-cv.glmnet(x,y,family='binomial',type='auc')
lasso1$lambda.min
lasso1$lambda.lse
coefficients1<-coef(lasso1,s=lasso1$lambda.min)
active.index1<-which(coefficients1!=0)
active.coefficients1<-coefficients1[active.index1]
data7<-data6[,active.index1]
str(data7)
var<-sub('woe_','',names(data7))
data7<-data_bin[,var]
woe_model<-woe(PURCHASE~.,data = data7,zeroadj = 0.5,appont = T)
data7<-predict(woe_model,newdata = data7,replace = T)
str(data7)

#检查共线性
corelation<-cor(data7[,!names(data7)%in% 'PURCHASE'])
kappa(corelation,exact = T)

#划分训练集、测试集(使用data_bin)
str(data_bin)
set.seed(1234)

train<-sample(nrow(data_bin),0.7*nrow(data_bin),replace = F)
woe_model<-woe(PURCHASE~.,data = data_bin[train,],zeroadj = 0.5,appont = T)
traindata<-predict(woe_model,newdata = data_bin[train,],replace = T)

#选择最优子集
str(traindata)
library(leaps)
regfit<-regsubsets(PURCHASE~.,data=traindata,nvmax=22, method = 'seqrep')
reg_summary<-summary(regfit)
print(reg_summary$bic)
which.min(reg_summary$bic)
reg_summary$adjr2
which.max(reg_summary$adjr2)
select_var<-names(which(reg_summary$which[11,]==TRUE))[-1]

#逻辑回归
glmodel<-glm(PURCHASE~.,traindata[,c('PURCHASE',select_var)],family = binomial)
summary(glmodel)


temp0<-predict(glmodel,traindata[,c('PURCHASE',select_var)],type='response')
summary(temp)
library(ROCR)
t0<-prediction(temp0,traindata[,c('PURCHASE',select_var)]$PURCHASE)
t01<-performance(t0,'tpr','fpr')
t02<-performance(t0,'auc')
t02@y.values#计算auc值
max(attr(t01,'y.values')[[1]]-attr(t01,'x.values')[[1]])
plot(t01,main='ROC(train data)',sub='AUC=0.6718486',col='darkblue')
abline(a=0,b=1,col='red',xlim=c(0,1),ylim=c(0,1))









