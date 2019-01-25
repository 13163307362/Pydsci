setwd("D:\\东证期货杯\\选题2竞赛数据-20171114(1)\\竞赛数据-20171114")

library(plyr)

library(dplyr)

library(data.table)

library(ggplot2)

library(Rmisc)



contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")

contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")

contest_basic<-rbind(contest_basic_train[,-11],contest_basic_test)

nrow(contest_basic)

apply(contest_basic,2,function(x) sum(x==""))



#Agent合为APP、wechat、rongzhijia、ELSE、missing

contest_basic$AGENT[contest_basic$AGENT==""]<-"missing"

contest_basic$AGENT[contest_basic$AGENT %in% c("其他","应用商店","搜索引擎","收到邮件或短信","新闻资讯平台", "朋友推荐","社交软件平台","第三方贷款平台","bestpay","chinapnr","DDH_SYNC","fenqile","huifusdb","ifpp","kuaiqian","orgloan","weijinhui")]<-"ELSE"

#提取身份证户籍和性别信息、补齐部分WORK_PROVINCE

contest_basic1<-mutate(contest_basic,huji=substr(contest_basic$ID_CARD,1,6),sex=substr(contest_basic$ID_CARD,17,17))

contest_basic1$sex[contest_basic1$sex %in% c("1","3","5","7","9")]<-"1"

contest_basic1$sex[contest_basic1$sex %in% c("0","2","4","6","8")]<-"0"

#根据本地籍补齐部分WORK_PROVINCE缺失值，无法根据WORK_PROVINCE补齐是否本地籍，因为本地籍缺失WORK_PROVINCE必缺失

contest_basic1[contest_basic1$IS_LOCAL=="本地籍" & is.na(contest_basic1$WORK_PROVINCE),]$WORK_PROVINCE<-contest_basic1[contest_basic1$IS_LOCAL=="本地籍" & is.na(contest_basic1$WORK_PROVINCE),]$huji

#work_province合为江苏、河北、山西、辽宁、黑龙江、吉林、其他、缺失

#contest_basic1$WORK_PROVINCE[is.na(contest_basic1$WORK_PROVINCE)]<-"missing"

contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="32"]<-"32"

contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="13"]<-"13"

contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="14"]<-"14"

contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="21"]<-"21"

contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="23"]<-"23"

contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="22"]<-"22"

quhao<-c("11","12","15","31","33","34","35","36","37","41","42","43","44","45","46","50","51","52","53","54","61","62","63","64","65")

contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2) %in% quhao]<-"00"

table(contest_basic1$WORK_PROVINCE)

#EDU_LEVEL合并为硕士及以上（包含博士研究生、硕士及以上、硕士研究生），本科，专科，专科以下（专科及以下、初中、高中），缺失（包含缺失和其他）

contest_basic1$EDU_LEVEL[contest_basic1$EDU_LEVEL %in% c("博士研究生","硕士及以上","硕士研究生")]<-"1"

contest_basic1$EDU_LEVEL[contest_basic1$EDU_LEVEL %in% c("本科")]<-"2"

contest_basic1$EDU_LEVEL[contest_basic1$EDU_LEVEL %in% c("专科")]<-"3"

contest_basic1$EDU_LEVEL[contest_basic1$EDU_LEVEL %in% c("专科及以下","初中","高中")]<-"4"

contest_basic1$EDU_LEVEL[contest_basic1$EDU_LEVEL %in% c("其他","")]<-"5"

#SALARY缺失率为0.66665，取值为1、2、3、4、5、6、7，将缺失值编码为-1表示缺失

contest_basic1$SALARY[is.na(contest_basic1$SALARY)]<- -1

##用mice多重插补

#重新编码婚姻状况

contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS=="离婚"]<-"1"

contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS=="离异"]<-"2"

contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS=="其他"]<-"3"

contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS=="丧偶"]<-"4"

contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS=="未婚"]<-"5"

contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS=="已婚"]<-"6"

contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS==""]<-NA

#重新编码是否本地籍

contest_basic1$IS_LOCAL[contest_basic1$IS_LOCAL=="本地籍"]<-"1"

contest_basic1$IS_LOCAL[contest_basic1$IS_LOCAL=="非本地籍"]<-"0"

contest_basic1$IS_LOCAL[contest_basic1$IS_LOCAL==""]<-NA

contest_basic1$AGENT<-as.factor(contest_basic1$AGENT)

contest_basic1$IS_LOCAL<-as.factor(contest_basic1$IS_LOCAL)

contest_basic1$WORK_PROVINCE<-as.factor(contest_basic1$WORK_PROVINCE)

contest_basic1$EDU_LEVEL<-as.factor(contest_basic1$EDU_LEVEL)

contest_basic1$MARRY_STATUS<-as.factor(contest_basic1$MARRY_STATUS)

contest_basic1$SALARY<-as.factor(contest_basic1$SALARY)

contest_basic1$HAS_FUND<-as.factor(contest_basic1$HAS_FUND)

contest_basic1$sex<-as.factor(contest_basic1$sex)

apply(contest_basic1,2,function(x) sum(is.na(x)))

library(mice)

miceMod<-mice(contest_basic1[,!names(contest_basic1) %in% c("REPORT_ID","ID_CARD","LOAN_DATE","huji")], method="rf")#基于随机森林模型进行多重插补

miceOutput<-complete(miceMod)#生成完整数据

sum(is.na(miceOutput))

head(miceOutput)

basic<-cbind(contest_basic[,1],miceOutput)

head(basic)#REPORT_ID ,int

str(basic)

#########################################contest_ext_crd_hd_report表################################################################

contest_ext_crd_hd_report<-fread("contest_ext_crd_hd_report.csv",encoding="UTF-8")





#重新编码QUERY_REASON中的贷后管理、贷款审批、担保资格审查为a b c 

table(contest_ext_crd_hd_report$QUERY_REASON)

contest_ext_crd_hd_report$QUERY_REASON[contest_ext_crd_hd_report$QUERY_REASON=="贷后管理"]<-"a"

contest_ext_crd_hd_report$QUERY_REASON[contest_ext_crd_hd_report$QUERY_REASON=="贷款审批"]<-"b"

contest_ext_crd_hd_report$QUERY_REASON[contest_ext_crd_hd_report$QUERY_REASON=="担保资格审查"]<-"c"

contest_ext_crd_hd_report$QUERY_REASON<-as.factor(contest_ext_crd_hd_report$QUERY_REASON)

contest_ext_crd_hd_report$QUERY_ORG<-as.factor(contest_ext_crd_hd_report$QUERY_ORG)

head(contest_ext_crd_hd_report)

nrow(contest_ext_crd_hd_report)

##与基础表合并

union1<-left_join(basic,contest_ext_crd_hd_report[,-2])

nrow(union1)



###分为测试集和训练集###

contest_ext_crd_hd_report_train<-left_join(contest_basic_train[,c(1,11)],contest_ext_crd_hd_report)

nrow(contest_ext_crd_hd_report_train)

contest_ext_crd_hd_report_train$QUERY_REASON<-as.factor(contest_ext_crd_hd_report_train$QUERY_REASON)

contest_ext_crd_hd_report_train$QUERY_ORG<-as.factor(contest_ext_crd_hd_report_train$QUERY_ORG)

contest_ext_crd_hd_report_test<-left_join(contest_basic_test[,1],contest_ext_crd_hd_report)

nrow(contest_ext_crd_hd_report_test)

##描述统计图



bar_QUERY_REASON<- ggplot(contest_ext_crd_hd_report_train, aes(x=as.factor(Y),fill = QUERY_REASON)) +
  
  geom_bar(position = 'fill') + 
  
  theme_bw() + 
  
  labs(x = 'Y', y = 'QUERY_REASON')

bar_QUERY_REASON

bar_QUERY_ORG<- ggplot(contest_ext_crd_hd_report_train, aes(x=as.factor(Y),fill = QUERY_ORG)) +
  
  geom_bar(position = 'fill') + 
  
  theme_bw() + 
  
  labs(x = 'Y', y = 'QUERY_ORG')

bar_QUERY_ORG

multiplot(bar_QUERY_ORG, bar_QUERY_REASON ,cols = 2)



#########################################contest_ext_crd_cd_ln表################################################################

contest_ext_crd_cd_ln<-fread("contest_ext_crd_cd_ln.tsv",encoding="UTF-8")

# table(contest_ext_crd_cd_ln[is.na(contest_ext_crd_cd_ln$payment_cyc),]$payment_rating)

# ##测试集

# names(contest_ext_crd_cd_ln)[2]<-"REPORT_ID"

# contest_basic_train<-inner_join(contest_basic[,c(1,11)],contest_ext_crd_cd_ln)

# #还款频率的合并

# (table(contest_basic_train[contest_basic_train$Y==1,]$payment_rating))/(table(contest_basic_train$payment_rating))

# table(contest_basic_train$payment_rating)

# #贷款种类细分的合并

# table(contest_basic_train[contest_basic_train$Y==1,]$guarantee_type)/(table(contest_basic_train$guarantee_type))

# table(contest_basic_train$guarantee_type)



head(contest_ext_crd_cd_ln)

str(contest_ext_crd_cd_ln)

contest_ext_crd_cd_ln<-fread("contest_ext_crd_cd_ln.tsv",encoding="UTF-8")[,-6]

contest_ext_crd_cd_ln$payment_cyc[contest_ext_crd_cd_ln$payment_cyc=="NULL"]<-NA

contest_ext_crd_cd_ln$class5_state[contest_ext_crd_cd_ln$class5_state=="NULL"]<-NA

contest_ext_crd_cd_ln$payment_state[contest_ext_crd_cd_ln$payment_state=="NULL"]<-NA

contest_ext_crd_cd_ln$balance[contest_ext_crd_cd_ln$balance=="NULL"]<-NA

contest_ext_crd_cd_ln$remain_payment_cyc[contest_ext_crd_cd_ln$remain_payment_cyc=="NULL"]<-NA

contest_ext_crd_cd_ln$scheduled_payment_amount[contest_ext_crd_cd_ln$scheduled_payment_amount=="NULL"]<-NA

contest_ext_crd_cd_ln$actual_payment_amount[contest_ext_crd_cd_ln$actual_payment_amount=="NULL"]<-NA

contest_ext_crd_cd_ln$curr_overdue_cyc[contest_ext_crd_cd_ln$curr_overdue_cyc=="NULL"]<-NA

contest_ext_crd_cd_ln$curr_overdue_amount[contest_ext_crd_cd_ln$curr_overdue_amount=="NULL"]<-NA

contest_ext_crd_cd_ln$recent_pay_date[contest_ext_crd_cd_ln$recent_pay_date=="NULL"]<-NA

contest_ext_crd_cd_ln$scheduled_payment_date[contest_ext_crd_cd_ln$scheduled_payment_date=="NULL"]<-NA

contest_ext_crd_cd_ln$end_date[contest_ext_crd_cd_ln$end_date=="NULL"]<-NA

table(contest_ext_crd_cd_ln$state)

head(contest_ext_crd_cd_ln)

str(contest_ext_crd_cd_ln)

apply(contest_ext_crd_cd_ln,2,function(x) sum(is.na(x)))

table(contest_ext_crd_cd_ln[is.na(contest_ext_crd_cd_ln$remain_payment_cyc),]$state)

table(contest_ext_crd_cd_ln[is.na(contest_ext_crd_cd_ln$payment_cyc)&!is.na(contest_ext_crd_cd_ln$balance)&contest_ext_crd_cd_ln$state=="逾期",]$payment_rating)

table(contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$payment_rating=="一次性归还"&is.na(contest_ext_crd_cd_ln$remain_payment_cyc),])

contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$payment_rating=="一次性归还"&is.na(contest_ext_crd_cd_ln$remain_payment_cyc),]

table(contest_ext_crd_cd_ln[!is.na(contest_ext_crd_cd_ln$payment_cyc),]$payment_rating)

contest_ext_crd_cd_ln[is.na(contest_ext_crd_cd_ln$payment_cyc)&!is.na(contest_ext_crd_cd_ln$balance)&contest_ext_crd_cd_ln$state=="逾期",]$payment_rating



###分为测试集和训练集###

contest_basic_train1<-contest_basic_train

names(contest_basic_train1)[1]<-"report_id"

str(contest_basic_train1)

contest_ext_crd_cd_ln_train<-left_join(contest_basic_train1[,c(1,11)],contest_ext_crd_cd_ln)

nrow(contest_ext_crd_cd_ln_train)

contest_ext_crd_cd_ln_test<-left_join(contest_basic_train1[,c(1,11)],contest_ext_crd_cd_ln)

nrow(contest_ext_crd_cd_ln_test)





############特征第一波,计算成对缺失数值型字段的min\max\mean,还有五级分类各类对应的贷款笔数，24月还款状态信息作为特征

#loan_id、 report_id、 balance、 scheduled_payment_amount、 actual_payment_amount、 curr_overdue_cyc、 curr_overdue_amount、payment_state、class5_state

tezheng1<-contest_ext_crd_cd_ln[,c(2,12,14,15,16,17,10,9)]

apply(tezheng1,2,function(x) sum(is.na(x)))

tezheng1_1<-na.omit(tezheng1)

tezheng1_1$report_id<-as.character(tezheng1_1$report_id)

tezheng1_1$balance<-as.numeric(tezheng1_1$balance)

tezheng1_1$scheduled_payment_amount<-as.numeric(tezheng1_1$scheduled_payment_amount)

tezheng1_1$actual_payment_amount<-as.numeric(tezheng1_1$actual_payment_amount)

tezheng1_1$curr_overdue_cyc<-as.numeric(tezheng1_1$curr_overdue_cyc)

tezheng1_1$curr_overdue_amount<-as.numeric(tezheng1_1$curr_overdue_amount)

##求均值

tezheng1_1_mean<-tezheng1_1[, .(balance.mean= mean(balance),scheduled_payment_amount.mean=mean(scheduled_payment_amount),actual_payment_amount.mean=mean(actual_payment_amount),curr_overdue_cyc.mean=mean(curr_overdue_cyc),curr_overdue_amount.mean=mean(curr_overdue_amount)),by= .(report_id)]

##求max值

tezheng1_1_max<-tezheng1_1[, .(balance.max= max(balance),scheduled_payment_amount.max=max(scheduled_payment_amount),actual_payment_amount.max=max(actual_payment_amount),curr_overdue_cyc.max=max(curr_overdue_cyc),curr_overdue_amount.max=max(curr_overdue_amount)),by= .(report_id)]

##求min值

tezheng1_1_min<-tezheng1_1[, .(balance.min= min(balance),scheduled_payment_amount.min=min(scheduled_payment_amount),actual_payment_amount.min=min(actual_payment_amount),curr_overdue_cyc.min=min(curr_overdue_cyc),curr_overdue_amount.min=min(curr_overdue_amount)),by= .(report_id)]

##class5_state,五级分类各类对应的贷款笔数

tezheng1_cs_n<- tezheng1_1[, .(.N), by = .(report_id,class5_state)]

tezheng1_cs_n1<-dcast(tezheng1_cs_n, report_id ~ class5_state, value.var = 'N')

tezheng1_cs_n1[is.na(tezheng1_cs_n1)]<-0

names(tezheng1_cs_n1)[c(2,3,4,5,6)]<-c("guanzhu1_n","keyi1_n","weizhi1_n","ciji1_n","zhengchang1_n")

##payment_state,24月还款状态

library(stringr)

#24月未开户时长，开户时长用24去减，缺失的地方未开户时长为0

a1<-str_count(tezheng1_1$payment_state,"/")

a1<-24-a1

length(a1)

#N的个数

a2<-str_count(tezheng1_1$payment_state,"N")

#*号的个数

a3<-str_count(tezheng1_1$payment_state,"\\*")

#各数字个数

a4_1<-str_count(tezheng1_1$payment_state,"1")

a4_2<-str_count(tezheng1_1$payment_state,"2")

a4_3<-str_count(tezheng1_1$payment_state,"3")

a4_4<-str_count(tezheng1_1$payment_state,"4")

a4_5<-str_count(tezheng1_1$payment_state,"5")

a4_6<-str_count(tezheng1_1$payment_state,"6")

a4_7<-str_count(tezheng1_1$payment_state,"7")

a4_C<-str_count(tezheng1_1$payment_state,"C")

payment_state1<-data.table(report_id=tezheng1_1$report_id,a1=a1,a4_1=a4_1,a4_2=a4_2,a4_3=a4_3,a4_4=a4_4,a4_5=a4_5,a4_6=a4_6,a4_7=a4_7,a4_C=a4_C)

str(payment_state1)

#计算均值、min、max

payment_state1$report_id<-as.character(payment_state1$report_id)

payment_state1_mean<-payment_state1[, .(a1.mean= mean(a1),a4_1.mean= max(a4_1),a4_2.mean= mean(a4_2),a4_3.mean= mean(a4_3),a4_4.mean= mean(a4_4),a4_5.mean= mean(a4_5),a4_6.mean=mean(a4_6),a4_7.mean=mean(a4_7),a4_C.mean=mean(a4_C)),by= .(report_id)]

payment_state1_max<-payment_state1[, .(a1.max= max(a1),a4_1.max=max(a4_1),a4_2.max=max(a4_2),a4_3.max=max(a4_3),a4_4.max=max(a4_4),a4_5.max=max(a4_5),a4_6.max=max(a4_6),a4_7.max=max(a4_7),a4_C.max=max(a4_C)),by= .(report_id)]

payment_state1_min<-payment_state1[, .(a1.min= min(a1),a4_1.min=min(a4_1),a4_2.min=min(a4_2),a4_3.min=min(a4_3),a4_4.min=min(a4_4),a4_5.min=min(a4_5),a4_6.min=min(a4_6),a4_7.min=min(a4_7),a4_C.min=min(a4_C)),by= .(report_id)]



#合并特征

tezheng1_union<-cbind(tezheng1_1_mean,tezheng1_1_max[,-1],tezheng1_cs_n1[,-1],payment_state1_mean[,-1],payment_state1_max[,-1])

head(tezheng1_union)

nrow(tezheng1_union)

############特征第二波state、finance_org、type_dw、guarantee_type、payment_rating、credit_limit_amount

##计算每个report_id的loan_id数

report_loan_id_n<- contest_ext_crd_cd_ln[, .(.N), by = .(report_id)]

report_loan_id_n1<- report_loan_id_n[, .(report_id=report_id,loan_id_n=N)]

head(report_loan_id_n1)

##每个report_id的各账户状态数

report_state_n<- contest_ext_crd_cd_ln[, .(.N), by = .(report_id,state)]

#将report_state_n变为宽表

report_state_n<-dcast(report_state_n, report_id ~ state, value.var = 'N')

report_state_n[is.na(report_state_n)]<-0

head(report_state_n)

names(report_state_n)[c(2,3,4,5,6)]<-c("daizhang_n","zhengchang_n","jieqing_n","zhuanchu_n","yvqi_n")

##每个report_id贷款机构数

#将436个贷款机构为NA的标识为Na，119个--标识为其本身

contest_ext_crd_cd_ln$finance_org[is.na(contest_ext_crd_cd_ln$finance_org)]<-"Na"

table(contest_ext_crd_cd_ln$finance_org)

#计算id机构数

contest_ext_crd_cd_ln_gg<-contest_ext_crd_cd_ln[,c(2,4)]

contest_ext_crd_cd_ln_gg_n<- contest_ext_crd_cd_ln_gg[, .(.N), by = .(report_id,finance_org)]

head(contest_ext_crd_cd_ln_gg_n1)

contest_ext_crd_cd_ln_gg_n1<- contest_ext_crd_cd_ln_gg_n[,-3][, .(.N), by = .(report_id)]

names(contest_ext_crd_cd_ln_gg_n1)[2]<-"daikuanjigou_n"

head(contest_ext_crd_cd_ln_gg_n1)



##每个report_id每种贷款的贷款总额、各种贷款笔数

#计算每个report_id每种type_dw的合同总额

contest_ext_crd_cd_ln_dw<-contest_ext_crd_cd_ln[,c(2,5,11)]

report_wd_amount<- contest_ext_crd_cd_ln_dw[, .(sum(credit_limit_amount)), by = .(report_id,type_dw)]

#将report_wd_amount根据type_dw和credit_limit_amount总额v1变为宽表，即得每个report_id每种贷款的贷款总额

report_wd_amount1<-dcast(report_wd_amount, report_id ~ type_dw, value.var = 'V1')

report_wd_amount1[is.na(report_wd_amount1)]<-0

nrow(report_wd_amount1)

#各种贷款笔数

contest_ext_crd_cd_ln_dw_n<-contest_ext_crd_cd_ln[,c(2,5)]

report_wd_amount_n<- contest_ext_crd_cd_ln_dw_n[, .(.N), by = .(report_id,type_dw)]

report_wd_amount_n1<-dcast(report_wd_amount_n, report_id ~ type_dw, value.var = 'N')

report_wd_amount_n1[is.na(report_wd_amount_n1)]<-0

head(report_wd_amount_n1)

names(report_wd_amount_n1)[c(2:10)]<-c("daikuan_n1","daikuan_n2","daikuan_n3","daikuan_n4","daikuan_n5","daikuan_n6","daikuan_n7","daikuan_n8","daikuan_n9")

###若要合并，则担保方式分为保证担保贷款、抵押担保贷款、质押担保贷款 、其他（包含农户联保、其他担保、信用/免担保、组合（不含保证）担保、组合（含保证）担保）

##各担保方式下的贷款笔数

contest_ext_crd_cd_ln_gt<-contest_ext_crd_cd_ln[,c(2,6)]

contest_ext_crd_cd_ln_gt_n<- contest_ext_crd_cd_ln_gt[, .(.N), by = .(report_id,guarantee_type)]

contest_ext_crd_cd_ln_gt_n1<-dcast(contest_ext_crd_cd_ln_gt_n, report_id ~ guarantee_type, value.var = 'N')

contest_ext_crd_cd_ln_gt_n1[is.na(contest_ext_crd_cd_ln_gt_n1)]<-0

head(contest_ext_crd_cd_ln_gt_n1)

names(contest_ext_crd_cd_ln_gt_n1)[c(2:9)]<-c("danbao_n1","danbao_n2","danbao_n3","danbao_n4","danbao_n5","danbao_n6","danbao_n7","danbao_n8")



##各担保方式贷款总额

contest_ext_crd_cd_ln_gt_amount<-contest_ext_crd_cd_ln[,c(2,6,11)]

report_gt_amount<- contest_ext_crd_cd_ln_gt_amount[, .(sum(credit_limit_amount)), by = .(report_id,guarantee_type)]

#将report_gt_amount根据guarantee_type和credit_limit_amount总额v1变为宽表，即得各担保方式贷款总额

report_gt_amount1<-dcast(report_gt_amount, report_id ~ guarantee_type, value.var = 'V1')

report_gt_amount1[is.na(report_gt_amount1)]<-0

head(report_gt_amount1)

names(report_gt_amount1)[c(2:9)]<-c("danbao_m1","danbao_m2","danbao_m3","danbao_m4","danbao_m5","danbao_m6","danbao_m7","danbao_m8")



###若要合并，则还款频率分为（按半年归还、按季归还、按年归还）、按其他方式归还、（按日归还、按月归还、按周归还）、不定期归还、一次性归还

##各还款频率下贷款笔数

contest_ext_crd_cd_ln_pr<-contest_ext_crd_cd_ln[,c(2,7)]

contest_ext_crd_cd_ln_pr_n<- contest_ext_crd_cd_ln_pr[, .(.N), by = .(report_id,payment_rating)]

contest_ext_crd_cd_ln_pr_n1<-dcast(contest_ext_crd_cd_ln_pr_n, report_id ~ payment_rating, value.var = 'N')

contest_ext_crd_cd_ln_pr_n1[is.na(contest_ext_crd_cd_ln_pr_n1)]<-0

names(contest_ext_crd_cd_ln_pr_n1)[c(2:10)]<-c("pinlv_n1","pinlv_n2","pinlv_n3","pinlv_n4","pinlv_n5","pinlv_n6","pinlv_n7","pinlv_n8","pinlv_n9")

ncol(contest_ext_crd_cd_ln_pr_n1)

##各还款频率下贷款总额,（没有用）

contest_ext_crd_cd_ln_pr_amount<-contest_ext_crd_cd_ln[,c(2,7,11)]

report_pr_amount<- contest_ext_crd_cd_ln_pr_amount[, .(sum(credit_limit_amount)), by = .(report_id,payment_rating)]

#将report_pr_amount根据payment_rating和credit_limit_amount总额v1变为宽表，即得各还款频率下贷款总额

report_pr_amount1<-dcast(report_pr_amount, report_id ~ payment_rating, value.var = 'V1')

report_gt_amount1[is.na(report_gt_amount1)]<-0

head(report_gt_amount1)

#合并特征

tezheng2_union<-cbind(report_loan_id_n1,report_state_n[,-1],contest_ext_crd_cd_ln_gg_n1[,-1],report_wd_amount_n1[,-1],contest_ext_crd_cd_ln_gt_n1[,-1],report_gt_amount1[,-1],contest_ext_crd_cd_ln_pr_n1[,-1])

str(tezheng2_union)

str(tezheng1_union)

nrow(tezheng2_union)

tezheng2_union$report_id<-as.character(tezheng2_union$report_id)

###贷款表特征合并

daikuanbiao<-full_join(tezheng1_union,tezheng2_union)

daikuanbiao[is.na(daikuanbiao)]<-0

apply(daikuanbiao,2,function(x) sum(is.na(x)))

str(daikuanbiao)

####与上面特征合并

names(daikuanbiao)[1]<-"REPORT_ID"

union1$REPORT_ID<-as.character(union1$REPORT_ID)

union2<-left_join(union1,daikuanbiao)

apply(union2,2,function(x) sum(is.na(x)))

#合并部分缺失理解为用户没有贷款，故合并产生的缺失用0补齐

union2[is.na(union2)]<-0

#########################################contest_ext_crd_cd_lnd表################################################################

contest_ext_crd_cd_lnd<-fread("contest_ext_crd_cd_lnd.tsv",encoding="UTF-8")

table(contest_ext_crd_cd_lnd$guarantee_type)

head(contest_ext_crd_cd_lnd)



apply(contest_ext_crd_cd_lnd,2,function(x) sum(is.na(x)))

apply(contest_ext_crd_cd_lnd,2,function(x) sum(x=="NULL"))

##将空值都标识为NA

contest_ext_crd_cd_lnd$used_credit_limit_amount[contest_ext_crd_cd_lnd$used_credit_limit_amount=="NULL"]<-NA

contest_ext_crd_cd_lnd$latest6_month_used_avg_amount[contest_ext_crd_cd_lnd$latest6_month_used_avg_amount=="NULL"]<-NA

contest_ext_crd_cd_lnd$used_highest_amount[contest_ext_crd_cd_lnd$used_highest_amount=="NULL"]<-NA

contest_ext_crd_cd_lnd$scheduled_payment_date[contest_ext_crd_cd_lnd$scheduled_payment_date=="NULL"]<-NA

contest_ext_crd_cd_lnd$scheduled_payment_amount[contest_ext_crd_cd_lnd$scheduled_payment_amount=="NULL"]<-NA

contest_ext_crd_cd_lnd$actual_payment_amount[contest_ext_crd_cd_lnd$actual_payment_amount=="NULL"]<-NA

contest_ext_crd_cd_lnd$recent_pay_date[contest_ext_crd_cd_lnd$recent_pay_date=="NULL"]<-NA

contest_ext_crd_cd_lnd$curr_overdue_cyc[contest_ext_crd_cd_lnd$curr_overdue_cyc=="NULL"]<-NA

contest_ext_crd_cd_lnd$curr_overdue_amount[contest_ext_crd_cd_lnd$curr_overdue_amount=="NULL"]<-NA

contest_ext_crd_cd_lnd$payment_state[contest_ext_crd_cd_lnd$payment_state=="NULL"]<-NA

#重新编码卡类型（该特征后期再补）

contest_ext_crd_cd_lnd$payment_state[contest_ext_crd_cd_lnd$cardtype=="贷记卡"]<-1

contest_ext_crd_cd_lnd$payment_state[contest_ext_crd_cd_lnd$cardtype=="NULL"]<-0



############lnd 特征第一波,计算成对缺失数值型字段的min\max\mean,还有五级分类各类对应的贷款笔数，24月还款状态信息作为特征

#report_id、以及其他成对缺失的指标

lnd_tezheng1<-contest_ext_crd_cd_lnd[,c(1,10,11,12,14,15,17,18,19)]

str(lnd_tezheng1)

apply(lnd_tezheng1,2,function(x) sum(is.na(x)))

lnd_tezheng1_1<-na.omit(lnd_tezheng1)

lnd_tezheng1_1$report_id<-as.character(lnd_tezheng1_1$report_id)

lnd_tezheng1_1$used_credit_limit_amount<-as.numeric(lnd_tezheng1_1$used_credit_limit_amount)

lnd_tezheng1_1$latest6_month_used_avg_amount<-as.numeric(lnd_tezheng1_1$latest6_month_used_avg_amount)

lnd_tezheng1_1$used_highest_amount<-as.numeric(lnd_tezheng1_1$used_highest_amount)

lnd_tezheng1_1$scheduled_payment_amount<-as.numeric(lnd_tezheng1_1$scheduled_payment_amount)

lnd_tezheng1_1$actual_payment_amount<-as.numeric(lnd_tezheng1_1$actual_payment_amount)

lnd_tezheng1_1$curr_overdue_cyc<-as.numeric(lnd_tezheng1_1$curr_overdue_cyc)

lnd_tezheng1_1$curr_overdue_amount<-as.numeric(lnd_tezheng1_1$curr_overdue_amount)



##求均值

lnd_tezheng1_1_mean<-lnd_tezheng1_1[, .(lnd_used_credit_limit_amount.mean= mean(used_credit_limit_amount),lnd_scheduled_payment_amount.mean=mean(scheduled_payment_amount),lnd_actual_payment_amount.mean=mean(actual_payment_amount),lnd_curr_overdue_cyc.mean=mean(curr_overdue_cyc),lnd_curr_overdue_amount.mean=mean(curr_overdue_amount),lnd_latest6_month_used_avg_amount.mean=mean(latest6_month_used_avg_amount),lnd_used_highest_amount.mean=mean(used_highest_amount)),by= .(report_id)]

##求max值

lnd_tezheng1_1_max<-lnd_tezheng1_1[, .(lnd_used_credit_limit_amount.max= max(used_credit_limit_amount),lnd_scheduled_payment_amount.max=max(scheduled_payment_amount),lnd_actual_payment_amount.max=max(actual_payment_amount),lnd_curr_overdue_cyc.max=max(curr_overdue_cyc),lnd_curr_overdue_amount.max=max(curr_overdue_amount),lnd_latest6_month_used_avg_amount.max=max(latest6_month_used_avg_amount),lnd_used_highest_amount.max=max(used_highest_amount)),by= .(report_id)]

##求min值

lnd_tezheng1_1_min<-lnd_tezheng1_1[, .(lnd_used_credit_limit_amount.min= min(used_credit_limit_amount),lnd_scheduled_payment_amount.min=min(scheduled_payment_amount),lnd_actual_payment_amount.min=min(actual_payment_amount),lnd_curr_overdue_cyc.min=min(curr_overdue_cyc),lnd_curr_overdue_amount.min=min(curr_overdue_amount),lnd_latest6_month_used_avg_amount.min=min(latest6_month_used_avg_amount),lnd_used_highest_amount.min=min(used_highest_amount)),by= .(report_id)]

##payment_state,24月还款状态

#library(stringr)

#24月未开户时长，开户时长用24去减，缺失的地方未开户时长为0

lnd_a1<-str_count(lnd_tezheng1_1$payment_state,"/")

lnd_a1<-24-lnd_a1

length(lnd_a1)

#N的个数

lnd_a2<-str_count(lnd_tezheng1_1$payment_state,"N")

#*号的个数

lnd_a3<-str_count(lnd_tezheng1_1$payment_state,"\\*")

#各数字个数

lnd_a4_1<-str_count(lnd_tezheng1_1$payment_state,"1")

lnd_a4_2<-str_count(lnd_tezheng1_1$payment_state,"2")

lnd_a4_3<-str_count(lnd_tezheng1_1$payment_state,"3")

lnd_a4_4<-str_count(lnd_tezheng1_1$payment_state,"4")

lnd_a4_5<-str_count(lnd_tezheng1_1$payment_state,"5")

lnd_a4_6<-str_count(lnd_tezheng1_1$payment_state,"6")

lnd_a4_7<-str_count(lnd_tezheng1_1$payment_state,"7")

lnd_a4_C<-str_count(lnd_tezheng1_1$payment_state,"C")

lnd_a4_D<-str_count(lnd_tezheng1_1$payment_state,"#")

length(lnd_a4_D)

lnd_payment_state1<-data.table(report_id=lnd_tezheng1_1$report_id,lnd_a1=lnd_a1,lnd_a4_1=lnd_a4_1,lnd_a4_2=lnd_a4_2,lnd_a4_3=lnd_a4_3,lnd_a4_4=lnd_a4_4,lnd_a4_5=lnd_a4_5,lnd_a4_6=lnd_a4_6,lnd_a4_7=lnd_a4_7,lnd_a4_C=lnd_a4_C,lnd_a4_D=lnd_a4_D)

str(lnd_payment_state1)

#计算均值、min、max

lnd_payment_state1_mean<-lnd_payment_state1[, .(lnd_a1.mean= mean(lnd_a1),lnd_a4_1.mean= max(lnd_a4_1),lnd_a4_2.mean= mean(lnd_a4_2),lnd_a4_3.mean= mean(lnd_a4_3),lnd_a4_4.mean= mean(lnd_a4_4),lnd_a4_5.mean= mean(lnd_a4_5),lnd_a4_6.mean=mean(lnd_a4_6),lnd_a4_7.mean=mean(lnd_a4_7),lnd_a4_C.mean=mean(lnd_a4_C),lnd_a4_D.mean=mean(lnd_a4_D)),by= .(report_id)]

lnd_payment_state1_max<-lnd_payment_state1[, .(lnd_a1.max= max(lnd_a1),lnd_a4_1.max=max(lnd_a4_1),lnd_a4_2.max=max(lnd_a4_2),lnd_a4_3.max=max(lnd_a4_3),lnd_a4_4.max=max(lnd_a4_4),lnd_a4_5.max=max(lnd_a4_5),lnd_a4_6.max=max(lnd_a4_6),lnd_a4_7.max=max(lnd_a4_7),lnd_a4_C.max=max(lnd_a4_C),lnd_a4_D.max=max(lnd_a4_D)),by= .(report_id)]

lnd_payment_state1_min<-lnd_payment_state1[, .(lnd_a1.min= min(lnd_a1),lnd_a4_1.min=min(lnd_a4_1),lnd_a4_2.min=min(lnd_a4_2),lnd_a4_3.min=min(lnd_a4_3),lnd_a4_4.min=min(lnd_a4_4),lnd_a4_5.min=min(lnd_a4_5),lnd_a4_6.min=min(lnd_a4_6),lnd_a4_7.min=min(lnd_a4_7),lnd_a4_C.min=min(lnd_a4_C),lnd_a4_D.min=min(lnd_a4_D)),by= .(report_id)]



#合并特征

lnd_tezheng1_union<-cbind(lnd_tezheng1_1_mean,lnd_tezheng1_1_max[,-1],lnd_payment_state1_mean[,-1],lnd_payment_state1_max[,-1])

head(lnd_tezheng1_union)

nrow(lnd_tezheng1_union)

apply(lnd_tezheng1_union,2,function(x) sum(is.na(x)))



############特征第二波state、finance_org、credit_limit_amount、share_credit_limit_amount、guarantee_type、cardtype

head(contest_ext_crd_cd_lnd)

apply(contest_ext_crd_cd_lnd,2,function(x) sum(is.na(x)))

##计算每个report_id的loancard_id数

report_loancard_id_n<- contest_ext_crd_cd_lnd[, .(.N), by = .(report_id)]

report_loancard_id_n1<- report_loancard_id_n[, .(report_id=report_id,loancard_id_n=N)]

head(report_loancard_id_n1)

##每个report_id的各账户状态数

lnd_report_state_n<- contest_ext_crd_cd_lnd[, .(.N), by = .(report_id,state)]

#将report_state_n变为宽表

lnd_report_state_n<-dcast(lnd_report_state_n, report_id ~ state, value.var = 'N')

lnd_report_state_n[is.na(lnd_report_state_n)]<-0

head(lnd_report_state_n)

names(lnd_report_state_n)[c(2,3,4,5,6,7)]<-c("lnd_dongjie_n","lnd_daizhang_n","lnd_weijihuo_n","lnd_zhifu_n","lnd_zhengchang_n","lnd_xiaohu_n")

##每个report_id贷记卡机构数

#将贷记卡机构为NA的标识为Na

table(contest_ext_crd_cd_ln$finance_org)

contest_ext_crd_cd_lnd$finance_org[is.na(contest_ext_crd_cd_lnd$finance_org)]<-"Na"

table(contest_ext_crd_cd_lnd$finance_org)

#计算id机构数

contest_ext_crd_cd_lnd_gg<-contest_ext_crd_cd_lnd[,c(1,2,4)]

contest_ext_crd_cd_lnd_gg_n<- contest_ext_crd_cd_lnd_gg[, .(.N), by = .(report_id,finance_org)]

contest_ext_crd_cd_lnd_gg_n1<- contest_ext_crd_cd_lnd_gg_n[,-3][, .(.N), by = .(report_id)]

names(contest_ext_crd_cd_lnd_gg_n1)[2]<-"daijikajigou_n"

head(contest_ext_crd_cd_lnd_gg_n1)



##各担保方式下的贷款笔数

contest_ext_crd_cd_lnd_gt<-contest_ext_crd_cd_lnd[,c(1,8)]

contest_ext_crd_cd_lnd_gt_n<- contest_ext_crd_cd_lnd_gt[, .(.N), by = .(report_id,guarantee_type)]

contest_ext_crd_cd_lnd_gt_n1<-dcast(contest_ext_crd_cd_lnd_gt_n, report_id ~ guarantee_type, value.var = 'N')

contest_ext_crd_cd_lnd_gt_n1[is.na(contest_ext_crd_cd_lnd_gt_n1)]<-0

head(contest_ext_crd_cd_lnd_gt_n1)

names(contest_ext_crd_cd_lnd_gt_n1)[c(2:7)]<-c("lnd_danbao_n1","lnd_danbao_n2","lnd_danbao_n3","lnd_danbao_n4","lnd_danbao_n5","lnd_danbao_n6")



##各担保方式贷款总额

contest_ext_crd_cd_lnd_gt_amount<-contest_ext_crd_cd_lnd[,c(1,8,9)]

lnd_report_gt_amount<- contest_ext_crd_cd_lnd_gt_amount[, .(sum(share_credit_limit_amount)), by = .(report_id,guarantee_type)]

#将report_gt_amount根据guarantee_type和credit_limit_amount总额v1变为宽表，即得各担保方式贷款总额

lnd_report_gt_amount1<-dcast(lnd_report_gt_amount, report_id ~ guarantee_type, value.var = 'V1')

lnd_report_gt_amount1[is.na(lnd_report_gt_amount1)]<-0

head(lnd_report_gt_amount1)

names(lnd_report_gt_amount1)[c(2:7)]<-c("lnd_danbao_m1","lnd_danbao_m2","lnd_danbao_m3","lnd_danbao_m4","lnd_danbao_m5","lnd_danbao_m6")

#合并特征

lnd_tezheng2_union<-cbind(report_loancard_id_n1,lnd_report_state_n[,-1],contest_ext_crd_cd_lnd_gg_n1[,-1],contest_ext_crd_cd_lnd_gt_n1[,-1],lnd_report_gt_amount1[,-1])

str(lnd_tezheng2_union)

str(lnd_tezheng1_union)

nrow(lnd_tezheng2_union)

lnd_tezheng2_union$report_id<-as.character(lnd_tezheng2_union$report_id)

###贷记卡表特征合并

daijikabiao<-full_join(lnd_tezheng1_union,lnd_tezheng2_union)

daijikabiao[is.na(daijikabiao)]<-0

apply(daijikabiao,2,function(x) sum(is.na(x)))

str(daijikabiao)

####与上面特征合并

names(daijikabiao)[1]<-"REPORT_ID"

union3<-left_join(union2,daijikabiao)

apply(union3,2,function(x) sum(is.na(x)))

#合并部分缺失理解为用户没有贷记卡，故合并产生的缺失用0补齐

union3[is.na(union3)]<-0

str(union3)



#########################################contest_ext_crd_is_creditcue表################################################################

#再上面的基础上增加一个特征：准贷记卡账户数,,合并导致的缺失暂时不管

contest_ext_crd_is_creditcue<-fread("contest_ext_crd_is_creditcue.csv",encoding="UTF-8")

union4<-left_join(union3,contest_ext_crd_is_creditcue[,c(1,8)])

union4$STANDARD_LOANCARD_COUNT<-as.numeric(union4$STANDARD_LOANCARD_COUNT)

apply(union4,2,function(x) sum(is.na(x)))

nrow(union4)

##剔除产生的30个缺失样本，其中一个样本来自测试集

union4<-na.omit(union4)

str(union4)



str(contest_ext_crd_is_creditcue)

apply(contest_ext_crd_is_creditcue,2,function(x) sum(is.na(x)))

apply(contest_ext_crd_is_creditcue,2,function(x) sum(x=="--"))

contest_ext_crd_is_creditcue$FIRST_LOAN_OPEN_MONTH[contest_ext_crd_is_creditcue$FIRST_LOAN_OPEN_MONTH =="--"]<-NA

contest_ext_crd_is_creditcue$FIRST_LOANCARD_OPEN_MONTH[contest_ext_crd_is_creditcue$FIRST_LOANCARD_OPEN_MONTH=="--"]<-NA

contest_ext_crd_is_creditcue$FIRST_SL_OPEN_MONTH[contest_ext_crd_is_creditcue$FIRST_SL_OPEN_MONTH=="--"]<-NA



contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")

contest_basic_train$REPORT_ID<-as.character(contest_basic_train$REPORT_ID)

contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")

contest_basic_test$REPORT_ID<-as.character(contest_basic_test$REPORT_ID)

contest_ext_crd_is_creditcue1<-inner_join(contest_ext_crd_is_creditcue,contest_basic_train[,c(1,11)])

contest_ext_crd_is_creditcue2<-inner_join(contest_ext_crd_is_creditcue,contest_basic_test[,c(1,10)])

str(contest_ext_crd_is_creditcue2)

mean(!complete.cases(contest_ext_crd_is_creditcue2))#总体缺失占比

#查看缺失值

library(VIM)

library(mice)

md.pattern(contest_ext_crd_is_creditcue)

aggr(contest_ext_crd_is_creditcue,col = c("skyblue", "pink"), prop=T,numbers=F,only.miss=T,bars=T,labels=paste("creditcue_",c(1:11)),cex.axis=0.6, gap=5)



#########################################contest_ext_crd_is_sharedebt表################################################################

contest_ext_crd_is_sharedebt<-fread("contest_ext_crd_is_sharedebt.csv",encoding="UTF-8")

contest_ext_crd_is_sharedebt$MAX_CREDIT_LIMIT_PER_ORG[contest_ext_crd_is_sharedebt$MAX_CREDIT_LIMIT_PER_ORG==""]<-NA

contest_ext_crd_is_sharedebt$MIN_CREDIT_LIMIT_PER_ORG[contest_ext_crd_is_sharedebt$MIN_CREDIT_LIMIT_PER_ORG==""]<-NA

contest_ext_crd_is_sharedebt$BALANCE[contest_ext_crd_is_sharedebt$BALANCE==""]<-NA

contest_ext_crd_is_sharedebt$USED_CREDIT_LIMIT[contest_ext_crd_is_sharedebt$USED_CREDIT_LIMIT==""]<-NA

apply(contest_ext_crd_is_sharedebt,2,function(x) sum(is.na(x)))

apply(contest_ext_crd_is_sharedebt,2,function(x) sum(x==""))

ncol(contest_ext_crd_is_sharedebt)

nrow(contest_ext_crd_is_sharedebt)

head(contest_ext_crd_is_sharedebt,20)

str(contest_ext_crd_is_sharedebt)



contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")

contest_basic_train$REPORT_ID<-as.character(contest_basic_train$REPORT_ID)

contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")

contest_basic_test$REPORT_ID<-as.character(contest_basic_test$REPORT_ID)

contest_ext_crd_is_sharedebt1<-inner_join(contest_ext_crd_is_sharedebt,contest_basic_train[,c(1,11)])

contest_ext_crd_is_sharedebt2<-inner_join(contest_ext_crd_is_sharedebt,contest_basic_test[,c(1,10)])

str(contest_ext_crd_is_sharedebt2)

mean(!complete.cases(contest_ext_crd_is_sharedebt1))#总体缺失占比

#查看缺失值

library(VIM)

library(mice)

md.pattern(contest_ext_crd_is_sharedebt)

aggr(contest_ext_crd_is_sharedebt,col = c("skyblue", "pink"), prop=T,numbers=F,only.miss=T,bars=T,labels=paste("sharedebt_",c(1:11)),cex.axis=0.6, gap=5)

##计算每个report_id的loan_id数



############sharedebt 特征第一波,计算成对缺失数值型字段的min\max\mean

#REPORT_ID 、以及其他成对缺失的指标

share_tezheng1<-contest_ext_crd_is_sharedebt[,c(1,2,7,8,10)]

head(share_tezheng1_1)

nrow(share_tezheng1_max_amount11)

apply(share_tezheng1,2,function(x) sum(is.na(x)))

share_tezheng1_1<-na.omit(share_tezheng1)

share_tezheng1_1$MAX_CREDIT_LIMIT_PER_ORG<-as.numeric(share_tezheng1_1$MAX_CREDIT_LIMIT_PER_ORG)

share_tezheng1_1$MIN_CREDIT_LIMIT_PER_ORG<-as.numeric(share_tezheng1_1$MIN_CREDIT_LIMIT_PER_ORG)

share_tezheng1_1$USED_CREDIT_LIMIT<-as.numeric(share_tezheng1_1$USED_CREDIT_LIMIT)

##各类贷款的平均单个贷款机构最大合同金额

share_tezheng1_max_amount1<-dcast(share_tezheng1_1, REPORT_ID ~ TYPE_DW, value.var = 'MAX_CREDIT_LIMIT_PER_ORG')

share_tezheng1_max_amount1[is.na(share_tezheng1_max_amount1)]<-0

head(share_tezheng1_max_amount1)

names(share_tezheng1_max_amount1)[c(2:3)]<-c("max_weixiaohu_zd_m1","max_weixiaohu_d_m2")



##各类贷款的平均单个贷款机构最小合同金额

share_tezheng1_min_amount1<-dcast(share_tezheng1_1, REPORT_ID ~ TYPE_DW, value.var = 'MIN_CREDIT_LIMIT_PER_ORG')

share_tezheng1_min_amount1[is.na(share_tezheng1_min_amount1)]<-0

head(share_tezheng1_min_amount1)

names(share_tezheng1_min_amount1)[c(2:3)]<-c("min_weixiaohu_zd_m1","min_weixiaohu_d_m2")



##各类贷款的已用额度

share_tezheng1_used_amount1<-dcast(share_tezheng1_1, REPORT_ID ~ TYPE_DW, value.var = 'USED_CREDIT_LIMIT')

share_tezheng1_used_amount1[is.na(share_tezheng1_used_amount1)]<-0

head(share_tezheng1_used_amount1)

names(share_tezheng1_used_amount1)[c(2:3)]<-c("used_weixiaohu_zd_m1","used_weixiaohu_d_m2")

#合并特征

share_tezheng1_union<-cbind(share_tezheng1_max_amount1,share_tezheng1_min_amount1[,-1],share_tezheng1_used_amount1[,-1])

head(share_tezheng1_union)

nrow(share_tezheng1_union)

apply(share_tezheng1_union,2,function(x) sum(is.na(x)))





############sharedebt 特征第二波,计算没有缺失的特征



##type_dw,计算每种贷款数目（即是否有该类贷款）

table(contest_ext_crd_is_sharedebt$TYPE_DW)

sharedebt_DW_n<-contest_ext_crd_is_sharedebt[, .(.N), by = .(REPORT_ID,TYPE_DW)]

#将sharedebt_DW_n变为宽表

sharedebt_DW_n1<-dcast(sharedebt_DW_n, REPORT_ID ~ TYPE_DW, value.var = 'N')

sharedebt_DW_n1[is.na(sharedebt_DW_n1)]<-0

str(sharedebt_DW_n1)

names(sharedebt_DW_n1)[2:4]<-c("s_weijieqing","s_weixiaohu_zd","s_weixiaohu_d")

##finance_corp_count，计算各类贷款的贷款法人数

sharedebt_fcc_n1<-contest_ext_crd_is_sharedebt[,c(1,2,3)]

sharedebt_fcc_n2<-dcast(sharedebt_fcc_n1, REPORT_ID ~ TYPE_DW, value.var = 'FINANCE_CORP_COUNT')

sharedebt_fcc_n2[is.na(sharedebt_fcc_n2)]<-0

str(sharedebt_fcc_n2)

names(sharedebt_fcc_n2)[c(2:4)]<-c("fcc_weijieqing_n","fcc_weixiaohu_zd_n","fcc_weixiaohu_d_n")

##finance_org_count计算各类贷款的贷款机构数

sharedebt_foc_n1<-contest_ext_crd_is_sharedebt[,c(1,2,4)]

sharedebt_foc_n2<-dcast(sharedebt_foc_n1, REPORT_ID ~ TYPE_DW, value.var = 'FINANCE_ORG_COUNT')

sharedebt_foc_n2[is.na(sharedebt_foc_n2)]<-0

names(sharedebt_foc_n2)[2:4]<-c("foc_weijieqing_n","foc_weixiaohu_zd_n","foc_weixiaohu_d_n")

##account_count,计算各类贷款的贷款账户数

sharedebt_ac_n1<-contest_ext_crd_is_sharedebt[,c(1,2,5)]

sharedebt_ac_n2<-dcast(sharedebt_ac_n1, REPORT_ID ~ TYPE_DW, value.var = 'ACCOUNT_COUNT')

sharedebt_ac_n2[is.na(sharedebt_ac_n2)]<-0

names(sharedebt_ac_n2)[2:4]<-c("ac_weijieqing_n","ac_weixiaohu_zd_n","ac_weixiaohu_d_n")

##credit_limit,计算各类贷款的合同金额

sharedebt_cl_n1<-contest_ext_crd_is_sharedebt[,c(1,2,6)]

sharedebt_cl_n2<-dcast(sharedebt_cl_n1, REPORT_ID ~ TYPE_DW, value.var = 'CREDIT_LIMIT')

sharedebt_cl_n2[is.na(sharedebt_cl_n2)]<-0

names(sharedebt_cl_n2)[2:4]<-c("cl_weijieqing_n","cl_weixiaohu_zd_n","cl_weixiaohu_d_n")

##latest_6m_used_avg_amount,计算各类贷款的最近6个月平均使用额度

sharedebt_l6a_n1<-contest_ext_crd_is_sharedebt[,c(1,2,11)]

sharedebt_l6a_n2<-dcast(sharedebt_l6a_n1, REPORT_ID ~ TYPE_DW, value.var = 'LATEST_6M_USED_AVG_AMOUNT')

sharedebt_l6a_n2[is.na(sharedebt_l6a_n2)]<-0

names(sharedebt_l6a_n2)[2:4]<-c("l6a_weijieqing_n","l6a_weixiaohu_zd_n","l6a_weixiaohu_d_n")

#合并特征

share_tezheng2_union<-cbind(sharedebt_DW_n1,sharedebt_fcc_n2[,-1],sharedebt_foc_n2[,-1],sharedebt_ac_n2[,-1],sharedebt_cl_n2[,-1],sharedebt_l6a_n2[,-1])

head(share_tezheng2_union)

nrow(share_tezheng2_union)

apply(share_tezheng2_union,2,function(x) sum(is.na(x)))

###share表特征合并

sharebiao<-full_join(share_tezheng1_union,share_tezheng2_union)

sharebiao[is.na(sharebiao)]<-0

apply(sharebiao,2,function(x) sum(is.na(x)))

str(sharebiao)

sharebiao$fcc_weijieqing_n<-as.numeric(sharebiao$fcc_weijieqing_n)

sharebiao$fcc_weixiaohu_zd_n<-as.numeric(sharebiao$fcc_weixiaohu_zd_n)

sharebiao$fcc_fcc_weixiaohu_d_n<-as.numeric(sharebiao$fcc_weixiaohu_d_n)

sharebiao$foc_weijieqing_n<-as.numeric(sharebiao$foc_weijieqing_n)

sharebiao$foc_weixiaohu_zd_n<-as.numeric(sharebiao$foc_weixiaohu_zd_n)

sharebiao$foc_weixiaohu_d_n<-as.numeric(sharebiao$foc_weixiaohu_d_n)

sharebiao$ac_weijieqing_n<-as.numeric(sharebiao$ac_weijieqing_n)

sharebiao$ac_weixiaohu_zd_n<-as.numeric(sharebiao$ac_weixiaohu_zd_n)

sharebiao$ac_weixiaohu_d_n<-as.numeric(sharebiao$ac_weixiaohu_d_n)

sharebiao$cl_weijieqing_n<-as.numeric(sharebiao$cl_weijieqing_n)

sharebiao$cl_weixiaohu_zd_n <-as.numeric(sharebiao$cl_weixiaohu_zd_n)

sharebiao$cl_weixiaohu_d_n<-as.numeric(sharebiao$cl_weixiaohu_d_n)

sharebiao$l6a_weijieqing_n<-as.numeric(sharebiao$l6a_weijieqing_n)

sharebiao$l6a_weixiaohu_zd_n <-as.numeric(sharebiao$l6a_weixiaohu_zd_n)

sharebiao$l6a_weixiaohu_d_n<-as.numeric(sharebiao$l6a_weixiaohu_d_n)



####与上面特征合并

union5<-left_join(union4,sharebiao)

str(union5)

apply(union5,2,function(x) sum(is.na(x)))

##剔除产生的52个缺失样本，其中一个样本来自测试集

union5<-na.omit(union5)



#合并部分缺失理解为用户没有贷记卡，故合并产生的缺失用0补齐

union5[is.na(union5)]<-0

nrow(union5)

#########################################contest_ext_crd_is_ovdsummary表################################################################



contest_ext_crd_is_ovdsummary<-fread("contest_ext_crd_is_ovdsummary.csv",encoding="UTF-8")

apply(contest_ext_crd_is_ovdsummary,2,function(x) sum(is.na(x)))

apply(contest_ext_crd_is_ovdsummary,2,function(x) sum(x==""))









head(contest_ext_crd_is_ovdsummary)

##count_dw,计算每种贷款的贷款逾期笔数

ovdsummary_cdw_n1<-contest_ext_crd_is_ovdsummary[,c(1,2,3)]

ovdsummary_cdw_n2<-dcast(ovdsummary_cdw_n1, REPORT_ID ~ TYPE_DW, value.var = 'COUNT_DW')

head(ovdsummary_cdw_n2)

names(ovdsummary_cdw_n2)[2:4]<-c("ovd_yuqi_n1","ovd_yuqi_n2","ovd_yuqi_n3")



##months,计算每种贷款的贷款逾期月份数

ovdsummary_m_n1<-contest_ext_crd_is_ovdsummary[,c(1,2,4)]

ovdsummary_m_n2<-dcast(ovdsummary_m_n1, REPORT_ID ~ TYPE_DW, value.var = 'MONTHS')

head(ovdsummary_m_n2)

names(ovdsummary_m_n2)[2:4]<-c("ovd_month_n1","ovd_month_n2","ovd_month_n3")



##highest_oa_per_mon,计算每种贷款的贷款单月最高逾期总额

ovdsummary_hom_n1<-contest_ext_crd_is_ovdsummary[,c(1,2,5)]

ovdsummary_hom_n2<-dcast(ovdsummary_hom_n1, REPORT_ID ~ TYPE_DW, value.var = 'HIGHEST_OA_PER_MON')

head(ovdsummary_hom_n2)

names(ovdsummary_hom_n2)[2:4]<-c("ovd_hm_m1","ovd_hm_m2","ovd_hm_m3")

##max_duration,计算每种贷款的最大贷款时长

ovdsummary_md_n1<-contest_ext_crd_is_ovdsummary[,c(1,2,6)]

ovdsummary_md_n2<-dcast(ovdsummary_md_n1, REPORT_ID ~ TYPE_DW, value.var = 'MAX_DURATION')

head(ovdsummary_md_n2)

names(ovdsummary_md_n2)[2:4]<-c("ovd_m_t1","ovd_m_t2","ovd_m_t3")

##h合并特征

ovdsummary<-cbind(ovdsummary_cdw_n2,ovdsummary_m_n2[,-1],ovdsummary_hom_n2[,-1],ovdsummary_md_n2[,-1])

##与上面的特征合并

str(ovdsummary)

ovdsummary$ovd_yuqi_n1<-as.numeric(ovdsummary$ovd_yuqi_n1)

ovdsummary$ovd_yuqi_n2<-as.numeric(ovdsummary$ovd_yuqi_n2)

ovdsummary$ovd_yuqi_n3<-as.numeric(ovdsummary$ovd_yuqi_n3)

ovdsummary$ovd_month_n1<-as.numeric(ovdsummary$ovd_month_n1)

ovdsummary$ovd_month_n2<-as.numeric(ovdsummary$ovd_month_n2)

ovdsummary$ovd_month_n3<-as.numeric(ovdsummary$ovd_month_n3)

ovdsummary$ovd_hm_m1<-as.numeric(ovdsummary$ovd_hm_m1)

ovdsummary$ovd_hm_m2<-as.numeric(ovdsummary$ovd_hm_m2)

ovdsummary$ovd_hm_m3<-as.numeric(ovdsummary$ovd_hm_m3)

ovdsummary$ovd_m_t1<-as.numeric(ovdsummary$ovd_m_t1)

ovdsummary$ovd_m_t2<-as.numeric(ovdsummary$ovd_m_t2)

ovdsummary$ovd_m_t3<-as.numeric(ovdsummary$ovd_m_t3)



union6<-left_join(union5,ovdsummary)

str(union6)

apply(union6,2,function(x) sum(is.na(x)))

#合并部分缺失理解为用户没有贷记卡，故合并产生的缺失用0补齐

union6[is.na(union6)]<-0

str(union6)

#############################contest_ext_crd_qr_recorddtlinfo表##############################################################################

recorddtlinfo<-fread("contest_ext_crd_qr_recorddtlinfo.tsv",encoding="UTF-8")[,-3]

head(recorddtlinfo)

str(recorddtlinfo)

apply(recorddtlinfo,2,function(x) sum(is.na(x)))

apply(recorddtlinfo,2,function(x) sum(x==""))

table(recorddtlinfo$query_reason)

##各report_id查询次数

recorddtlinfo_id_n<- recorddtlinfo[, .(.N), by = .(report_id)]

head(recorddtlinfo_id_n)

names(recorddtlinfo_id_n)[2]<-"chaxun_n"

nrow(recorddtlinfo_id_n)

##query_reason,各原因的查询个数

recorddtlinfo_qr_n<- recorddtlinfo[, .(.N), by = .(report_id,query_reason)]

recorddtlinfo_qr_n1<-dcast(recorddtlinfo_qr_n, report_id ~ query_reason, value.var = 'N')

recorddtlinfo_qr_n1[is.na(recorddtlinfo_qr_n1)]<-0

head(recorddtlinfo_qr_n1)

names(recorddtlinfo_qr_n1)[2:4]<-c("info_n1","info_n2","info_n3")

str(recorddtlinfo_qr_n1)

##合并特征

info<-cbind(recorddtlinfo_id_n,recorddtlinfo_qr_n1[,-1])

str(info)

##与上面特征合并

names(info)[1]<-"REPORT_ID"

info$REPORT_ID<-as.character(info$REPORT_ID)

union7<-left_join(union6,info)

str(union7)

apply(union7,2,function(x) sum(is.na(x)))

#合并部分缺失理解为用户没有贷记卡，故合并产生的缺失用0补齐

union7[is.na(union7)]<-0

str(union7)



#############################contest_ext_crd_cd_ln_spl表#################################################################################

spl<-fread("contest_ext_crd_cd_ln_spl.tsv",encoding="UTF-8")

head(spl)

spl$content[943]

spl[943,]

str(spl)

apply(spl,2,function(x) sum(is.na(x)))

apply(spl,2,function(x) sum(x=="--"))

length(table(spl$content))

##计算各类贷款的交易数

spl_dw_n<- spl[, .(.N), by = .(report_id,type_dw)]

spl_dw_n1<-dcast(spl_dw_n, report_id ~ type_dw, value.var = 'N')

spl_dw_n1[is.na(spl_dw_n1)]<-0

head(spl_dw_n1)

names(spl_dw_n1)[2:6]<-c("spl_jiaoyi_n1","spl_jiaoyi_n2","spl_jiaoyi_n3","spl_jiaoyi_n4","spl_jiaoyi_n5")

##计算各类贷款的变更月数

spl_cm<- spl[, .(sum(changing_months)), by = .(report_id,type_dw)]

spl_cm1<-dcast(spl_cm, report_id ~ type_dw, value.var = 'V1')

spl_cm1[is.na(spl_cm1)]<-0

head(spl_cm1)

names(spl_cm1)[2:6]<-c("spl_month_n1","spl_month_n2","spl_month_n3","spl_month_n4","spl_month_n5")

##计算各类贷款的发生金额

spl_ca<- spl[, .(sum(changing_amount)), by = .(report_id,type_dw)]

spl_ca1<-dcast(spl_ca, report_id ~ type_dw, value.var = 'V1')

spl_ca1[is.na(spl_ca1)]<-0

head(spl_ca1)

names(spl_ca1)[2:6]<-c("spl_m1","spl_m2","spl_m3","spl_m4","spl_m5")

##合并特征

splf<-cbind(spl_dw_n1,spl_cm1[,-1],spl_ca1[,-1])

str(splf)

##与上面特征合并

names(splf)[1]<-"REPORT_ID"

splf$REPORT_ID<-as.character(splf$REPORT_ID)

union8<-left_join(union7,splf)

nrow(union8)

apply(union8,2,function(x) sum(is.na(x)))

#合并部分缺失理解为用户没有贷记卡，故合并产生的缺失用0补齐

union8[is.na(union8)]<-0

str(union8)







contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")

names(contest_basic_train)[1]<-"report_id"

contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")

names(contest_basic_test)[1]<-"report_id"

spl1<-inner_join(spl,contest_basic_train[,c(1,11)])

spl2<-inner_join(spl,contest_basic_test[,c(1,10)])

str(contest_ext_crd_cd_lnd_ovd2)

mean(!complete.cases(contest_ext_crd_cd_lnd_ovd2))#总体缺失占比

table(spl1[spl1$Y==1,]$type_dw)

table(spl1[spl1$Y==0,]$type_dw)



#############################contest_ext_crd_cd_lnd_ovd表#################################################################################

contest_ext_crd_cd_lnd_ovd<-fread("contest_ext_crd_cd_lnd_ovd.csv",encoding="UTF-8")

contest_ext_crd_cd_lnd_ovd$AMOUNT[contest_ext_crd_cd_lnd_ovd$AMOUNT==""]<-NA

contest_ext_crd_cd_lnd_ovd$LAST_MONTHS[contest_ext_crd_cd_lnd_ovd$LAST_MONTHS==""]<-NA

nrow(contest_ext_crd_cd_lnd_ovd[contest_ext_crd_cd_lnd_ovd$MONTH_DW=="--            ",])

contest_ext_crd_cd_lnd_ovd$MONTH_DW[contest_ext_crd_cd_lnd_ovd$MONTH_DW=="--            "]<-NA

contest_ext_crd_cd_lnd_ovd$LAST_MONTHS<-as.numeric(contest_ext_crd_cd_lnd_ovd$LAST_MONTHS)

contest_ext_crd_cd_lnd_ovd$AMOUNT<-as.numeric(contest_ext_crd_cd_lnd_ovd$AMOUNT)

na1<-contest_ext_crd_cd_lnd_ovd[!is.na(contest_ext_crd_cd_lnd_ovd$MONTH_DW),]

apply(na1,2,function(x) sum(is.na(x)))



str(contest_ext_crd_cd_lnd_ovd)

contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")

contest_basic_train$REPORT_ID<-as.character(contest_basic_train$REPORT_ID)

contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")

contest_basic_test$REPORT_ID<-as.character(contest_basic_test$REPORT_ID)

contest_ext_crd_cd_lnd_ovd1<-inner_join(contest_ext_crd_cd_lnd_ovd,contest_basic_train[,c(1,11)])

contest_ext_crd_cd_lnd_ovd2<-inner_join(contest_ext_crd_cd_lnd_ovd,contest_basic_test[,c(1,10)])



#查看缺失值

library(VIM)

library(mice)

md.pattern(contest_ext_crd_cd_lnd_ovd)

aggr(contest_ext_crd_cd_lnd_ovd,col = c("skyblue", "pink"), prop=T,numbers=F,only.miss=T,bars=T,labels=paste("sharedebt_",c(1:4)),cex.axis=0.6, gap=5)





#逾期金额、月份数mean

na1$LAST_MONTHS<-as.numeric(na1$LAST_MONTHS)

na1$AMOUNT<-as.numeric(na1$AMOUNT)

na1<-na1[,-2]

lnd_ovd_mean<-na1[, .(AMOUNT.mean= mean(AMOUNT),LAST_MONTHS.mean= mean(LAST_MONTHS)),by= .(REPORT_ID)]

##与上面特征合并

union9<-left_join(union8,lnd_ovd_mean)

str(lnd_ovd_mean)

head(union9)

apply(union9,2,function(x) sum(is.na(x)))

#合并部分缺失理解为用户没有贷记卡，故合并产生的缺失用0补齐

union9[is.na(union9)]<-0

str(union9)

nrow(union9)









# 合并这些图形在一个绘图区域，cols = 2的意思就是排版为一行二列

multiplot(box_sat, box_eva, box_mon, box_time, cols = 2)



write.csv(union9,"union9_2_23_52.csv")

###################用union9跑一下模型#####################

contest_basic_train$REPORT_ID<-as.character(contest_basic_train$REPORT_ID)

union9$SALARY<-as.numeric(union9$SALARY)

union9$STANDARD_LOANCARD_COUNT<-as.numeric(union9$STANDARD_LOANCARD_COUNT)

union9$SALARY<-as.numeric(union9$SALARY)

union9_train<-inner_join(union9,contest_basic_train[,c(1,11)])

test<-inner_join(contest_basic_test[,c(1,2)],union9)[,-2]

class(union9_train$Y)

names(union9)

##独热编码

library(dummies)

union9_onehot<-dummy.data.frame(union9_train,names=c("AGENT","QUERY_REASON","QUERY_ORG","IS_LOCAL","WORK_PROVINCE","EDU_LEVEL","MARRY_STATUS","HAS_FUND","sex","a","b","c"),sep=".")

names(union9_onehot)

nrow(union9_onehot)

table(union9_onehot$Y)



##分层抽样

library(caret)        # createDataPartition() / train()

## 按照类别进行分层抽样，建立训练集和测试集

set.seed(123) # 设定随机种子

## 按照新数据的目标变量进行8:2的分层抽样，返回矩阵形式的抽样索引

index2 <- createDataPartition(union9_onehot$Y, p = 0.8, list = F)

traindata <- union9_onehot[index2, ] # 创建训练集

testdata <- union9_onehot[-index2, ]# 创建测试集

summary(traindata)

str(testdata)

## 验证抽样结果，统计三个数据集中正反例比例是否一致

prop.table(table(union9_onehot$Y))

prop.table(table(traindata$Y))

prop.table(table(testdata$Y))

## 对数据进行标准化

standard <- preProcess(union9_onehot, method = 'range')

card_s <- predict(standard, union9_onehot)

traindata2 <- card_s[index2, ]

testdata2 <- card_s[-index2, ]

#traindata2<-subset(traindata2,select=-c(keyi1_n, ciji1_n, a4_C.mean, a4_C.max, lnd_a4_2.mean, lnd_a4_3.mean, lnd_a4_4.mean, lnd_a4_5.mean, lnd_a4_6.mean, lnd_a4_7.mean, lnd_a4_C.mean, lnd_a4_D.mean, lnd_a4_2.max, lnd_a4_3.max, lnd_a4_4.max, lnd_a4_5.max, lnd_a4_6.max, lnd_a4_7.max, lnd_a4_C.max, lnd_a4_D.max, lnd_daizhang_n))

names(traindata2)





##logist模型

names(traindata2)

logit.model = glm(Y~.,data = traindata2[,-1],family = "binomial",control=list(maxit=200))

#logit.model1 = step(logit.model)

summary(logit.model1)

logit.response = predict(logit.model,testdata2[,-c(1,224)],type = "response")

logit.predict = ifelse(logit.response>0.5,"1","0")

table(testdata2$Y,logit.predict)

accurancy = mean(logit.predict == testdata2$Y)

accurancy

table(gbm.predict)

library(pROC)

modelroc_logit <- roc(testdata2$Y,logit.response)

plot(modelroc_logit , print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     
     auc.polygon.col="skyblue", print.thres=TRUE)



p<-predict(lgst,type='response')

qplot(seq(-2,2,length=59900),sort(logit.response),col='predict')

max(sort(logit.response))







library(xgboost)

xgb1 <- xgboost(data = data.matrix(traindata2[,-c(1,224)]), 
                
                label =data.matrix(traindata2$Y), 
                
                eta = 0.1,
                
                max_depth = 15, 
                
                nround=200, 
                
                subsample = 0.5,
                
                colsample_bytree = 0.5,
                
                seed = 1,
                
                objective = "binary:logistic",
                
                nthread = 3
                
)

y_pred1 <- predict(xgb1, data.matrix(testdata2[,-c(1,224)]))

pre1 = ifelse(y_pred1>0.5,1,0)

table(testdata2$Y,pre1)

accurancy1= mean(pre1 == testdata2$Y)

pred_1<-as.numeric(y_pred1)

modelroc_1<- roc(testdata2$Y,pred_1)

plot(modelroc_1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     
     auc.polygon.col="skyblue", print.thres=TRUE)



xgb2 <- xgboost(data = data.matrix(traindata2[,-c(1,224)]), 
                
                label =data.matrix(traindata2$Y), 
                
                eta = 0.15,
                
                max_depth = 15, 
                
                nround=400, 
                
                subsample = 0.4,
                
                colsample_bytree = 0.5,
                
                seed = 1,
                
                objective = "binary:logistic",
                
                nthread = 3
                
)



y_pred2 <- predict(xgb2, data.matrix(testdata2[,-c(1,224)]))

pre2 = ifelse(y_pred2>0.5,1,0)

table(testdata2$Y,pre2)

accurancy2 = mean(pre2 == testdata2$Y)

pred_2<-as.numeric(y_pred2)

modelroc_2<- roc(testdata2$Y,pred_2)

plot(modelroc_2, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     
     auc.polygon.col="skyblue", print.thres=TRUE)



ph<-0.4*y_pred1+0.2*logit.response+0.4*y_pred2

ph<-as.numeric(ph)

modelroc_ph <- roc(testdata2$Y,ph)

plot(modelroc_ph, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     
     auc.polygon.col="skyblue", print.thres=TRUE)

pre2ph = ifelse(ph>0.011,1,0)

table(testdata2$Y,pre2ph)

accurancy2ph = mean(pre2ph == testdata2$Y)



#变量重要性排序图

model <- xgb.dump(xgb, with.stats = T)

names <- dimnames(data.matrix(traindata2[,-c(1,224)]))[[2]]

importance_matrix <- xgb.importance(names, model = xgb)

xgb.plot.importance(importance_matrix[1:20,])

xgb.plot.importance(importance_matrix[1:20,], col = c("lightblue", "mistyrose", "lightcyan","lavender", "cornsilk"),rel_to_first = TRUE, xlab = "Relative importance")

length(testdata$Y)

length(y_pred)

names(testdata2)
































