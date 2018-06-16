rm(list=ls())
#데이터 불러오기

csv_Data<-read.csv('data//교통사고정보//2017_교통사고정보.csv') 
csv_Data2<-read.csv('data//교통사고정보//2016_교통사고정보.csv') 
csv_Data3<-read.csv('data//교통사고정보//2015_교통사고정보.csv') 
csv_Data4<-read.csv('data//교통사고정보//2012_2014_교통사고정보.csv') 

#불필요한 데이터 삭제
library(dplyr)
bind_Data<-bind_rows(csv_Data,csv_Data2,csv_Data3,csv_Data4)
Data<-bind_Data%>%
  select(-발생년,-발생년월일시,-발생분,-당사자종별_1당_대분류,-당사자종별_2당_대분류,-사고유형) #중복 데이터 삭제
#

#데이터 확인
str(Data)

#데이터 타입 형식 변환
Data$발생지시군구<-as.character(Data$발생지시군구)
Data$발생지시도<-as.factor(Data$발생지시도)
Data$사고유형_대분류<-as.factor(Data$사고유형_대분류)
Data$사고유형_중분류<-as.factor(Data$사고유형_중분류)
Data$법규위반_대분류<-as.factor(Data$법규위반_대분류)
Data$법규위반<-as.factor(Data$법규위반)
Data$도로형태_대분류<-as.factor(Data$도로형태_대분류)
Data$도로형태<-as.factor(Data$도로형태)
Data$당사자종별_1당<-as.factor(Data$당사자종별_1당)
Data$당사자종별_2당<-as.factor(Data$당사자종별_2당)


#Data$사망자수<-as.factor(Data$사망자수)
#Data$사상자수<-as.factor(Data$사상자수)
#Data$중상자수<-as.factor(Data$중상자수)
#Data$경상자수<-as.factor(Data$경상자수)

#시간 함수 만들기
Data$time<-substr(Data[,2],9,10)
Data$time<-as.integer(Data$time)

#결측치 확인
table(is.na(Data)) 


library(caret) #교차검증 하기위해(createDataPartition()사용)
set.seed(137) #항상 같은데이터로 분리하기 위해서
test_idx<-createDataPartition(Data$주야,p=0.3)$Resample1 

test_Data<-Data[test_idx,]
train_Data<-Data[-test_idx,]
NROW(test_Data)
prop.table(table(test_Data$주야))
NROW(train_Data)
prop.table(table(train_Data$주야))

#교차 검증 준비
create_ten_fold_cv<-function(){
  set.seed(117)
  lapply(createFolds(train_Data,k=10),function(idx){
    return(list(train=train_Data[-idx,],
                validation=train_Data[idx,]))
  })
}
x<-create_ten_fold_cv()
str(x)
head(x$Fold1$train)

#데이터 탐색
library(Hmisc)
summary(주야~요일+발생지시도+사고유형_대분류+사고유형_중분류+도로형태,data=x$Fold1$train,method='reverse')

#데이터 시각화(featureplot사용)
Data_complete<-x$Fold1$train[complete.cases(x$Fold1$train),]
featurePlot(
  Data_complete[,
                sapply(names(Data_complete),
                       function(n){is.numeric(Data_complete[,n])})],
  Data_complete[,c('주야')],
  "ellipse")

mosaicplot(주야~요일+사고유형_대분류,data=x$Fold1$train,color=TRUE,main='요일 and 사고유형')

xtabs(~요일+사고유형_대분류,data=x$Fold1$train)
xtabs(주야=="야간"~요일+사고유형_대분류,data=x$Fold1$train)/xtabs(~요일+사고유형_대분류,data=x$Fold1$train)
xtabs(주야=="주간"~요일+사고유형_대분류,data=x$Fold1$train)/xtabs(~요일+사고유형_대분류,data=x$Fold1$train)

#rpart 모델
library(rpart)
m<-rpart(주야~요일+발생지시도+사고유형_대분류+사고유형_중분류+도로형태,data=train_Data)

#plot(rpart(주야~요일+발생지시도+사고유형_대분류+사고유형_중분류+도로형태,data=train_Data,method='class'))
#text(rpart(주야~요일+발생지시도+사고유형_대분류+사고유형_중분류+도로형태,data=train_Data,method='class'))
p<-predict(m,newdata=train_Data,type='class')

plot(p)
text(p)

head(p)
library(foreach) ###foreach, %do% 사용위해
folds<-create_ten_fold_cv()
rpart_result<-foreach(f=folds) %do% {
  model_rpart<-rpart(
    주야~요일+발생지시도+사고유형_대분류+사고유형_중분류+도로형태, data=f$train)
  predicted<-predict(model_rpart, newdata=f$validation,
                     type="class")
  return(list(actual=f$validation$주야,predicted=predicted))
}
#head(rpart_result)
#Accuracy 평가 (Accuracy 계산결과 rpart 모델성능 판단)
evaluation<-function(lst){
  accuracy<-sapply(lst,function(one_result){ #벡터로 묶음
    return(sum(one_result$predicted==one_result$actual)
           /NROW(one_result$actual))
  })
  print(sprintf("MEAN +/- SD: %.3f +/- %.3f",
                mean(accuracy),sd(accuracy))) #평균, 표준편차
  return(accuracy)
}
rpart_accuracy<-evaluation(rpart_result) #Accuracy 계산결과 rpart 모델성능 판단

#ctree 모델
library(party)
ctree_result<-foreach(f=folds)%do% {
  model_ctree<-ctree(
    주야~요일+발생지시도+사고유형_대분류+사고유형_중분류+도로형태,data=f$train)
  predicted<-predict(model_ctree,newdata=f$validation,
                     type='response')
  return(list(actual=f$validation$주야,predicted=predicted))
}
(ctree_accuracy<-evaluation(ctree_result))

#정확도 분포
plot(density(rpart_accuracy),main='rpart VS ctree')
lines(density(ctree_accuracy),col='red',lty='dashed')



