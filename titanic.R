#데이터 불러오기
titanic=read.csv('titanic3.csv')
#head(titanic)
titanic<-titanic[,!names(titanic)%in%
                   c('home.dest','boat','body')]
str(titanic)

#데이터 타입 지정
titanic$pclass<-as.factor(titanic$pclass)
titanic$name<-as.character(titanic$name)
titanic$ticket<-as.character(titanic$ticket)
titanic$cabin<-as.character(titanic$cabin)
titanic$survived<-factor(titanic$survived, levels=c(0,1), #사망(0->1),생존(1->2)
                         labels=c('dead','survived'))
str(titanic)

levels(titanic$embarked)
table(titanic$embarked)

levels(titanic$embarked)[1]<-NA
table(titanic$embarked,useNA='always')

titanic$cabin<-ifelse(titanic$cabin=="",NA,titanic$cabin)
str(titanic)

#테스트 데이터 분리
#데이터 분리는 교차검증(Cross Validation)에서 createDataPartition()사용
#createDataPartition()은 Y값을 고려한 데이터의 분할을 지원하여 훈련데이터와 테스트데이터 간에 일정한 비율 유지
#install.packages('caret')
#install.packages('dimRed')
library(caret)
set.seed(137) #데이터 분리시마다 매번 같은 데이터로 하기위해
test_idx<-createDataPartition(titanic$survived,p=0.1)$Resample1
titanic_test<-titanic[test_idx,]
titanic_train<-titanic[-test_idx,]
NROW(titanic_test)
prop.table(table(titanic_test$survived))
NROW(titanic_train)
prop.table(table(titanic_train$survived))

save(titanic,titanic_test,titanic_train,file='titanic.RData')

#교차 검증 준비
createFolds(titanic_train$survived,k=10)

create_ten_fold_cv<-function(){ #Fold 10개를 가진 리스트를 반환하는 함수, 각 FOld에는 train과 validation으로 저장(훈련데이터,검증데이터)
  set.seed(137)
  lapply(createFolds(titanic_train$survived,k=10),function(idx){
    return(list(train=titanic_train[-idx,],
                validation=titanic_train[idx,]))
  })
}

x<-create_ten_fold_cv()
str(x)

head(x$Fold01$train)

#데이터 탐색
library(Hmisc)

help("summary.formula")
summary(survived~pclass+sex+age+sibsp+parch+fare+embarked, data=x$Fold01$train, method = 'reverse')
#Hmisc::summary.formula(survived~pclass+sex+age+sibsp+parch+fare+embarked, data=x$Fold01$train)

data_complete<-x$Fold01$train[complete.cases(x$Fold01$train),]
#data_complete
featurePlot(
  data_complete[,
                sapply(names(data_complete),
                       function(n){is.numeric(data_complete[,n])})],
  data_complete[,c('survived')],
  "ellipse"
)
