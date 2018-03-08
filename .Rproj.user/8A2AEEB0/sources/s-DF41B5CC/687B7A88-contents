#결측치 정제하기

df<-data.frame(sex=c('M','F',NA,'M','F'),score=c(5,4,3,4,NA)) # <NA> = 결측치
df
#is.na() 결측치 확인
is.na(df) #TRUE = 결측치, FALSE=결측치X
#table(is.na()) 결측치 빈도 출력
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))
library(ggplot2)
library(dplyr)
df%>%filter(is.na(score)) # score가 NA인 데이터만 출력
df%>%filter(!is.na(score))# NA를 제외한 데이터만 출력 
#결측치 제거한 데이터 저장
df_nomiss<-df%>%filter(!is.na(score) & !is.na(sex))
df_nomiss
mean(df_nomiss$score)
#na.omit() 모든 변수에 결측치 없는 데이터 추출 (필요한 데이터도 같이 지워질 수 있음)
df_nomiss2<-na.omit(df)
df_nomiss2
# na.rm=T 결측치 제외하고 값 산출
mean(df$score,na.rm = T)

exam<-read.csv('csv_exam.csv')
exam[c(3,8,15),'math']<-NA # 3,8,15 행의 math에 NA할당
exam%>%summarise(mean_math=mean(math,na.rm=T))
#평균 값으로 결측치 대체하기
mean(exam$math, na.rm=T) #결측치 제외하고 math 평균 산출
exam$math<-ifelse(is.na(exam$math),55,exam$math) # math가 NA이면 55로 
table(is.na(exam$math)) 
mean(exam$math)

#이상치 정제하기
#이상치 sex에 3, score에 6
outlier<-data.frame(sex=c(1,2,1,3,2,1),score=c(5,4,3,4,2,6))
#이상치 확인하기
table(outlier$sex)
table(outlier$score)
#결측 처리하기
outlier$sex<-ifelse(outlier$sex ==3 ,NA,outlier$sex)
outlier$score<-ifelse(outlier$score==6,NA,outlier$score)
outlier
#결측치 제외하기
outlier%>%
  filter(!is.na(sex)&!is.na(score))%>%
  group_by(sex)%>%
  summarise(mean_score=mean(score))

boxplot(mpg$hwy)#상자그림 출력
boxplot(mpg$hwy)$stats #상자그림 통계치 출력(아래쪽극단치경계, 1사분위수, 중앙값, 3사분위수, 위쪽극단치경계)
#결측 처리하기
mpg$hwy<-ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy) #정상범위 설정 후 그 범위 이외에는 NA 으로 대입
table(is.na(mpg$hwy))
#결측치 제외하고 간단한 분석
mpg%>%
  group_by(drv)%>%
  summarise(mean_hwy=mean(hwy,na.rm=T))

mpg<-as.data.frame(ggplot2::mpg) #데이터 불러오기
mpg[c(10,14,58,93),'drv']<-'K' #drv 이상치 할당
mpg[c(29,43,129,203),'cty']<-c(3,4,39,42) #cty에 이상치 할당
#Q1. drv에 이상치가 있는지 확인하세요. 이상치를 결측 처리한 후 이상치가 사라졌는지 확인하세요. 결측처리 할때는 %in% 기호를 활용하세요.
table(mpg$drv)
table(mpg$cty)
mpg$drv<-ifelse(mpg$drv=='K',NA,mpg$drv)
mpg$cty<-ifelse(mpg$cty%in%c(3,4,39,42),NA,mpg$cty)
#Q2. 상자 그림의 통계치를 이용해 정상 범위를 벗어난 값을 결측 처리한 후 다시 상자 그림을 만들어 이상치가 사라졌는지 확인하세요.
boxplot(mpg$cty)
boxplot(mpg$cty)$stats
mpg$cty<-ifelse(mpg$cty<9 | mpg$cty>26,NA,mpg$cty)
boxplot(mpg$cty)
#Q3. 이상치를 제외한 다음 drv별로 cty평균이 어떻게 다른지 알아보세요.
mpg%>%
  filter(!is.na(drv)&!is.na(cty))%>%
  group_by(drv)%>%
  summarise(avg_cty=mean(cty))
