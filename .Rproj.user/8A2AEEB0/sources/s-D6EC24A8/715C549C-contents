#데이터 파악하기
exam<-read.csv('csv_exam.csv')
exam

head(exam) #앞에서부터 출력(default=6개)
tail(exam) #뒤에서부터 출력(6개)
dim(exam) #행,열 출력
str(exam) #데이터 속성 확인하기
summary(exam) #요약 통계량 산출하기

library(ggplot2)

mpg<-as.data.frame(ggplot2::mpg) #mpg 데이터를 데이터 프레임 형식으로 불러오기
str(mpg)
summary(mpg)

#변수명 변경
install.packages('dplyr') #데이터를 원하는 형태로 가공할 때 사용하는 패키지
library(dplyr)
df_raw<-data.frame(var1=c(1,2,1),var2=c(2,3,2))
df_raw
df_new<-df_raw
df_new
df_new<-rename(df_new,v2=var2)
df_new

#파생변수 만들기
df<-data.frame(var1=c(4,3,8),var2=c(2,6,1))
df
df$var_sum<-df$var1+df$var2 #파생변수 생성
df

mpg$total<-(mpg$cty+mpg$hwy)/2 #통합연비변수 생성
head(mpg)
summary(mpg$total)
hist(mpg$total) #히스토그램 생성

mpg$test<-ifelse(mpg$total >=20,'pass','fail')
head(mpg,20)

table(mpg$test)

library(ggplot2)
qplot(mpg$test)

mpg$grade<-ifelse(mpg$total>=30,'A',ifelse(mpg$total>=20,'B','C')) #연비에 따른 등급설정
head(mpg,20)
table(mpg$grade) #빈도표 출력 
qplot(mpg$grade) #막대그래프 생성

#Q1. ggplot2의 midwest 데이터를 데이터 프레임 형태로 불러온 다음 데이터의 특징을 파악하세요
mw<-(as.data.frame(midwest))
#Q2. poptotal(전체인구)변수를 total로, popasian(아시아인구)변수를 asian으로 수정하세요.
mw<-rename(mw,total=poptotal,asian=popasian)
#Q3. total,asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수를 만들고, 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세요.
mw$AsianPercent<-mw$asian/mw$total
mw
hist(mw$AsianPercent)
#Q4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 'large',그 외에는 'small'을 부여하는 파생변수를 만들어 보세요.
avg<-mean(mw$AsianPercent)
mw$grade<-ifelse(mw$AsianPercent>=avg,'large','small')
mw
#Q5. 'large'와'small'에 해당하는 지역이 얼마나 되는지 빈도표와 빈도 막대 그래프를 만들어 확인하세요.
table(mw$grade)
qplot(mw$grade)
