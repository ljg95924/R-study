#결측치 정제하기

df<-data.frame(sex=c('M','F',NA,'M','F'),score=c(5,4,3,4,NA)) # <NA> = 결측치
df
#is.na() 결측치 확인
is.na(df) #TRUE = 결측치, FALSE=결측치X
#table(is.na()) 결측치 빈도 출력
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))

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

