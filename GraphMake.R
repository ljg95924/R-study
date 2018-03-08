library(ggplot2)
#산점도 만들기

ggplot(data=mpg,aes(x=displ,y=hwy)) #배경 => x축은 displ, y축은 hwy로 지정
#geom_point() 산점도 추가
ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point() #배경에 산점도 추가
#xlim() = x축 범위 지정
ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point()+xlim(3,6)

#Q1. mpg 데이터의 cty와hwy간에 어떤 관계가 있는지 알아보려합니다. x축은 cty, y축은 hwy로 된 산점도를 만들어보세요.
ggplot(data=mpg,aes(x=cty,y=hwy))+geom_point()
#Q2. x축은 poptotal, y축은 popasian으로 된 산점도를 만들어보세요. 전체 인구는 50만명 이하, 아시아인 인구는 1만명 이하인 지역만 산점도에 표시되게 설정하세요.
ggplot(data=midwest,aes(x=poptotal,y=popasian))+geom_point()+xlim(0,500000)+ylim(0,10000)

#막대그래프-집단 간 차이 표현하기

#집단별 평균 hwy 만들기
library(dplyr)
df_mpg<-mpg%>%
  group_by(drv)%>%
  summarise(mean_hwy=mean(hwy))
df_mpg
#geom_col() = 막대 그래프 생성
ggplot(data=df_mpg,aes(x=drv,y=mean_hwy))+geom_col()
#크기 순으로 정렬, reorder() = 값의 크기 순으로 정렬할 수 있음, x축에 변수와 정렬기준으로 삼을 변수을 지정, -기호는 내림차순 정렬
ggplot(data=df_mpg,aes(x=reorder(drv,-mean_hwy),y=mean_hwy))+geom_col()

#빈도 막대 그래프 geom_bar()
ggplot(data=mpg,aes(x=drv))+geom_bar()
#x축에 연속 변수를 지정하면 값의 분포를 파악할수 있다.
ggplot(data=mpg,aes(x=hwy))+geom_bar()

#Q1. 'suv' 차종을 대상으로 평균 cty가 가장 높은 회사 다섯곳을 막대그래프로 표현해보세요. 막대는 연비가 높은 순으로 정렬하세요.
mp<-mpg%>%
  filter(class=='suv')%>%
  group_by(manufacturer)%>%
  summarise(avg_cty=mean(cty))%>%
  arrange(desc(avg_cty))%>%
  head(5)
ggplot(data=mp,aes(x=reorder(manufacturer,-avg_cty),y=avg_cty))+geom_col()
#Q2. 자동차 중에서 어떤 class가 가장 많은지 알아보려고 합니다. 자동차 종류별 빈도를 표현한 막대 그래프를 만들어 보세요.
ggplot(data=mpg,aes(x=class))+geom_bar()
