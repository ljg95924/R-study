library(dplyr)
exam<-read.csv('csv_exam.csv')
exam
#조건에 맞는 데이터만 추출하기

#filter
exam%>%filter(class==1)#exam에서 class가 1인 경우만 추출해 출력 
exam%>%filter(class!=1)
exam%>%filter(math>50 & class==1)
exam%>%filter(class %in% c(1,3,5)) #class가 1,3,5인것만 추출

class1<-exam%>%filter(class==1)
class2<-exam%>%filter(class==2)
mean(class1$math)
mean(class2$math)

#Q1. displ(자동차 배기량)이 4이하인 자동차와 5이상인 자동차 중 어떤 자동차의 hwy(고속도로연비가)가 평균적으로 더 높은지 알아보세요
displ1<-mpg%>%filter(displ<=4)
displ2<-mpg%>%filter(displ>=5)
mean(displ1$hwy)
mean(displ2$hwy)
#Q2. 'audi'와'toyota'중 어느 manufacturer(자동차 제조 회사)의 cty(도시연비)가 평균적으로 더 높은지 알아보세요.
audi<-mpg%>%filter(manufacturer=='audi')
toyota<-mpg%>%filter(manufacturer=='toyota')
mean(audi$cty)
mean(toyota$cty)
#Q3. 'chevrolet','ford','honda' 자동차의 고속도로 연비 평균을 알아보려고 합니다. 이 회사들의 데이터를 추출한 후 hwy 전체 평균을 구해보세요
chevrolet<-mpg%>%filter(manufacturer %in% c('chevrolet','ford','honda'))
mean(chevrolet$hwy)

#필요한 변수만 추출하기

exam%>%select(english) #english만 추출
exam%>%select(class,math,english)# class,math,english 변수 추출
exam%>%select(-math) #math 제외
exam%>%filter(class==1)%>%select(english)
exam%>%
  select(id,math)%>% #id,math 만 출력
  head

#Q1. mpg 데이터는 11개 변수로 구성되어 있습니다. 이 중 일부만 추출해 분석에 활용하려고 합니다. mpg 데이터에서 class,cty 변수를 추출해 새로운 데이터를 만드세요.
mpg%>%select(class,cty)
#Q2. 자동차 종류에 따라 도시 연비가 다른지 알아보려고 합니다. 앞에서 추출한 데이터를 이용해 class(자동차 종류)가 'suv'인 자동차와 'compact'인 자동차 중 어떤 자동차의 cty가 더 높은지 알아보세요.
test1<-mpg%>%filter(class== 'suv')
test2<-mpg%>%filter(class=='compact')
mean(test1$cty)
mean(test2$cty)

#순서대로 정렬하기

exam%>%arrange(math)#math 오름차순 정렬
exam%>%arrange(desc(math))#내림차순 정렬
exam%>%arrange(class,math)#우선적으로 class 로 정렬 후 math기준으로 정렬

#Q1. 'audi'에서 생산한 자동차 중에 어떤 자동차 모델의 hwy가 높은지 알아보려고 합니다. 'audi'에서 생산한 자동차 중 hwy가 1~5위에 해당하는 자동차의 데이터를 출력하세요
mpg%>%filter(manufacturer=='audi')%>%arrange(desc(hwy))

#파생변수 추가하기

exam%>%mutate(total=math+english+science)%>%#total 변수 추가
  head
exam%>%mutate(total=math+english+science,
              mean=(math+english+science)/3)%>%
  head
exam%>%
  mutate(test=ifelse(science>=60,'pass','fail'))%>% 
  head
exam%>%
  mutate(total=math+english+science)%>%
  arrange(total)%>%
  head

#Q1. mpg 데이터 복사본을 만들고, cty와 hwy를 더한 '합산 연비 변수'를 추가하세요.
mpgdata<-mpg
mpgdata<-mpgdata%>%
  mutate(total_y=hwy+cty)
#Q2. 앞에서 만든 변수를 2로 나눠 '평균연비변수'를 추가하세요
mpgdata<-mpgdata%>%
  mutate(avg_y=total_y/2)
#Q3. '평균 연비 변수'가 가장 높은 자동차 3종의 데이터를 출력하세요.
mpgdata%>%
  arrange(desc(avg_y))%>%
  head
#Q4. 1~3번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 실행해보세요

#집단별로 요약하기

exam%>%summarise(mean_math=mean(math)) #math 평균 산출
exam%>%
  group_by(class)%>% #class 별로 분리
  summarise(mean_math=mean(math)) #math 평균 산출
#여러 요약 통계량 한번에 산출하기
exam%>%
  group_by(class)%>%
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math),
            n=n()) #행갯수(학생수)
#각 집단별로 다시 집단 나누기
mpg%>%
  group_by(manufacturer,drv)%>% #manufactuere로 먼저 분리 후 drv 으로 다시한번 분리 
  summarise(mean_cty=mean(cty))%>%
  head(10)
#회사별로 'suv'자동차의 도시 및 고속도로 통합 연비 평균을 구해 내림차순으로 정렬하고, 1~5위까지 출력하기
mpg%>%
  group_by(manufacturer)%>% #회사별로 분리
  filter(class=='suv')%>% #suv 자동차 추출
  mutate(tot=(cty+hwy)/2)%>% #통합 연비 변수 생성
  summarise(mean_tot=mean(tot))%>% #통합 연비 평균 구하기
  arrange(desc(mean_tot))%>% #내림차순 정렬
  head(5) #1~5까지 출력

#Q1. mpg 데이터의 class는 'suv','compact' 등 자동차를 톡징에 따라 일곱 종류 로 분류한 변수 입니다.
#어떤 차종의 도시 연비가 높은지 비교해보려고 합니다. class별 cty평균을 구해보세요.
mpg%>%
  group_by(class)%>%
  summarise(cty_avg=mean(cty))
#Q2. 어떤 차종의 도시 연비가 높은지 쉽게 알아볼 수 있도록 cty 평균이 높은 순으로 정렬해보세요.
mpg%>%
  group_by(class)%>%
  summarise(cty_avg=mean(cty))%>%
  arrange(desc(cty_avg))
#Q3. hwy 평균이 가장 높은 회사 세곳을 출력하세요.
mpg%>%
  group_by(class)%>%
  summarise(cty_avg=mean(cty))%>%
  arrange(desc(cty_avg))%>%
  head(3)
#Q4. 각 회사별 'compact'차종 수를 내림차순으로 정렬해 출력하세요.
mpg%>%
  group_by(manufacturer)%>%
  filter(class=='compact')%>%
  summarise(n=n())%>%
  arrange(desc(n))

#데이터 합치기

#가로로 합치기(by 를 기준으로)
test1<-data.frame(id=c(1,2,3,4,5),midterm=c(60,80,70,90,85)) #중간 고사 데이터 생성
test2<-data.frame(id=c(1,2,3,4,5),final=c(70,83,65,95,80)) #기말 고사 데이터 생성
total<-left_join(test1,test2,by='id') #id를 기준으로 합침
total

name<-data.frame(class=c(1,2,3,4,5),teacher=c('kim','lee','park','choi','jung'))
name
exam_new<-left_join(exam,name,by='class')
exam_new

#세로로 합치기  (조건 => 데이터 변수명이 일치해야한다. (id, test))
group_a<-data.frame(id=c(1,2,3,4,5),test=-c(60,80,70,90,85))
group_b<-data.frame(id=c(6,7,8,9,10),test=c(70,83,65,95,80))
group_all<-bind_rows(group_a,group_b) 
group_all

fuel<-data.frame(fl=c('c','d','e','p','r'),price_fl=c(2.35,2.38,2.11,2.76,2.22),stringsAsFactors=F)
fuel
#Q1. mpg 데이터에는 연료 종류를 나타낸 fl 변수는 있지만 연료가격을 나타낸 변수는 없습니다. 위에서 만든 fuel 데이터를 이용해 mpg 데이터에 price_fl(연료가격)변수를 추가하세요.
mpg
mpg<-left_join(mpg,fuel,by='fl')
#Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해 model,fl,price_fl 변수를 추출해 앞부분 5행을 출력해보세요.
mpg%>%
  select(model,fl,price_fl)%>%
  head(5)

#미국 동북중부 437개 지역의 인구통계를 담고있는 midwest 데이터를 사용해 데이터 분석 문제를 해결해 보세요. midwest는ggplot2패키지에 들어있습니다.
#Q1. popadults는 해당 지역의 성인 인구, poptotal 은 전체 인구를 나타냅니다. midwest 데이터에 '전체인구대비 미성년인구 백분율' 변수를 추가하세요.
mw<-midwest%>%
  mutate(minor_total_percent=(100-100*popadults/poptotal))
mw%>%select(minor_total_percent)
#Q2. 미성년 인구 백분율이 가장 높은 상위 5개 county의 미성년 인구 백분율을 출력하세요.
mw%>%
  select(county,minor_total_percent)%>%
  arrange(desc(minor_total_percent))%>%
  head(5)
#Q3. 분류표의 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의 지역이 있는지 알아보세요. (large= 40%이상, middle=30~40%, small= 30%미만)
mw$minor_total_grade<-ifelse(mw$minor_total_percent>=40,'large',ifelse(mw$minor_total_percent>=30,'middle','small'))
mw%>%select(minor_total_grade,minor_total_percent)
#Q4. popasian은 해당 지역의 아시아인 인구를 나타냅니다. '전체 인구 대비 아시아인 인구 백분율' 변수를 추가하고 하위 10개 지역의 state(주),county(지역),아시아인 인구 백분율을 출력하세요.
mw2<-mw%>%
  mutate(popasian_percent=100*popasian/poptotal)%>%
  arrange(popasian_percent)%>%
  head(10)%>%
  select(state,county,popasian_percent)
mw2