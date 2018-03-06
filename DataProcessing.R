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
