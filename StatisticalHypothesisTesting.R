#13-2 t검정 -두 집단의 평균 비교
#t검정: 두 집단의 평균에 통계적으로 유의한 차이가 있는지 알아볼 때 사용하는 통계분석기법
#compact 자동차와 suv자동차의 도시 연비 t검정
#1. 데이터 불러오기
mpg<-as.data.frame(ggplot2::mpg)
library(dplyr)
mpg_diff<-mpg%>%
  select(class,cty)%>%
  filter(class %in% c('compact','suv'))
head(mpg_diff)
table(mpg_diff$class)
#2. t.test()이용하기, 비교할 값:cty, 비교할 집단:class, var.equal=T :집단 간 분산이 같다고 가정 
t.test(data=mpg_diff,cty~class,var.equal=T)
#p-value는 유의확률(=우연히 관찰된 확률)을 의미, 일반적으로 유의확률 5%를 판단 기준으로 삼는다. 5%미만이면 '집단 간 차이가 통계적으로 유의하다' 라고 해석
#sample estimates에서 suv보다 compact의 도시 연비가 더 높다고 보인다.

#일반 휘발류와 고급 휘발유의 도시 연비 t 검정
mpg_diff2<-mpg%>%
  select(fl,cty)%>%
  filter(fl %in% c('r','p')) #r:regular,p:premium
table(mpg_diff2$fl)
t.test(data=mpg_diff2,cty~fl,var.equal=T)

#13-3 상관분석- 두 변수의 관계성 분석
#상관분석:두 연속 변수가 서로 관련이 있는지 검정하는 통계 분석 기법
#상관분석을 통해 도출한 상관계수로 두 변수가 얼마나 관련되어 있는지, 관련성의 정도를 파악할수 있다.
#1에 가까울수록 관령성이 크다는 것을 의미, 양수면 정비례 음수면 반비례 관계
#실업자 수와 개인 소비 지출의 상관관계
economics<-as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)
#p-value가 0.05미만으로 통계적으로 유의하다.
#cor이 상관계수를 의미, 0.61으로 정비례관계
#상관행렬 히트맵 만들기
#1. cor()을 이용하여 상관행렬 만들기
head(mtcars)
car_cor<-cor(mtcars) #상관행렬 생성
round(car_cor,2)#소수점 셋째 자리에서 반올림해 출력
#2. corrplot 이용해 히트맵 만들기
install.packages('corrplot')
library(corrplot)
corrplot(car_cor)
#3. method 이용해 그래프 형태 바꾸기
corrplot(car_cor,method='number')
col<-colorRampPalette(c('#BB4444','#EE9988','#FFFFFF','#77AADD','#4477AA'))
corrplot(car_cor,
         method='color', #색깔로 표현
         col=col(200), #색상 200개 선정
         type='lower', #왼쪽 아래 행렬만 표시
         order='hclust', #유사한 상관계수끼리 군집화
         addCoef.col='black', #상관계수 색깔
         tl.col='black', #변수명 색깔
         tl.srt=45, #변수명 45도 기울임
         diag=F) #대각 행렬 제외
