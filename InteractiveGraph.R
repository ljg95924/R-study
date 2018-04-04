#12-1 plotly 패키지로 인터랙티브 그래프 만들기
#인터랙티브그래프: 마우스 움직임에 반응하며 실시간으로 형태가 변하는 그래프를 말한다.
#인터랙티브 그래프 만들기
#1. 패키지 준비하기
install.packages('plotly')
library(plotly)
#2. ggplot2로 그래프 만들기
library(ggplot2)
p<-ggplot(data=mpg,aes(x=displ,y=hwy,col=drv))+geom_point()
p
#3. 인터랙티브 그래프 만들기
ggplotly(p)
#4. 인터랙티브 막대 그래프 만들기
p<-ggplot(data=diamonds,aes(x=cut,fill=clarity))+geom_bar(position='dodge')
ggplotly(p)
#12-2 dygraphs 패키지로 인터랙티브 시계열 그래프 만들기
#인터랙티브 시계열 그래프 만들기
#1. dygraphs 패키지설치
install.packages('dygraphs')
library(dygraphs)
#2. economics 데이터 불러오기
economics<-ggplot2::economics
head(economics)
#3. 데이터를 시간 순서속성으로 만들기
library(xts) #시간순서 속성을 지니는 xts 데이터 타입
eco<-xts(economics$unemploy,order.by=economics$date)
head(eco)
#4. 인터랙티브 시계열 그래프 만들기
dygraph(eco)
#5. 그래프 아래에 날짜 범위 선택 기능 추가
dygraph(eco)%>%dyRangeSelector()
#6. 여러 값 표현하기
#저축률과 같이 표현하기
eco_a<-xts(economics$psavert,order.by=economics$date)
#실업자 수와 같이 표현하기
eco_b<-xts(economics$unemploy/1000,order.by=economics$date)
#7. cbind()로 결합 및 변수 수정 
eco2<-cbind(eco_a,eco_b)#데이터 결합
colnames(eco2)<-c('psavert','unemploy') #변수명 변경(데이터 타입이 데이터프레임이 아닌 xts 타입이기때문에 rename() 사용 불가)
head(eco2)
#8. 그래프 만들기
dygraph(eco2)%>%dyRangeSelector()
