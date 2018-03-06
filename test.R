install.packages('ggplot2')#ggplot2 패키지를 설치
library(ggplot2)#ggplot2 패키지를 로드

x<-c('a','a','b','c')
x
qplot(x)#x변수의 빈도 막대 그래프 출력

qplot(data=mpg,x=hwy)#data에 mpg를 ,x축에 hwy 변수를 지정해 그래프를 생성
