#15-1 R 내장 함수로 데이터 추출하기
#dplyr이 내장 함수에 비해 사용하기 쉽고 처리 속도도 빠르지만 다른 사용자들이 만든 코드를 이해하고 활용하려면 내장 함수를 사용하는 방법도 알아야한다.
#행번호로 행 추출하기
#1. 파일 불러오기
exam<-read.csv('csv_exam.csv')
#2. 전체 데이터 출력
exam[]
#3. 원하는 데이터 추출
exam[1,] #1행추출
exam[2,] #2행추출
#조건을 충족하는 행 추출하기
exam[exam$class==1,] #class가 1인 행 추
exam[exam$math>=80,]#수학점수가 80점 이상인 행 추출
#열 번호로 변수 추출하기
exam[,1]#1열 추출
exam[,2]#2열 추출
exam[,3]#3열 추출
#변수명으로 변수 추출하기
exam[,'class']#class 변수 추출
exam[,'math']
#c()사용
exam[,c('class','math','english')]
#행, 변수 동시 추출
exam[1,3] #행,변수 모두 인덱스
#행 인덱스, 열 변수명
exam[5,'english']
#행 부등호 조건, 열 변수명
exam[exam$math>=50,'english']
#행 부등호 조건, 열 변수명
exam[exam$math>=50,c('english','science')]
#15-2 변수 타입
#변수의 종류 1. 연속 변수 2. 범주 변수
#1. 연속 변수-Numeric 타입 : 키,몸무게,소득처럼 연속적이고 크기를 의미하는 값
#2. 범주 변수-Factor 타입 : 값이 대상을 분류하는 의미를 지니는 변수 ex)남자:1 , 여자:2
#변수 타입 간 차이 알아보기
#1. 변수 생성
var1<-c(1,2,3,1,2)
var2<-factor(c(1,2,3,1,2))
#2. 차이 보기
var1
var2
#3. factor 변수는 연산이 안된다
var1+2
var2+2
#변수 타입 바꾸기
var2<-as.numeric(var2)
var2
