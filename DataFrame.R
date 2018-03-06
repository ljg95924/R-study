#점수 생성
english<-c(90,80,60,70)
math<-c(50,60,100,20)
class<-c(1,1,2,2)
#english,math,class를 데이터 프레임으로 생성
df_midterm<-data.frame(english,math,class)
df_midterm
mean(df_midterm$english) #영어 평균

install.packages('readxl') #엑셀파일 불러오기위한 패키지
library(readxl)
#excel_exam.xlsx 파일 불러오기 , bit.ly/doit_ra 홈페이지에서 다운로드 
df_exam<-read_excel('excel_exam.xlsx') 
df_exam

#데이터 분석
mean(df_exam$english)#영어 평균값 출력

df_exam_novar<-read_excel('excel_exam_novar.xlsx')
df_exam_novar

#첫번째행을 변수명이 아닌 데이터로 인식해서 불러옴
df_exam_novar<-read_excel('excel_exam_novar.xlsx',col_names = F)
df_exam_novar

#시트가 여러개있는 엑셀파일인 경우
df_exam_sheet<-read_excel('excel_exam_sheet.xlsx',sheet = 3)
df_exam_sheet

#csv 파일 불러오기, stringsAsFactors=F => 변수에 문자가 들어있을 때 문자타입으로 불러오기위해서 
df_csv_exam<-read.csv('csv_exam.csv')
df_csv_exam

#csv 파일로 저장
write.csv(df_midterm, file='df_midterm.csv')
