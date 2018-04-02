#11-1미국 주별 강력 범죄율 단계 구분도 만들기
#미국 주별 강력 범죄율 단계 구분도 만들기
#1. 패키지 준비하기
install.packages('ggiraphExtra') #단계 구분도를 만들기 위한 패키지
library(ggiraphExtra)
#2. 미국 주별 범죄 데이터 준비하기
str(USArrests)
head(USArrests)
#3. 행이름이 없는 지역명 변수 추가
library(tibble) 
crime<-rownames_to_column(USArrests,var='state') #행이름 추가
head(crime)
crime$state<-tolower(crime$state) #소문자로
str(crime)
#4. 미국 주 지도 데이터 준비하기
library(ggplot2)
states_map<-map_data('state') #maps 패키지에 미국 주별 위경도를 나타낸 state데이터를 데이터 프레임 형태로 불러오기
str(states_map)
#5. 단계 구분도 만들기
ggChoropleth(data=crime, #지도에 표현할 데이터
             aes(fill=Murder, #색깔로 표현할 변수
                 map_id=state), #지역 구분 기준 변수
             map=states_map) #지도 데이터
#6. 인터랙티브 단게 구본도 만들기
ggChoropleth(data=crime, #지도에 표현할 데이터
             aes(fill=Murder, #색깔로 표현할 변수
                 map_id=state), #지역 구분 기준 변수
             map=states_map, #지도 데이터
             interactive = T)#인터렉티브(마우스 움직임에 반응하는 인터랙티브 단계 구분도)

#11-2 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기
#대한민국 시도별 인구 단계 구분도 만들기
#1. 패키지 준비하기
#install.packages('stringi') #kormaps2014 패키지를 이용하려고(??????)
install.packages('devtools') #install_github() 이용하려고
devtools::install_github('cardiomoon/kormaps2014')
library(kormaps2014)
#2. 대한민국 시도별 인구 데이터 준비하기
str(changeCode(korpop1)) #2015년 센서스 데이터(시도별)
head(korpop1)
#3. 변수명 영어로
library(dplyr)
korpop1<-rename(korpop1,
                pop=총인구_명,
                name=행정구역별_읍면동)
#4. 대한민국 시도 지도 데이터 준비하기
str(changeCode(kormap1)) #2014년 한국 행정 지도(시도별)
#5. 단계 구분도 만들기
ggChoropleth(data=korpop1, #지도에 표현할 데이터
             aes(fill=pop, #색깔에 표현할 변수
                 map_id=code, #지역 기준 변수
                 tooltip=name), #지도 위에 표시할 지역
             map=kormap1, #지도 데이터
             interactive = T, #인터랙티
             )
options(encoding='UTF-8')
#대한민국 시도별 결핵 환자 수 단계 구분도 만들기
str(changeCode(tbc))
ggChoropleth(data=tbc,
             aes(fill=NewPts,
                 map_id=code,
                 tooltip=name),
             map=kormap1,
             interactive = T)
