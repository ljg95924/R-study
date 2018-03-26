install.packages('foreign')
library(foreign) #spss 파일 불러오기
library(dplyr) #전처리
library(ggplot2) #시각화
library(readxl) #엑셀파일 불러오기
install.packages("reshape")
library(reshape)


#데이터 불러오기
raw_welfare<-read.spss(file ='C:\\Users\\CS3-10\\Downloads\\Koweps_hpc10_2015_beta1.sav',to.data.frame = T)
welfare<-raw_welfare

head(welfare)

#Koweps_codebook.xlsx 파일 참조해서 변수의 특성을 파악

welfare<-rename(welfare,
                sex=h10_g3, #성별
                birth=h10_g4, #태어난 연도
                marriage=h10_g10, #혼인 상태
                religion=h10_g11, #종교
                income=p1002_8aq1, #월급 
                code_job=h10_eco9, #직업 코드
                code_region=h10_reg7) #지역 코드


#9-2성별에 따른 월급차이

#1. 변수 검토
class(welfare$sex)
table(welfare$sex)
#2. 전처리
#이상치 결측 처리
welfare$sex<-ifelse(welfare$sex==9,NA,welfare$sex) #코드북에 무응답은 9로 되어잇음
#결측치 확인
table(is.na(welfare$sex))
#성별 항목 이름 부여
welfare$sex<-ifelse(welfare$sex==1, 'male','female')
table(welfare$sex)

qplot(welfare$sex)

#월급 변수 검토 및 전처리
#1. 변수 검토하기
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income)+xlim(0,1000)
#2. 전처리
#이상치 결측 처리
welfare$income<-ifelse(welfare$income %in% c(0,9999),NA,welfare$income) #코드북에 월급이 1~9998 사이에만 존재한다 말한다.
#결측치 확인
table(is.na(welfare$income))

#성별에 따른 월급차이 분석하기
#1. 성별 월급 평균표 만들기
sex_income<-welfare%>%
  filter(!is.na(income))%>%
  group_by(sex)%>%
  summarise(mean_income=mean(income))
sex_income

#2. 그래프 만들기
ggplot(data=sex_income,aes(x=sex,y=mean_income))+geom_col()

#9-3 나이와 월급의 관계
#나이 변수 검토하기
#1. 변수 검토하기
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
#2. 전처리
#이상치 확인
summary(welfare$birth)
#결측치 확인
table(is.na(welfare$birth))
#3. 나이 파생변수 만들기 
welfare$age<-2018-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

#나이와 월급의 관계 분석하기
#1. 나이에 따른 월급 평균표 만들기
age_income<-welfare%>%
  filter(!is.na(income))%>%
  group_by(age)%>%
  summarise(mean_income=mean(income))
head(age_income)
#2. 그래프 만들기
ggplot(data=age_income,aes(x=age,y=mean_income))+geom_line()

#9-4 연령대에 따른 월급차이
#연령대 파생변수 만들기
welfare<-welfare%>%
  mutate(ageg=ifelse(age<33,'young',
                     ifelse(age<=62,'middle','old')))
table(welfare$ageg)
#연령대에 따른 월급 차이 분석하기
#1.월급 평균표 만들기
ageg_income<-welfare%>%
  filter(!is.na(income))%>%
  group_by(ageg)%>%
  summarise(mean_income=mean(income))
ageg_income
#2. 그래프만들기
ggplot(data=ageg_income,aes(x=ageg,y=mean_income))+geom_col()
ggplot(data=ageg_income,aes(x=ageg,y=mean_income))+geom_col()+scale_x_discrete(limits=c('young','middle','old'))

#9-5 연령대 및 성별 월급 차이
#1. 연령대 및 성별 월급 평균표 만들기
sex_income<- welfare%>%
  filter(!is.na(income))%>%
  group_by(ageg,sex)%>%
  summarise(mean_income=mean(income))
sex_income
#2. 그래프 만들기
ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex))+geom_col()+scale_x_discrete(limits=c('young','middle','old'))
ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex))+geom_col(position='dodge')+scale_x_discrete(limits=c('young','middle','old'))
#나이 및 성별 월급 차이 분석하기
sex_age<-welfare%>%
  filter(!is.na(income))%>%
  group_by(age,sex)%>%
  summarise(mean_income=mean(income))
head(sex_age)
#그래프 만들기
ggplot(data=sex_age,aes(x=age,y=mean_income,col=sex))+geom_line()

#9-6 직업별 월급 차이
#직업 변수 검토 및 전처리하기
#1. 변수 검토하기
class(welfare$code_job) # code_job=직업코드
table(welfare$code_job)
#2. 전처리
list_job<-read_excel('C:\\Users\\CS3-10\\Downloads\\Koweps_Codebook.xlsx',col_names = T,sheet = 2)#직업 분류 코드 목록은 2번째 시트에 있음
head(list_job)
dim(list_job)
#3. job과welfare 을 join
welfare<-left_join(welfare,list_job,id='code_job')
welfare%>%
  filter(!is.na(code_job))%>%
  select(code_job,job)%>%
  head(10)
#직업별 월급 차이 분석하기
#1. 직업별 월급 평균표 만들기
job_income<-welfare%>%
  filter(!is.na(job)&!is.na(income))%>%
  group_by(job)%>%
  summarise(mean_income=mean(income))
head(job_income)
#2. 내림차순 정렬
top10<-job_income%>%
  arrange(desc(mean_income))%>%
  head(10)
top10
#3. 그래프 만들기
ggplot(data=top10,aes(x=reorder(job,mean_income),y=mean_income))+geom_col()+coord_flip()#coord_flip=막대 우로 90회전
#4. 월급 적은 직업
bottom10<-job_income%>%
  arrange(mean_income)%>%
  head(10)
ggplot(data=bottom10,aes(x=reorder(job,-mean_income),y=mean_income))+geom_col()+coord_flip()+ylim(0,850)

#9-7 성별 직업 빈도