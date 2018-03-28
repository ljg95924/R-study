#install.packages('foreign')
library(foreign) #spss 파일 불러오기
library(dplyr) #전처리
library(ggplot2) #시각화
library(readxl) #엑셀파일 불러오기



#데이터 불러오기 (https://github.com/youngwoos/Doit_R/blob/master/Data/csv_exam.csv)
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
#성별 직업 빈도 분석하기
#1. 성별 직업 빈도표 만들기
job_male<-welfare%>%
  filter(!is.na(job)&sex=='male')%>%
  group_by(job)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(10)
job_female<-welfare%>%
  filter(!is.na(job)&sex=='female')%>%
  group_by(job)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(10)
ggplot(data=job_male,aes(x=reorder(job,n),y=n))+geom_col()+coord_flip()
ggplot(data=job_female,aes(x=reorder(job,n),y=n))+geom_col()+coord_flip()

#9-8 종교 유무에 따른 이혼율
#종교 변수 검토 및 전처리하기
#1. 변수 검토하기
class(welfare$religion)
table(welfare$religion)
#2. 전처리
welfare$religion<-ifelse(welfare$religion==1,'yes','no')
table(welfare$religion)
qplot(welfare$religion)

#혼인 상태 변수 검토 및 전처리하기
#1. 변수 검토하기
class(welfare$marriage)
table(welfare$marriage)
#2. 이혼여부 파생변수 만들기 
welfare$group_marriage<-ifelse(welfare$marriage==1,'marriage', #marriage=유배우
                               ifelse(welfare$marriage==3,'divorce',NA)) #divorce=이혼
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

#종교 유무에 따른 이혼율 표 만들기
religion_marriage<-welfare%>%
  filter(!is.na(group_marriage))%>%
  group_by(religion,group_marriage)%>%
  summarise(n=n())%>%
  mutate(tot_group=sum(n))%>%
  mutate(pct=round(n/tot_group*100,1))
religion_marriage

religion_marriage<-welfare%>%
  filter(!is.na(group_marriage))%>%
  count(religion,group_marriage)%>% #count=집단별 빈도 구하는 함수
  group_by(religion)%>%
  mutate(pct=round(n/sum(n)*100,1))
religion_marriage

divorce<-religion_marriage%>%
  filter(group_marriage=='divorce')%>%
  select(religion,pct)
divorce
ggplot(data=divorce,aes(x=religion,y=pct))+geom_col()

#연령대 및 종교 유무에 따른 이혼율 분석하기
#1. 연령대별 이혼율 표 만들기
ageg_marriage<-welfare%>%
  filter(!is.na(group_marriage))%>%
  group_by(ageg,group_marriage)%>%
  summarise(n=n())%>%
  mutate(tot_group=sum(n))%>%
  mutate(pct=round(n/tot_group*100,1))
ageg_marriage
ageg_marriage<-welfare%>%
  filter(!is.na(group_marriage))%>%
  count(ageg,group_marriage)%>%
  group_by(ageg)%>%
  mutate(pct=round(n/sum(n)*100,1))
#2. 연령대별 이혼율 그래프 만들기
#초년 제외 , 이혼 추출
ageg_divorce<-ageg_marriage%>%
  filter(ageg != 'young'&group_marriage=='divorce')%>%
  select(ageg,pct)
ageg_divorce
#그래프 만들기
ggplot(data=ageg_divorce, aes(x=ageg,y=pct))+geom_col()
#3. 연령대 및 종교 유무에 따른 이혼율 표 만들기
#연령대, 종교유무,결혼상태별 비율표 만들기
ageg_religion_marriage<-welfare%>%
  filter(!is.na(group_marriage)&ageg !='young')%>%
  group_by(ageg,religion,group_marriage)%>%
  summarise(n=n())%>%
  mutate(tot_group=sum(n))%>%
  mutate(pct=round(n/tot_group*100,1))
ageg_religion_marriage
#연령대 및 종교 유무별 이혼율 표 만들기
df_divorce<-ageg_religion_marriage%>%
  filter(group_marriage=='divorce')%>%
  select(ageg,religion,pct)
df_divorce
#4. 연령대 및 종교 유무에 따른 이혼율 그래프 만들기
ggplot(data=df_divorce,aes(x=ageg,y=pct,fill=religion))+geom_col(position = 'dodge')
#9-9 지역별 연령대 비율
#지역 변수 검토 및 전처리하기
#1. 변수 검토하기
class(welfare$code_region)
table(welfare$code_region)
#2. 전처리 (1.서울 2. 수도권 3. 부산/경남/울산 4. 대구/경북 5. 대전/충남 6. 강원/충북 7. 광주/전남/전북/제주도)
#지역 코드 목록 만들기
list_region<-data.frame(code_region=c(1:7),
                        region=c('서울',
                                 '수도권(인천/경기)',
                                 '부산/경남/울산',
                                 '대구/경북',
                                 '대전/충남',
                                 '강원/충북',
                                 '광주/전남/전북/제주도'))
list_region
#지역명 변수 추가
welfare<-left_join(welfare,list_region,id='code_region')
welfare%>%
  select(code_region,region)%>%
  head
#지역별 연령대 비율 분석하기
#1. 지역별 연령대 비율표 만들기
region_ageg<-welfare%>%
  group_by(region,ageg)%>%
  summarise(n=n())%>%
  mutate(tot_group=sum(n))%>%
  mutate(pct=round(n/tot_group*100,2))
region_ageg
#2. 그래프 만들기
ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg))+geom_col()+coord_flip()
#3. 노년층 비율 높은 순으로 막대 정렬하기
list_order_old<-region_ageg%>%
  filter(ageg=='old')%>%
  arrange(pct)
list_order_old
#지역명 순서 변경 만들기
order<-list_order_old$region
order
ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg))+geom_col()+coord_flip()+scale_x_discrete(limits=order)
#4. 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)
levels(region_ageg$ageg)

region_ageg$ageg<-factor(region_ageg$ageg,
                         level=c('old','middle','young'))
class(region_ageg$ageg)
levels(region_ageg$ageg)
ggplot(data=region_ageg,aes(x=region,y=pct,fill=ageg))+geom_col()+coord_flip()+scale_x_discrete(limits=order)
