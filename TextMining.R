#10-1 힙합 가사 텍스트 마이닝
#텍스트 마이닝 준비하기
#1. 패키지 준비하기 (java부터 설치 후에)
#install.packages('rJava')
#install.packages('memoise')
#install.packages('KoNLP')
library(KoNLP)
library(dplyr)
#2. 사전 설정하기
useNIADic()#98만개의 단어로 구성되어있음
#3. 데이터 준비하기 ( bit.ly/doit_rd 에서 hiphop.txt 다운)
#데이터 불러오기
txt<-readLines('hiphop.txt')
head(txt)
#4. 특수문자 제거하기
#install.packages('stringr')
library(stringr)
txt<-str_replace_all(txt,'\\W'," ") #\\W는 특수문자를 의미하는 정규표현식

#가장 많이 사용된 단어 알아보기
#1. 명사 추출하기
extractNoun('대한민국의 영토는 한반도와 그 부속도서로 한다')
#2. txt 파일에서 명사만 추출하기
#가사에서 명사 추출
nouns<-extractNoun(txt)
#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount<-table(unlist(nouns))
#데이터 프레임으로 변환
df_word<-as.data.frame(wordcount,stringsAsFactors = F)
#변수명 수정
df_word<-rename(df_word,
                word=Var1,
                freq=Freq)
#3. 자주 사용된 단어 빈도표 만들기
#두 글자 이상 단어 추출
df_word<-filter(df_word,nchar(word)>=2)
#4. 상위 20개 단어 추출
top_20<-df_word%>%
  arrange(desc(freq))%>%
  head(20)
top_20

#워드 클라우드 만들기
#1. 패키지 준비하기
#install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer)
#2. 단어 색상 목록 만들기
#Dark2 색상 목록에서 8개 색상 추출
pal<-brewer.pal(8,'Dark2')
#3. 난수 고정하기 (wordcloud는 함수를 실행할 때마다 난수를 이용해 매번 다른 모양의 워드클라우드를 만들어냄)
set.seed(1234)
#4. 워드 클라우드 만들기
wordcloud(words=df_word$word, #단어
          freq=df_word$freq,  #빈도
          min.freq=2, #최소단어빈도
          max.words=200, #표현 단어 수
          random.order = F, #고빈도 단어 중앙 배치
          rot.per=.1, #회전 단어 비율
          scale=c(4,0.3), #단어 크기 범위
          colors = pal) #색상목록
#5. 단어 색상 바꾸기
pal<-brewer.pal(9,'Blues')[5:9] #색상 목록 생성
set.seed(1234)#난수 고정
wordcloud(words=df_word$word, #색상 목록 생성
          freq=df_word$freq, #빈도
          min.freq=2, #최소단어빈도
          max.words=200, #표현 단어 수
          random.order = F, #고빈도 단어 중앙 배치
          rot.per=.1, #회전 단어 비율
          scale=c(4,0.3), #단어 크기 범위
          colors = pal) #색상목록

#10-2 국정원 트윗 텍스트 마이닝
#국정원 트윗 텍스트 마이닝
#1. 데이터 준비하기
#데이터 로드
twitter<-read.csv('twitter.csv',
                  header=T,
                  stringsAsFactors = F,
                  fileEncoding = 'UTF-8')
#변수명 수정
twitter<-rename(twitter,
                no=번호,
                id=계정이름,
                data=작성일,
                tw=내용)
#특수문자 제거
twitter$tw<-str_replace_all(twitter$tw,'\\W',' ')
head(twitter$tw)
#2. 단어 빈도표 만들기
#트윗에서 명사추출
nouns<-extractNoun(twitter$tw)
#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount<-table(unlist(nouns))
#데이터 프레임으로 변환
df_word<-as.data.frame(wordcount,stringsAsFactors = F)
#변수명수정
df_word<-rename(df_word,
                word=Var1,
                freq=Freq)
#3. 두글자 이상 데이터로 추출 및 상위20개 추출
#두글자 이상 단어만 추출
df_word<-filter(df_word,nchar(word)>=2)
#상위 20개만 추출
top20<-df_word%>%
  arrange(desc(freq))%>%
  head(20)
top20
#4. 단어 빈도 막대 그래프 만들기
library(ggplot2)
order<-arrange(top20,freq)$word #빈도 순서 변수 생성
ggplot(data=top20,aes(x=word,y=freq))+
  ylim(0,2500)+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limit=order)+ #빈도순 막대 정렬
  geom_text(aes(label=freq),hjust=-0.3) #빈도 표시
#5. 워드 클라우드 만들기
pal<-brewer.pal(8,'Dark2')
set.seed(1234)
wordcloud(words=df_word$word,
          freq=df_word$freq,
          min.freq=10,
          max.words = 200,
          random.order=F,
          rot.per=.1,
          scale=c(6,0.2),
          colors=pal)
