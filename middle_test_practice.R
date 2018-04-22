v<-c(10,20,30,40,50)
x<-c(1,'a',2,'b')
v
x
fruits<-c('apple','melon','banana','grape','tomato','potato')
sort(fruits,decreasing = T)
fruits[seq(2,6,by=2)]

b<-c(1:8,11:18,111:118)
dim(b)<-c(2,4,3)
x<-matrix(1:12,nrow=3)
t(x)
xr1<-x[1,]
xr1
xc3<-x[,3]
xc3
xs<-x[2:3,2:3]
xs

data('state')
state.x77[3,8]
state.x77[c(5,22,44),c(1,4,7)]
state.x77[-(5:49),3:5]
state.x77[,2>4000]
state.x77[state.x77[,'Income']>4000,]

data('iris')
head(iris)
iris[iris[,'Sepal.Width']>3.5,]
iris[iris[,'Species']=='versicolor',]
colnames(iris)
names(iris)
colnames(iris)[grep('width',names(iris),ignore.case=T)]
head(iris[,grep('Petal',names(iris))])

#3장
a<-c(1,2,NA,4)
a[!is.na(a)]
copy_i<-iris
output<-c()
copy_i
for (i in 1:length(copy_i$Sepal.Length)){
  if(copy_i$Sepal.Length[i]>5){
    output[i]<-'greater than 5'
  }
  else{
    output[i]<-'less than 5'
  }
}
output
output2<-c()
output2<-ifelse((copy_i$Sepal.Length)>5.0,copy_i$output22<-'greater than 5',copy_i$output22<-'less than 5')
copy_i
copy_i$Sepal.Length
func<-function(x){
  if(x['Sepal.Length']>5)
    "greater than 5"
  else
    "less than 5"
}
func2<-function(x){
  ifelse(x['Sepal.Length']>5,'greater than5','less than 5')
}
output4<-apply(copy_i,1,FUN = func2)
output4

which(iris$Species=='versicolor')
subset(iris,iris$Species=='versicolor')
iris[which(iris$Species=='versicolor'),]

a<-matrix(1:9,nrow=3)
b<-matrix(10:18,nrow=3)
cbind(a,b)
rbind(a,b)

m<-matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),
          nrow =30,ncol=3)
m
count_f<-function(x){
  if(x<0)
    cc<-cc+1
}
apply(m,2,function(x) length(x[x<0]))
apply(m,2,function(x) mean(x[x>0]))

library(lattice)
lapply(barley,function(x) unique(x))
lapply(barley,unique)
sapply(barley,unique)
sapply(barley,function(x) unique(x))
sapply(barley,function(x)length(unique(x)))

tapply(iris$Petal.Length,iris$Species,mean)

m<-matrix(1:8,
          ncol=2,
          dimnames = list(c('spring','summer','fial','winter'),
                          c('male','female')))
m
tapply(m,list(c(1,2,2,2,1,1,3,2),
              c(1,3,1,1,2,2,1,3)),sum) 
m[2,1]
list(c(1,1,2,2,1,1,2,2),
     c(1,1,1,1,2,2,2,2))

sex<-factor('m',c('m','f'))
sex
sex<-factor(c('m','f','m'))
sex
levels(sex)<-c('male','female')
ordered(c('a','b','c'))
factor(c('a','b','c'),ordered=T)

x<-c('a','b','c')
length(x)
nrow(x)
NROW(x)
colnames(x)<-c('kim','seo','park')
names(x)<-c('kim','seo','park')

setdiff(c('a','b','c'),c('a','d'))
union(c('a','b','c'),c('a','d'))
intersect(c('a','b','c'),c('a','d'))
identical(c(1,2,3),c(1,2,3))
seq(1,5)
seq(1,7,2)
x<-list(name='foo',height=70)
x
list(a=list(val=c(1,2,3)),b=list(val=c(1,2,3,4)))
x<-list(name='foo',height=c(1,3,5))
x[1]
x[2]

matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,byrow=T)
matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,
       dimnames = list(c('item1','itme2','item3'),
                       c('feature1','feature2','feature3')))
x<-matrix(c(1,2,3,4,5,6,7,8,9),nrow=3)
rownames(x)<-c('r1','r2','r3')
colnames(x)<-c('c1','c2','c3')
x

x[1:2,]
x[c(1,3),c(1,3)]
x[x[,'c2']>=5,]
x[x[,2]>=5,]
d<-data.frame(x=c(1,2,3,4,5),y=c(2,4,6,8,10))
d$x
d
d[,2]
d[,c('x','y')]
d[,c('x'),drop=F]
iris$Species<-as.character(iris$Species)
str(iris)

x<-data.frame(id=c(1,2),name=c('a','b'),stringsAsFactors = F)
str(x)

apply(iris[,1:4],2,sum)
colSums(iris[,1:4])

result<-lapply(1:3,function(x){x*2})
result
x<-list(a<-1:3,b<-4:6)
x
lapply(x,mean)
lapply(iris[,1:4],mean)

data.frame(do.call(cbind,lapply(iris[,1:4],mean)))

x<-sapply(iris[,1:4],mean)
x
as.data.frame(t(x))

y<-sapply(iris[,1:4],function(x){x>3})
class(y)
head(y)

tapply(1:10,rep(1,10),sum)
tapply(1:10,1:10%%2==1,sum)
tapply(iris$Sepal.Length,iris$Species,mean)
m<-matrix(1:8,
          ncol=2,
          dimnames=list(c('spring','summer','fail','winter'),
                        c('male','female')))
m
summary(iris)
order(iris$Sepal.Width)
iris[order(iris$Sepal.Width),]

split(iris,iris$Species)
lapply(split(iris$Sepal.Length,iris$Species),mean)
subset(iris,Species=='setosa')
subset(iris,select=c(Sepal.Length,Species))
x<-c(20,11,33,50,47)
sort(x,decreasing = T)
ordered(-x)
order(-x)
with(iris,{
  print(mean(Sepal.Length))
  print(mean(Sepal.Width))
})

x<-data.frame(val=c(1,2,3,4,NA,5,NA))
x
x<-within(x,{
  val<-ifelse(is.na(val),median(val,na.rm=T),val)
})
x
x$val[is.na(x$val)]<-median(x$val,na.rm=T)

Sepal.Width
attach(iris)
Sepal.Width
detach(iris)
Sepal.Width

x<-c(2,4,6,7,10)
x%%2
which(x%%2==0)
x[which(x%%2==0)]

which.min(x)
x[which.min(x)]
merge(x,y,all=T)
x<-c(1,1,3,4,7,6,7,8,9)
x
mean(x)
median(x)
install.packages('mlbench')
library(mlbench)
data(Ozone)
plot(Ozone$V8,Ozone$V9)
plot(Ozone$V8,Ozone$V9,xlab='sandburg Temp',ylab='EL Monte',main='Ozne',pch='=',xlim = c(10,100))
data(cars)
str(cars)
head(cars)
plot(cars)
plot(cars,type='l')
plot(cars,type='o')
tapply(cars$dist,cars$speed,mean)
plot(tapply(cars$dist,cars$speed,mean),type='o')
opar<-par(mfrow=c(1,2))
plot(Ozone$V8,Ozone$V9,xlab='sandburg T',ylab='EL Monte T',main='ozone')
plot(Ozone$V8,Ozone$V9,xlab='sandburg T',ylab='EL Monte T',main='ozone2')
plot(jitter(Ozone$V6),jitter(Ozone$V7))
par(opar)
?jitter
plot(iris$Sepal.Width,iris$Petal.Length)
points(iris$Petal.Width,iris$Petal.Length,cex=.5,pch='+',col='#FF0000')
with(iris,{
  plot(NULL,xlim=c(0,5),ylim=c(0,10),type='n')
  points(iris$Sepal.Width,iris$Sepal.Length,pch=20)
  points(iris$Sepal.Width,iris$Sepal.Length,pch='+',col='#FF0000')
  })
x<-seq(0,2*pi,0.1)
y<-sin(x)
plot(x,y,cex=.5,col='red')
lines(x,y)

plot(cars)
lines(lowess(cars))
plot(cars,xlim=c(0,25))
abline(a=-5,b=3.5,col='red')
plot(cars,xlim=c(0,25))
abline(a=-5,b=3.5,col='red')
abline(h=mean(cars$dist),lty=2,col='blue')
abline(v=mean(cars$speed),lty=2,col='green')

curve(sin,0,2*pi)
plot(iris$Sepal.Width,iris$Sepal.Length,cex=.5,pch=20,
     xlab='width',ylab='length')
points(iris$Petal.Width,iris$Petal.Length,cex=.5,pch='+',col='#FF0000')
legend('topright',legend=c('Sepal','petal'),
       pch=c(20,43),cex=0.8,col=c('black','red'),bg='gray')
boxplot(iris$Sepal.Width)
boxstats<-boxplot(iris$Sepal.Width)
boxstats

sv<-subset(iris,Species=='setosa' | Species=='versicolor')
str(sv)
head(sv)
sv$Species<-factor(sv$Species)
boxsta<-boxplot(Sepal.Width~Species,data=sv,notch=T)
boxsta

hist(iris$Sepal.Width)
hist(iris$Sepal.Width,freq=F)
x<-hist(iris$Sepal.Width,freq = F)
x

plot(density(iris$Sepal.Width))
hist(iris$Sepal.Width,freq=F)
lines(density(iris$Sepal.Width))
barplot(c(2,55,8,5,7,10,11,3,4,7,15))
barplot(tapply(iris$Sepal.Width,iris$Species,mean))
slices<-c(10,12,4,16,8)
pie(slices)

str(Titanic)
Titanic
mosaicplot(~ Class + Survived,data=Titanic,color=T)
?mosaicplot

x<-seq(-3,3,length=200)
plot(x,dnorm(x,mean=0,sd=1),type='l',
     main='Normal distribution,X~N(0,1)')
x<-seq(-3,3,length=200)
plot(x,pnorm(x,mean=0,sd=1),type='l')
r<-rnorm(1000000,0,1)
hist(r)

plot(density(rnorm(1000,0,10))) #rnorm(생성할 임의의 수의 갯수, 평균, 표준편차)
mean(1:5)
var(1:5)     
fivenum(1:11)
summary(1:11)

x<-factor(c('a','b','c','c','d','d'))
x
table(x)
which.max(table(x))
names(table(x))
sample(1:10,5)
sample(1:10,replace=T)

install.packages('sampling')
library(sampling)
x<-strata(c('Species'),size=c(3,3,3),method='srswor',
          data=iris)
x
getdata(iris,x)
strata(c('Species'),size=c(3,1,1),method='srswr',data=iris)

d<-data.frame(x=c('1','2','2','1'),
              y=c('A','B','A','B'),
              num=c(3,5,8,7))
d
xt<-xtabs(num~x+y,data=d)
xt

d2<-data.frame(x=c('A','A','A','B','B'),
               result=c(3,2,4,7,6))
xtabs(~x,d2)

cor(iris$Sepal.Width,iris$Sepal.Length)
cor(iris[,1:4])

install.packages('corrgram')
library(corrgram)
corrgram(cor(iris[,1:4]),type='cor',upper.panel = panel.conf)

cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method='pearson')


##-----------------2장----------------
v<-c(10,20,30,40,50)
v<-c(1,'a',2,'b')
v
fruits<-c('apple','melon','banana','grape','tomato','potato')
ordered(fruits)
sort(fruits)
fruits[seq(2,6,by=2)]

arr<-c(1:8,11:18,111:118)
dim(arr)<-c(2,4,3) ###
arr

x<-matrix(1:12,nrow=3)
t(x)
x[1,]
x[,3]
x[2:3,2:3]

data('state')
state.x77[3,8]
state.x77[c(5,22,44),c(1,4,7)]
str(state.x77)
state.x77[-(5:49),3:5]
state.x77[state.x77[,'Income']>4000,] ###
head(state.x77)
class(state.x77)
class(iris)
data('iris')
head(iris)
iris[iris$Sepal.Width>3.5,]
iris[iris[,'Sepal.Width']>3.5,]
iris[iris$Species=='versicolor',]
colnames(iris)
names(iris)
grep('width',names(iris),ignore.case=T)
names(iris)[grep('width',names(iris),ignore.case=T)] ###
?subset
?grep
head(iris[grep('Petal',names(iris))]) ###
##---------------3장
a<-c(1,2,NA,4)
ifelse(is.na(a),"",a)
a[!is.na(a)]

vec<-iris
length(vec[,1])
for(i in 1:length(vec[,1])){
  if(vec$Sepal.Length[i]>5){
    vec$output[i]='greater than 5'
  }
  else{
    vec$output[i]='less than 5'
  }
}
vec

ifelse(vec$Sepal.Length>5,'greater than 5','less than 5')

?apply
fuc<-function(x){
  if(x['Sepal.Length']>5) ####
    'greater than 5'
  else
    'less than 5'
}
apply(iris,1,FUN = fuc) ######

iris[iris$Species=='versicolor']
iris[iris[,'Species']=='vesicolor',]
iris$Species=='versicolor'     
grep('versicolor',iris$Species)###
which(iris$Species=='versicolor')###
?grep
?subset
iris[(which(iris$Species=='versicolor')),]###
subset(iris,iris$Species=='versicolor')###

A<-matrix(1:9,nrow=3)
A
B<-matrix(10:18,nrow=3)
B
cbind(A,B)
rbind(A,B)
##-----------------4장
m<-matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),
          nrow=30,ncol=3)
?apply
apply(m,2,function(x) length(x[x<0])) ###
apply(m,2,function(x) mean(x[x>0])) ###

library(lattice)
?lapply()
lapply(barley,unique)
sapply(barley,unique)
sapply(barley,function(x) length(unique(x)))
?sapply
?tapply
tapply(iris$Petal.Length,iris$Species,mean)
plot(data=iris,iris$Petal.Length,iris$Petal.Width,pch=21,bg=c('red','green','blue')[unclass(iris$Species)]) ###
?plot
barplot(tapply(iris$Petal.Length,iris$Species,mean))
?barplot
##---------------------7장
library(MASS)
data(Boston)
boston.sub<-names(Boston)
boston.sub
boston.sub<-Boston[c('lstat','indus','nox','rm','medv')]
head(Boston)
library(corrgram)
?corrgram
corrgram(cor(boston.sub),upper.panel=panel.conf)

par(mfrow=c(2,2)) #########
sapply(names(boston.sub),function(x){
  plot(boston.sub[,'lstat'],boston.sub[,'medv'],
       xlab='lstat',ylab='medv')
  plot(boston.sub[,'indus'],boston.sub[,'medv'],
       xlab='indus',ylab='medv')
  plot(boston.sub[,'nox'],boston.sub[,'medv'],
       xlab='nox',ylab='medv')
  plot(boston.sub[,'rm'],boston.sub[,'medv'],
       xlab='rm',ylab='medv')
  
  })
pairs(boston.sub) ##

cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method='pearson')
###--------1장
v<-c(10,20,30,40,50)
x<-c(1,'a',2,'b')
fruits<-c('apple','melon','banana','grape','tomato','potato')
sort(fruits)
sort(fruits,decreasing = T)
fruits[seq(2,6,by=2)]

arr<-c(1:8,11:18,111:118)
dim(arr)<-c(2,4,3)
arr

x<-matrix(1:12,nrow=3)
x
t(x)
xr1<-x[1,]
xc3<-x[,3]
xc3
xs<-x[2:3,2:3]
xs

data('state')
state.x77[3,8]
state.x77[c(5,22,44),c(1,4,7)]
state.x77[-(5:49),3:5]
state.x77[state.x77[,'Income']>4000,]

iris[iris$Sepal.Width>3.5,]
iris[iris$Species=='versicolor']
iris[iris[,'Species']=='versicolor',]

names(iris)
names(iris)[grep('width',names(iris),ignore.case = T)]
?grep
head(iris[grep('Petal',names(iris))])
##---------------------3장
temp<-(c(1,2,NA,4))
temp[!is.na(temp)]

iris_copy<-iris  
for(i in 1:length(iris_copy$Sepal.Length)){
  if(iris_copy$Sepal.Length[i]>5)
    iris_copy$size[i]<-'greater than 5'
  else
    iris_copy$size[i]<-'less than 5'
}
iris_copy
  ifelse(iris$Sepal.Length>5,'greater than 5','less than 5')
output<-apply(iris,1,function(x){
  if(x['Sepal.Length']>5)
    print(x['Sepal.Length'])
  else
    'less than 5'
})

?apply

which(iris$Species=='versicolor')
?subset
subset(iris,(iris$Species=='versicolor'))
A<-matrix(1:9,nrow=3)
B<-matrix(10:18,nrow=3)
cbind(A,B)
rbind(A,B)

m<-matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),
          nrow=30,ncol=3)
apply(m,2,function(x)length(x[x<0])) ###
apply(m,2,function(x)mean(x[x>0]))###
?apply

library(lattice)
?lapply()
lapply(barley, unique)
sapply(barley, function(x)unique(x))
sapply(barley,function(x)length(unique(x)))
tapply(iris$Petal.Length, iris$Species, mean)

plot(iris$Petal.Length,iris$Petal.Width)
plot(iris$Petal.Length,iris$Petal.Width)