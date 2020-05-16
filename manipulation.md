mydata = read.csv("https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv") ##data import
mydata
library(dplyr) ##working through dplyr
sample_n(mydata,4) ##selecting random N rows
sample_frac(mydata,.1)  ## we see the 10%  row of thr data
x1<-distinct(mydata)#remove duplicate rows based on variable
x2<- distinct(mydata,Index,Y2010,keep_all=T)##removwe duplicates based on multiple variable
###select()
##it is used to only desired variable
mydata2<-select(mydata,Index,State:Y2008)##selecting variable(or coloumns)
mydata22<-select(mydata,-c(Index,State))#dropping variable
##starts_with()
mydata3<-select(mydata,starts_with("Y"))##select var with starts with alphabet
mydata33<-select(mydata,-starts_with("y"))#dropping var with starts with alphabet

##following func helps you to select the var based on names 
#each one hold just one string
#starts_with()......starts with a prefix
#ends_with().....ends with a prefix
#contains().....contains a literal string
#matches().....matchea  a regular expression
#num_range()....numerical range like x01,x02,x03
#one_of().....variables in charecter vector
#everything()......all variable
mydata33<-select(mydata,num_range("2",1:5))
mydata4<-select(mydata,contains("I"))
##reorder variables
mydata5<-select(mydata,State,everything())##state in the front then other varibale
######rename variables   #it change variable name ##data class is data frame
mydata6<-rename(mydata,arafat=Index,emon=State)
###filter () function
##it is used to subset data with matching logical condition  ##this data class is data frame
##filter rows
mydata7<-filter(mydata, Index=="A")
mydata7<-filter(mydata, Y2002>="1395149",Index=="M")
##multiple selection criteria 
#the %in% operator can be used to select multiple items.
mydata77<-filter(mydata,Index %in% c("A","C"))
mydata77<-filter(mydata,Index %in% c("A","C"))
##AND condition in selection criteria
mydata8<-filter(mydata,Index %in% c("A","C") & Y2002>=1300000 & Y2009>=1900000)
#OR condition in selection criteria #it denoted by '|'
mydata9<-filter(mydata,Index %in% c("A","C")| Y2002>=1300000 | Y2009>=1900000)
#NOT condition #it's denoted by "!"
mydata10<-filter(mydata,!Index %in% c("A","C"))
#contains condition #grepl function is used to search pattern matching
mydata10<- filter(mydata,grepl ("ar",State))
##summarise ()function
#summarize selected variables 
summarise(mydata,Y2015_mean=mean(Y2015),
          Y2015_med=median(Y2015))
##summarize multiple variables 
summarise_at(mydata,vars(Y2005:Y2009),funs(n(),mean,median))
summarise_at(mydata,vars(Y2005,Y2006),list(n=~n(),~mean(.),median=median))
summarise_at(mydata,vars(Y2005,Y2006),list(n=~n(),missing=~sum(is.na(.)),~mean(.,na.rm = T),median=median))  #dot(.) is use for calculate each variable
##non standard functions
#we are going to subtract mean from its original value and then calculate variance of it
set.seed(22)
lol<-data.frame(X1=sample(1:100,100),X2=runif(100))
sample(1:5,3)
summarise_at(lol,vars(X1,X2),function(x)var(x-mean(x)))   
#equivalent method
summarise_at(lol,vars(X1,X2),~var(.-mean(.))) #each observation is denoted by dot(.)
##summarise_if()  #it's allow to summarise conditionally
summarise_if(lol,is.numeric,funs(n(),mean,median))
#summarise factor variable
summarise_all(mydata["Index"],funs(nlevels(.),nmiss=sum(is.na(.))))
##arrange()function  #use for sort data
arrange(mydata,desc(Index),Y2011) #must see the result 
##pipe operator (%>%) #dplyr utilize pipe operator from magrittr package
#pipe operator is use to wrap multiple function together
#it can be used with any function
dt<-mydata%>%select(Index,State)%>%sample_n(10)
##group_by() function  #group data by categorical variable
dt<-mydata%>% group_by(Index)%>%summarise_at(vars(Y2004:Y2006),funs(mean(.,na.rm = T),median))
##do() function #compute within groups
#the dot(.) is required to refer to a data frame
#we need to pull top 2 rows from A,C,I categories of variable index
dt1<-mydata%>%filter(Index%in%c("A","C","I"))%>%group_by(Index)%>%do(head(.,2))
dt1<-mydata%>%filter(Index%in%c("A","C","I"))%>%do(head(.,2)) ##compared with above one
##slice() function  #select rows by position
dt2<-mydata%>%select(Index,Y2015)%>%filter(Index%in% c("A","C","I"))%>%group_by(Index)%>% do(arrange(.,desc(Y2015)))%>%slice(3)
#using windows function  #min_rank () function
#it's alternate the above function
dt3<-mydata%>%select(Index,Y2015)%>%filter(Index%in% c("A","C","I"))%>%group_by(Index)%>% filter(min_rank(desc(Y2015))==3)
##summarize,group and sort together
t<-mydata%>%group_by(Index)%>%summarise(mean_2014=mean(Y2014,na.rm = T),mean_2015=mean(Y2015,na.rm = T))%>%arrange(desc(mean_2015))
##mutate() function ##create new variable
#calculate division of y2015 by y2014 and named it "change"
ll<-mutate(mydata,change=Y2015/Y2014)
#multiply all the variables by 1000
lll<-mutate_all(mydata,funs("new"=.*1000))
#above process apply on only numeric variables
lll1<-mutate_at(mydata,vars(Y2008:Y2010),funs(Rank=min_rank(.)))
#select state that generate highest income among the variable "index"
out<-mydata%>%group_by(Index)%>%arrange(desc(Y2015)==1)%>%select(Index,State,Y2015) #compare with under programming
out<-mydata%>%group_by(Index)%>%filter(min_rank(desc(Y2015))==1)%>%select(Index,State,Y2015)
#cumulative income of index variable
out2<-mydata%>%group_by(Index)%>%mutate(Total=cumsum(Y2015))%>%select(Index,Y2015,Total)
#join() function #jojn two dataset 
#inner_join(x,y,by=)
#left_join(x,y,by=)
#right_join(x,y,by=)
#full_join(x,y,by=)
#semi_join(x,y,by=)
#anti_join(x,y,by=)
# x,y-datasets(or tables) to merge/join
#by-common variable to join by

#common rows in both the tables
df1<-data.frame(ID=c(1,2,3,4,5),
                w=c('a','b','c','d','e'),
                x=c(1,1,0,0,1),
                y=rnorm(5),
                z=letters[1:5])
df2<-data.frame(ID=c(1,7,4,7,9),
                a=c('s','w','p','q','r'),
                b=c(1,4,5,0,2),
                c=rnorm(5),
                d=letters[2:6])
df3<-inner_join(df1,df2,by="ID")
left_join(df1,df2,by="ID")
##applying INTERSECT
mtcars$model<-rownames(mtcars)
first<-mtcars[1:20,]
second<-mtcars[10:32,]
#intersect selects unique rows that are common to both the data frame
intersect(first,second)
#applying union
x<-data.frame(ID=1:6,Id1=1:6)
y<-data.frame(ID=1:6,Id1=1:6)
#union displays all rows from both tables and remove duplicate records from the combined data set
union(x,y)
#allow duplicat rows in the combined data set
union_all(x,y)
#rows appear in one table but not in the other table
setdiff(first,second)
#if else statement
#if_else(condition,true,false,missing=false)
dff<-c(-10,23,NA)
if_else(dff<0,"negative","positive",missing = "missing value")
#create a new variable  with if_else
dff1<-data.frame(x=c(1,5,6,NA))
dff1%>%mutate(newvar=if_else(x<5,x+1,x+2,0))
#nested if else
mydf<-data.frame(x=c(1:5,NA))
mydf%>%mutate(newvar=
                if_else(is.na(x),"i am missing",
                        if_else(x==1,"i am one",
                                if_else(x==2,"i am two","others"))))
#alernative
mydf%>%mutate(flag=
                case_when(is.na(x)~"i am missing",
                          x==1~"i am one",
                          x==3~"i am three",
                          x>=4~"greater than or equal four",
                          TRUE~"OTHERS"))
#apply row wise operation 
#maximum value un each row
dk<-mydata%>%rowwise()%>%mutate(Max=max(Y2015,Y2014,Y2013,Y2012))%>%select(Y2012:Y2015,Max)
#combine data frames
ndata<-data.frame(ID=1:6,x=letters[1:6])
ndata1<-data.frame(ID=7:12,x=letters[7:12])
xy<-bind_rows(ndata,ndata1)
#or
xy<-rbind(ndata,ndata1)
xy<-bind_cols(ndata,ndata1)
#calculate percentile value
mydata%>%group_by(Index)%>%summarise(Pecentile_25=quantile(Y2014,probs = .25))
#automate model building
#we are build linear regression model for each level of a categorical variable 
#..........
##if() family function
#select_if,mutate_if,summarise_if
#select only numeric columns
mydatt<-select_if(mydata,is.numeric)
#factor column
mydatt1<-select_if(mydata,is.factor)
#number of levels in factor variables
summarise_if(mydata,is.factor,funs(nlevels(.)))
#multiply by 1000 to numeric variable
mydta11<-mutate_if(mydata,is.numeric,funs("new"=.*1000))
#convert value to NA
k<-c("a","b","","d")
na_if(k,"")
#use pull () function
iris%>%pull(Sepal.Length)
#equivalent
iris$Sepal.Length
iris%>%filter(Sepal.Length>5.5)%>%pull(Species)
iris
devtools::install_github("tidyverse/dplyr")
