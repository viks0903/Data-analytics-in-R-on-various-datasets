ipl <- read.csv('C:\\Users\\Vikas\\OneDrive\\Desktop\\Datasets\\IPL_Ball_by_Ball_2008_2022.csv\\IPL_Ball_by_Ball_2008_2022.csv')
ipl
View(ipl)


## Replacing NA with 0 in data frame ##

ipl2 <- read.csv('C:\\Users\\Vikas\\OneDrive\\Desktop\\Datasets\\IPL_Ball_by_Ball_2008_2022.csv\\IPL_Ball_by_Ball_2008_2022.csv')
library(dplyr)

## NA with all column to 0 ##

ipl2 <- ipl2 %>% replace(is.na(.),0)
View(ipl2)

## NA with 0 of particular variables ##

ipl3 <- read.csv('C:\\Users\\Vikas\\OneDrive\\Desktop\\Datasets\\IPL_Ball_by_Ball_2008_2022.csv\\IPL_Ball_by_Ball_2008_2022.csv')
ipl3
ipl4 <- ipl3["kind"][is.na(ipl3["kind"])] <- 0
ipl4


## data analysis ##

## first innings scores of batsman vs bowlers ## 

run <- ipl2[,c(1,5,6,9,13,17)]                      ## select columns ##
View(run)

virat <- run[run$batter == "V Kohli",]
View(virat)

out <- virat[virat$isWicketDelivery > 0, c(3,5)]
out
View(out)

agg <- aggregate(out,by=list(output=out$bowler),FUN = length)
View(agg)

agg1 <- agg[agg$isWicketDelivery > 1,]
View(agg1)

# histogram #
library(ggplot2)

ggplot(data = agg1, aes(y=output,x=isWicketDelivery)) + geom_point()
ggplot(data = agg1, aes(y=output,x=isWicketDelivery)) + geom_point(alpha=2)
ggplot(data = agg1, aes(y=output,x=isWicketDelivery)) + geom_point(alpha = 1, color = "purple")


ggplot(data = agg1, aes(y=output,x=isWicketDelivery)) + geom_boxplot()


ggplot() + geom_histogram(data = agg1, aes(x=isWicketDelivery),fill="blue",color= "black",bins=1,binwidth=0.5) 

## stacked histogram ##  
ggplot() + geom_histogram(data = agg1, aes(x=isWicketDelivery, fill = output), color = "black", bins = 1, binwidth = 0.5)

ggplot(agg1, aes(x=isWicketDelivery, y=bowler, fill=output)) + stat_summary(fun=mean,geom="bar")

plot(x=agg1$isWicketDelivery, y=agg1$bowler, type= 'p', col="red")

##   F du Plessis in both innings ##

faf <- ipl2[,c(2,5,6,9,12)]
View(faf)

faf1 <- faf[faf$innings == 1,]
View(faf1)

##grp_tbl<-faf1 %>% group_by(non_boundary)
agg1 <- faf1 %>% group_by(batter) %>% summarise(runs = sum(batsman_run))
View(agg1)

des <- agg1[order(-agg1$runs),]
View(des)

faf1run <- des[des$batter == "F du Plessis",]
faf1run
View(faf1run)

f1 <- faf1run %>% as.data.frame()
View(f1)
f1

faf2 <- faf[faf$innings == 2,]
View(faf2)
agg2 <- faf2 %>% group_by(batter) %>% summarise(runs = sum(batsman_run))
View(agg2)
des1 <- agg2[order(-agg2$runs),]
View(des1)

faf2run <- des1[des1$batter == "F du Plessis",]
faf2run

f2 <- faf2run %>% as.data.frame()
f2

fafboth <- cbind(f1,f2)
fafboth
View(fafboth)

colnames(fafboth) <- c('batsman','first','b','second')
View(fafboth)

fafboth1 <- fafboth[,-3]
View(fafboth1)

ggplot()
ggplot() + geom_histogram(data = fafboth1, aes(x=batsman,y=c(first,second)),fill="blue",color= "black",bins=1,binwidth=0.5)
ggplot(data = fafboth1, mapping = aes(y=batsman,x=first)) + geom_boxplot()

ggplot(data = fafboth1, aes(y=batsman,x=isWicketDelivery)) + geom_boxplot()


## bowler and their no of wickets ##

bowl <- ipl2[,c(6,13,15)]
View(bowl) 

wicket <- bowl[bowl$isWicketDelivery > 0,]
View(wicket)

wic <- bowl %>% group_by(bowler) %>% summarise(wicket=sum(isWicketDelivery))
wic
View(wic)

highwic <- wic[order(-wic$wicket),]
View(highwic)

top10bowler <- head(highwic,10)
View(top10bowler)

 # bar chart #
ggplot(top10bowler, aes(x=bowler, y=wicket)) + geom_bar(stat = "identity")


highrun <- run %>% group_by(batter) %>% summarise(totalruns=sum(batsman_run))
highrun <- highrun[order(-highrun$totalruns),]
top10batsman <- head(highrun,10)
View(top10batsman)

ggplot(top10batsman, aes(x=batter, y=totalruns)) + geom_bar(stat = "identity",col="black")
