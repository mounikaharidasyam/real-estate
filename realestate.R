#regression
#real estate project  to predict price

setwd("C:/Users/mouni/Documents/Data/Data")



ht_train=read.csv("housing_train.csv",stringsAsFactors = F)
colnames(ht_train)
ncol(ht_train)

ht_test= read.csv("housing_test.csv",stringsAsFactors = F)
colnames(ht_test)
ncol(ht_test)

table(ht_train$YearBuilt)
var(ht_all$Price)
var(ht_train$Price)

ht_test$Price=NA


ht_train$data='train'
ht_test$data='test'

ht_all=rbind(ht_train,ht_test)
View(ht_all)


library(dplyr)

glimpse(ht_all)

table(ht_all$Address)

table(ht_all$Suburb)

table(ht_all$Postcode)


table(ht_all$Type)

table(ht_all$Method)

table(ht_all$SellerG)

sort(round(tapply(ht_all$Price,ht_all$Type,mean,na.rm=T)))
#901936- 1294320 

sum(is.na(ht_test$YearBuilt))

sort(tapply(ht_all$Price,ht_all$CouncilArea,var,na.rm = T))


table(ht_all$CouncilArea)


lapply(ht_all,function(x) length(unique(x)))
ht_all$Address=NULL


         
glimpse(ht_all)

table(ht_all$Rooms)


mean(ht_all$Price,na.rm = T)

#combine categories on the basis of average response 
#round(tapply(ht_all$Price,ht_all$Suburb,mean,na.rm=T))

#round(tapply(ht_all$Price,ht_all$,mean,na.rm=T))


CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}





ht_all=CreateDummies(ht_all,"Suburb",100)
ht_all=CreateDummies(ht_all,"Type",100)
ht_all=CreateDummies(ht_all,"Method",100)
ht_all=CreateDummies(ht_all,"SellerG",100)
ht_all=CreateDummies(ht_all,"CouncilArea",100)



lapply(ht_all,function(x) sum(is.na(x)))

#missing values present in price bedroom2 bathroom2 car etc to eliminate this




for(col in names(ht_all)){
  
  if(sum(is.na(ht_all[,col]))>0 & !(col %in% c("data"))){
    
    ht_all[is.na(ht_all[,col]),col]=mean(ht_all[,col],na.rm=T)
  }
  
}



## separate train and test

ht_train=ht_all %>% filter(data=='train') %>% select(-data)
ht_test=ht_all %>% filter(data=='test') %>% select(-data,-Price)
ht_train



set.seed(2)
s=sample(1:nrow(ht_train),0.7*nrow(ht_train))
ht_train1=ht_train[s,]
ht_train2=ht_train[-s,]

  
  
fit=lm(Price ~ ., data = ht_train1)
fit



library(car)

#vif<5

sort(vif(fit),decreasing = T)


summary(fit)
#p<0.05


fit=step(fit)


formula(fit)

fit=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
         Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + 
           Suburb_Hampton + Suburb_Balwyn + 
          Suburb_Camberwell + Suburb_PortMelbourne + 
         Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
          Suburb_Kew + Suburb_Brighton + Suburb_Essendon + 
         Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
         Suburb_Reservoir + Type_u + Type_h + Method_PI + Method_S + 
         SellerG_Kay + SellerG_Miles +  SellerG_RT + 
         SellerG_Biggin +  SellerG_Marshall + SellerG_hockingstuart + 
         SellerG_Jellis + CouncilArea_Whitehorse + CouncilArea_HobsonsBay + 
          CouncilArea_Stonnington + CouncilArea_GlenEira + 
         CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Moreland + 
         CouncilArea_Boroondara + CouncilArea_
         ,data = ht_train1)

summary(fit)


val.pred=predict(fit,newdata=ht_train2)
val.pred



errors=ht_train2$Price-val.pred


errors**2 %>% mean() %>% sqrt()


212467/396917.2
212467/384861.2
212467/385203.2





fit.final=fit=lm(Price ~ .,data=ht_train)


fit.final=step(fit.final)

summary(fit.final)

test.pred=predict(fit.final,newdata=ht_test)

write.csv(test.pred,"mounika_haridasyam_P1_part2..csv",row.names = F)

plot(fit.final,1)

plot(fit.final,2)

plot(fit.final,3)

plot(fit.final,4)

library(ggplot2)
ht_train1 %>% 
  mutate(pred_IR=predict(fit,newdata=ht_train1)) %>% 
  ggplot(aes(x=Price,y=pred_IR))+geom_point(alpha=0.6)

#--------------

#var.test(ht_all$Price,ht_train$Price)
#table(ht_all$YearBuilt) 
#sum(is.na(ht_all$YearBuilt))
table(ht_all$Distance)

#1765

var(ht_all$Price)






         
------------------
  
  
 library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)

  
  
  ### Building A Deecison Tree
  
ht.tree=tree(Price~.,data=ht_train1)

## Tree in text format

ht.tree

## Visual Format

plot(ht.tree)
text(ht.tree)

## Performance on validation set
#make prediction use predict 

val.IR=predict(ht.tree,newdata = ht_train2)

rmse_val=((val.IR)-(ht_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val

212467/403259.3

## Making final model on entire training
## and prediction on test/production data

ht.tree.final=tree(Price~.,data=ht_train)
test.pred=predict(ht.tree.final,newdata=ht_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)

## Classification tree




