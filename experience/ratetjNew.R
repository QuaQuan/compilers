setwd("F:/experience")

data <-read.csv("./workrateNew.csv") #不需要查找的代码比例，越大越好

num =array(1:10)
for(i in 1:10) num[i]=0

for(i in 1:nrow(data)){
  a=data[i,1]
  if(a>=0.9){
    num[10]=num[10]+1
  } 
  if(a>=0.8){
    num[9]=num[9]+1
  } 
  if(a>=0.7){
    num[8]=num[8]+1
  } 
  if(a>=0.6){
    num[7]=num[7]+1
  } 
  if(a>=0.5){
    num[6]=num[6]+1
  } 
  if(a>=0.4){
    num[5]=num[5]+1
  } 
  if(a>=0.3){
    num[4]=num[4]+1
  } 
  if(a>=0.2){
    num[3]=num[3]+1
  } 
  if(a>=0.1){
    num[2]=num[2]+1
  } 
  if(a>=0){
    num[1]=num[1]+1
  }
}


num2 =array(1:10)
for(i in 1:10) num2[i]=0

for(i in 1:nrow(data)){
  a=data[i,2]
  if(a>=0.9){
    num2[10]=num2[10]+1
  } 
  if(a>=0.8){
    num2[9]=num2[9]+1
  } 
  if(a>=0.7){
    num2[8]=num2[8]+1
  } 
  if(a>=0.6){
    num2[7]=num2[7]+1
  } 
  if(a>=0.5){
    num2[6]=num2[6]+1
  } 
  if(a>=0.4){
    num2[5]=num2[5]+1
  } 
  if(a>=0.3){
    num2[4]=num2[4]+1
  } 
  if(a>=0.2){
    num2[3]=num2[3]+1
  } 
  if(a>=0.1){
    num2[2]=num2[2]+1
  } 
  if(a>=0){
    num2[1]=num2[1]+1
  }
}

out=array(1:20,dim=c(2,10))

for(i in 1:10){
  out[1,i]=num[11-i]/nrow(data)
  out[2,i]=num2[11-i]/nrow(data)
} 

colnames(out)=c("0.9~1","0.8~0.9","0.7~0.8","0.6~0.7","0.5~0.6","0.4~0.5","0.3~0.4","0.2~0.3","0.1~0.2","0~0.1")
rownames(out)=c("nnet","tarantula")
write.csv(out, "./ratetjNew.csv")