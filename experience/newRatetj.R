setwd("F:/experience")

data <-read.csv("./newWorkrate2.csv") #不需要查找的代码比例，越大越好

num =array(1:100,dim=c(10,10))
for(i in 1:10)
  for(j in 1:10){
    num[i,j]=0
    }

for(i in 1:nrow(data))
  for(j in 1:ncol(data))
    {
  a=data[i,j]
  if(a>=0.9){
    num[j,10]=num[j,10]+1
  } else
  if(a>=0.8){
    num[j,9]=num[j,9]+1
  } else
  if(a>=0.7){
    num[j,8]=num[j,8]+1
  } else
  if(a>=0.6){
    num[j,7]=num[j,7]+1
  } else
  if(a>=0.5){
    num[j,6]=num[j,6]+1
  } else
  if(a>=0.4){
    num[j,5]=num[j,5]+1
  } else
  if(a>=0.3){
    num[j,4]=num[j,4]+1
  } else
  if(a>=0.2){
    num[j,3]=num[j,3]+1
  } else
  if(a>=0.1){
    num[j,2]=num[j,2]+1
  } else
  if(a>=0){
    num[j,1]=num[j,1]+1
  }
}


for(i in 1:10)
  for(j in 1:10){
  num[i,j]=num[i,j]/nrow(data)
}

colnames(num)=c("0.9~1","0.8~0.9","0.7~0.8","0.6~0.7","0.5~0.6","0.4~0.5","0.3~0.4","0.2~0.3","0.1~0.2","0~0.1")
rownames(num)=c("nnet","tarantula","ochiai","zoltar","kulczynski1","kulczynski2","wong","wong1","wong2","tandg")
write.csv(num, "./newRatetj.csv")