
#----------每个版本需要修改，FormatResult_v1.csv文件放入文件夹中---------------#
setwd("F:/experience/f4v9")
data <- read.csv("./FormatResult_v9.csv",header=FALSE)


#----------将原始数据根据class划分为0/1——————----------------------------——————#
#-----每个程序需要根据函数数量改变—————————#
colnames(data) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
                    "a13","a14","a15","a16","a17","a18","class")
tjdata1 <-subset(data,class=="1")
tjdata0 <-subset(data,class=="0")
n1<- nrow(tjdata1)
n0<- nrow(tjdata0)
nFun<- ncol(tjdata1)-1


#----------计算tarantula算法的嫌疑度——————-------------------------------——————#
tarantula <-array(1:nFun)
a <-array(1:nFun) #失败用例出现的次数
a2 <-0 #总次数
b <-array(1:nFun) #成功用例出现的次数
b2 <-0 #总次数
for(k in 1:nFun){
  a[k]=0
  b[k]=0
  tarantula[k]=0
}
#fail
for (i in 1:n1){
  for(k in 1:nFun){
    a[k] =a[k]+tjdata1[i,k]
    a2 =a2+tjdata1[i,k]
  }
}
#pass
for (j in 1:n0){
  for(k in 1:nFun){
    b[k]=b[k]+tjdata0[j,k]
    b2=b2+tjdata0[j,k]
  }
}
out<-array(1:2*nFun,dim=c(nFun,2))
for(k in 1:nFun){
  tarantula[k]=(a[k]/a2)/(a[k]/a2+b[k]/b2)
  out[k,1]=k
  out[k,2]=tarantula[k]
}
write.csv(out, "./tarantula.csv", row.names = FALSE)


#----------计算每个错误用例有几个对应的hamming最小的正确用例以及对应的编号—-------—---------———#
minNum <- array(1:n1)
a <- array(1:n0)
min0 <- array(1:n1*n0,dim=c(n1,n0))
for (i in 1:n1){
  minNum[i] = 0
  for (j in 1:n0){
  min0[i,j] = 0
  }
}
for (i in 1:n1){
  min = 1000
  for (p in 1:n0){
    a[p]=0
  }
  #找到最小的hamming=min
  for (j in 1:n0){
    for(k in 1:nFun){
      if(tjdata1[i,k]>0)
        if( abs(tjdata1[i,k]-tjdata0[j,k])/tjdata1[i,k] >=0.2 ){
          a[j]=a[j]+1
        }
    }
    if(a[j]<min){
      min=a[j]
    }
  }
  for (j in 1:n0){
    if(a[j]==min){
      #min0[i,j]记录第i个错误用例对应的第j个hamming最小的正确用例的编号
      #minNum[i]记录每个错误用例有几个对应的hamming最小的正确用例
      minNum[i]=minNum[i]+1
      min0[i,minNum[i]]=j
    }
   }
}


#----------计算每个错误用例的hamming最小并且cos最大的正确用例以及对应的编号HamCos[i]——————-----------——————#
HamCos <- array(1:n1)
for (i in 1:n1){
  HamCos[i] = 0
}

for (i in 1:n1){
  maxCosine=0
  if(minNum[i]==1){
    HamCos[i]=min0[i,1]
  }
  else
  for (j in 1:minNum[i]){
    temp=sqrt(sum(tjdata1[i,]^2,na.rm = TRUE)*sum(tjdata0[minNum[j],]^2,na.rm = TRUE))
    if(temp>0)
    cosine2=sum(t(tjdata1[i,])*tjdata0[minNum[j],],na.rm = TRUE)/temp
    if(cosine2>=maxCosine){
      HamCos[i]=min0[i,j]
      maxCosine=cosine2
    }
  }
}


#----------计算tjData1[i,]和tjData0[HamCos[i],](每个错误用例和最相似的样本)，得到函数的系数——————-----------——————#
temp2 <- array(1:2*(nFun+1)*n1,dim=c(2*n1,nFun+1))
  for(j in 1:n1){
    temp2[j,nFun+1]=1;
    temp2[n1+j,nFun+1]=0;
    for(i in 1:nFun){
      temp2[j,i]=tjdata1[j,i]
      temp2[n1+j,i]=tjdata0[HamCos[j],i] 
    }
  }


temp <- array(1:2*nFun*n1,dim=c(nFun,2*n1))
for(i in 1:nFun){
  for(j in 1:n1){
    temp[i,j]=tjdata1[j,i]
    temp[i,n1+j]=tjdata0[HamCos[j],i]
  }
}

para<- array(1:nFun*(nFun+2),dim=c(nFun,nFun+2))
for(i in 1:nFun)
  for(j in 1:nFun){
    if(i==j){
      para[i,j]=1-sd(temp[i,])/10
    }else{
      para[i,j]=0.0
    }
  }
for(i in 1:nFun){
  para[i,nFun+1]=1
  para[i,nFun+2]=i
}
#--------------7个程序不同，需要修改----------------#
colnames(para) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
                    "a13","a14","a15","a16","a17","a18","nnet","function")
write.csv(para,file="./testData.csv",row.names = FALSE)
colnames(temp2) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
                    "a13","a14","a15","a16","a17","a18","class")
write.csv(temp2,file="./trainData.csv",row.names = FALSE)


#----------构造nnet模型，训练样本为原始表格（data），测试样本为testData.csv——————---——————#
trainData <- transform(temp2, class = as.factor(class))

library(nnet)
nnet.model <- nnet(class ~ ., trainData, size = 5,hidden=6,decay = 0.05)
summary(nnet.model)

predict <- predict(nnet.model, trainData, type = "class")
output_nnet.trainData <- cbind(trainData, predict)
colnames(output_nnet.trainData) <- c(colnames(trainData), "nnet")
write.csv(output_nnet.trainData, "./output_nnet.trainData.csv", row.names = FALSE)
save(nnet.model, file = "./nnet.model.RData")

testData <- read.csv("./testData.csv")
predict <- predict(nnet.model, testData, type = "raw")
output_nnet.testData <- cbind(testData, predict)
colnames(output_nnet.testData) <- c(colnames(testData), "nnet")
write.csv(output_nnet.testData, "./output_nnet.testData.csv",  row.names = FALSE)

