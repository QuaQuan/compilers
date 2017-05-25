
cchar= as.character('"')

programNum=2 #第几个程序

for(loop in 1:10){  #需要修改共几个版本
  tmp1=sprintf("%s%s%s%d%s%d%s%s","setwd(",cchar,"F:/experience/f",programNum,"v",loop,cchar,")")
  eval(parse(text=tmp1))
  
  tmp2=sprintf("%s%s%s%d%s%s%s%s","data <- read.csv(",cchar,"./FormatResult_v",loop,".csv",cchar,",","header=FALSE)")
  eval(parse(text=tmp2))
  
  #----------将原始数据根据class划分为0/1——————----------------------------——————#
  #-----每个程序需要根据函数数量改变—————————#
  colnames(data) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
                      "a13","a14","a15","a16","a17","a18","a19","class")
  tjdata1 <-subset(data,class=="1")
  tjdata0 <-subset(data,class=="0")
  n1<- nrow(tjdata1)
  n0<- nrow(tjdata0)
  nFun<- ncol(tjdata1)-1
  
  
  #----------计算tarantula算法的嫌疑度——————-------------------------------——————#
  tarantula <-array(1:nFun)
  ochiai<-array(1:nFun)
  zoltar<-array(1:nFun)
  kulczynski1<-array(1:nFun)
  kulczynski2<-array(1:nFun)
  wong<-array(1:nFun)
  wong1<-array(1:nFun)
  wong2<-array(1:nFun)
  tandg<-array(1:nFun)
  
  a <-array(1:nFun) #失败用例出现的次数
  a2 <-0 #总次数
  b <-array(1:nFun) #成功用例出现的次数
  b2 <-0 #总次数
  for(k in 1:nFun){
    a[k]=0
    b[k]=0
    tarantula[k]=0
    ochiai[k]=0
    zoltar[k]=0
    kulczynski1[k]=0
    kulczynski2[k]=0
    wong[k]=0
    wong1[k]=0
    wong2[k]=0
    tandg[k]=0
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
  
  out<-array(1:10*nFun,dim=c(nFun,10))
  for(k in 1:nFun){
    tarantula[k]=(a[k]/a2)/(a[k]/a2+b[k]/b2)
    ochiai[k]=(a[k]/sqrt(a2*(a[k]+b[k])))
    zoltar[k]=a[k]/(a2+b[k]+10000*(a2-a[k])*b[k]/a[k])
    kulczynski1[k]=a[k]/((a2-a[k])+b[k])
    kulczynski2[k]=0.5*(a[k]/a2+a[k]/(a[k]+b[k]))
    if(b[k]<=2){
      wong[k]=a[k]-b[k]
    }else if(b[k]<=10){
      wong[k]=a[k]-(2+0.1*(b[k]-2))
    }else{
      wong[k]=a[k]-(2.8+0.001*(b[k]-10))
    }
    wong1[k]=a[k]
    wong2[k]=a[k]-b[k]
    if(a[k]!=0){
      tandg[k]=a[k]*(1+a[k])/2*b2/a2-b[k]
    }else{
      tandg[k]=(a2-a[k])-b[k]
    }
    
    out[k,1]=k
    out[k,2]=tarantula[k]
    out[k,3]=ochiai[k]
    out[k,4]=zoltar[k]
    out[k,5]=kulczynski1[k]
    out[k,6]=kulczynski2[k]
    out[k,7]=wong[k]
    out[k,8]=wong1[k]
    out[k,9]=wong2[k]
    out[k,10]=tandg[k]
  }
  write.csv(out, "./tarantula.csv", row.names = FALSE)
}
