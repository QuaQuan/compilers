setwd("F:/experience")

cchar= as.character('"')

programNum=7 #第几个程序
tmp3=sprintf("%s%s%s%d%s%s%s%s","datap <-read.csv(",cchar,"./p",programNum,".csv",cchar,",","header=FALSE)")
eval(parse(text=tmp3))
tmp4=sprintf("%s%s%s%d%s%s%s%s","datae <-read.csv(",cchar,"./p",programNum,"error.csv",cchar,",","header=FALSE)")
eval(parse(text=tmp4))

for(loop in 1:23){  #需要修改共几个版本
  if(programNum==3&&loop==27){
    next
  }
  if(programNum==4&&(loop==1|loop==5|loop==6)){
    next
  }
  if(programNum==5&&loop==9){
    next
  }
  tmp1=sprintf("%s%s%s%d%s%d%s%s%s","data <-read.csv(",cchar,"./f",programNum,"v",loop,"/output_nnet.testData.csv",cchar,")")
  eval(parse(text=tmp1))
  tmp2=sprintf("%s%s%s%d%s%d%s%s%s","datat <-read.csv(",cchar,"./f",programNum,"v",loop,"/tarantula.csv",cchar,")")
  eval(parse(text=tmp2))
  
  wrong = datae[loop,1]
  if(wrong==-1){
    next
  }
  nFun<- nrow(data)
  
  #---------------处理tarantula,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,2]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为tarantula需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  tarantula=1-loct/num
  out <- read.csv("./newWorkrate2.csv")
  out[nrow(out)+1,2]=tarantula
  
  #---------------处理ochiai,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,3]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为ochiai需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  ochiai=1-loct/num
  out[nrow(out),3]=ochiai
  
  #---------------处理zoltar,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,4]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为ochiai需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  zoltar=1-loct/num
  out[nrow(out),4]=zoltar

  #---------------处理kulczynski1,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,5]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为ochiai需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  kulczynski1=1-loct/num
  out[nrow(out),5]=kulczynski1
  
  #---------------处理kulczynski2,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,6]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为ochiai需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  kulczynski2=1-loct/num
  out[nrow(out),6]=kulczynski2
  
  #---------------处理wong,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,7]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为ochiai需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  wong=1-loct/num
  out[nrow(out),7]=wong
  
  #---------------处理wong1,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,8]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为ochiai需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  wong1=1-loct/num
  out[nrow(out),8]=wong1
  
  #---------------处理wong2,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,9]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为ochiai需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  wong2=1-loct/num
  out[nrow(out),9]=wong2
  
  #---------------处理tandg,loct为需要查询的代码量----------------#
  t<-array(1:nFun)#第i个函数的输出为t[i]
  for(i in 1:nFun){
    t[i]=datat[i,10]
  }
  rankt<-array(1:nFun)#rankt[i]为嫌疑度从大到小的排名（rankt[i]是函数标号）
  rankt=order(-t)
  
  loct=0 #loct为ochiai需要查询的代码量
  for(i in 1:nFun){
    if(rankt[i]!=wrong){
      loct=loct+datap[rankt[i],2]
    }else{
      loct=loct+datap[wrong,2]
      break
    }
  }
  num=0 #num为所有函数的代码行数之和
  for(i in 1:nFun){
    num = num+datap[i,2]
  }
  tandg=1-loct/num
  out[nrow(out),10]=tandg
  
  write.csv(out,file="./newWorkrate2.csv",row.names = FALSE)
}
