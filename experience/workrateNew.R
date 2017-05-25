setwd("F:/experience")
cchar= as.character('"')

programNum=1 #-----------第几个程序--------#

tmp3=sprintf("%s%s%s%d%s%s%s%s","datap <-read.csv(",cchar,"./p",programNum,".csv",cchar,",","header=FALSE)")
eval(parse(text=tmp3))
tmp4=sprintf("%s%s%s%d%s%s%s%s","datae <-read.csv(",cchar,"./p",programNum,"error.csv",cchar,",","header=FALSE)")
eval(parse(text=tmp4))

# x.inv <- try(eval, silent=FALSE)
#   if ('try-error' %in% class(x.inv)) next

for(loop in 1:7){  #---------需要修改共几个版本---------------#
  tmp1=sprintf("%s%s%s%d%s%d%s%s%s","data <-read.csv(",cchar,"./f",programNum,"v",loop,"/output_nnet.testData_New.csv",cchar,")")
  eval(parse(text=tmp1))
  tmp2=sprintf("%s%s%s%d%s%d%s%s%s","datat <-read.csv(",cchar,"./f",programNum,"v",loop,"/tarantula.csv",cchar,")")
  eval(parse(text=tmp2))
  
wrong = datae[loop,1]
if(wrong==-1){
  next
}

#---------------处理nnet.output,locnn为nn需要查询的代码量----------------#
nFun<- nrow(data)
ynn<- ncol(data)#输出的坐标为[,ynn]

nn<-array(1:nFun)#第i个函数的输出为nn[i]
for(i in 1:nFun){
  nn[i]=data[i,ynn]
}
ranknn<-array(1:nFun)#ranknn[i]为嫌疑度从大到小的排名（ranknn[i]是函数标号）
ranknn=order(-nn)

locnn=0 #locnn为nn需要查询的代码量
for(i in 1:nFun){
  if(ranknn[i]!=wrong){
    locnn=locnn+datap[ranknn[i],2]
  }else{
    locnn=locnn+datap[wrong,2]
    break
  }
}

#---------------处理tarantula,loct为tarantula需要查询的代码量----------------#
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


#---------------处理p1.csv(最终结果),num为所有函数的代码行数之和----------------#
num=0 #num为所有函数的代码行数之和
for(i in 1:nFun){
  num = num+datap[i,2]
}
nnet=1-locnn/num
tarantula=1-loct/num

out <- read.csv("./workrateNew.csv")
out[nrow(out)+1,1]=nnet
out[nrow(out),2]=tarantula
write.csv(out,file="./workrateNew.csv",row.names = FALSE)

}
