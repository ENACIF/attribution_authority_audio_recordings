#Package
library(readxl)

#System 
url<-file.choose()
D1<- as.data.frame(read_excel(url))
head(D1)

#Cost calculation
D1$Y<-D1$FUSED
D1$Ye<-exp(D1$Y)
D1$Ac=log2(1+(1/D1$Ye))
D1$B<-log2(1+D1$Ye)

V=subset(D1,RESPUESTA==1)
SumA=sum(V$Ac)
A1<-(1/length(V))*SumA

Fa=subset(D1,RESPUESTA==0)
SumB<-sum(Fa$B)
B1<-(1/length(Fa))*SumB
Cllr<-(1/2)*(A1+B1)

#LR_Costs_A
D1$Y_a<-D1$A
D1$A_a=log2(1+(1/D1$Y_a))
D1$B_a<-log2(1+D1$Y_a)

V=subset(D1,RESPUESTA==1)
SumA=sum(V$A_a)
A1<-(1/length(V))*SumA

Fa=subset(D1,RESPUESTA==0)
SumB<-sum(Fa$B_a)
B1<-(1/length(Fa))*SumB
Cllr_a<-(1/2)*(A1+B1)

#LR_Costs_e
D1$Y_e<-D1$E
D1$A_e=log2(1+(1/D1$Y_e))
D1$B_e<-log2(1+D1$Y_e)

V=subset(D1,RESPUESTA==1)
SumA=sum(V$A_e)
A1<-(1/length(V))*SumA

Fa=subset(D1,RESPUESTA==0)
SumB<-sum(Fa$B_e)
B1<-(1/length(Fa))*SumB
Cllr_e<-(1/2)*(A1+B1)

#LR_Costs_e
D1$Y_i<-D1$I
D1$A_i=log2(1+(1/D1$Y_i))
D1$B_i<-log2(1+D1$Y_i)

V=subset(D1,RESPUESTA==1)
SumA=sum(V$A_i)
A1<-(1/length(V))*SumA

Fa=subset(D1,RESPUESTA==0)
SumB<-sum(Fa$B_i)
B1<-(1/length(Fa))*SumB
Cllr_i<-(1/2)*(A1+B1)

#LR_Costs_e
D1$Y_o<-D1$O
D1$A_o=log2(1+(1/D1$Y_o))
D1$B_o<-log2(1+D1$Y_o)

V=subset(D1,RESPUESTA==1)
SumA=sum(V$A_o)
A1<-(1/length(V))*SumA

Fa=subset(D1,RESPUESTA==0)
SumB<-sum(Fa$B_o)
B1<-(1/length(Fa))*SumB
Cllr_o<-(1/2)*(A1+B1)

#LR_Costs_u
D1$Y_u<-D1$U
D1$A_u=log2(1+(1/D1$Y_u))
D1$B_u<-log2(1+D1$Y_u)

V=subset(D1,RESPUESTA==1)
SumA=sum(V$A_u)
A1<-(1/length(V))*SumA

Fa=subset(D1,RESPUESTA==0)
SumB<-sum(Fa$B_u)
B1<-(1/length(Fa))*SumB
Cllr_u<-(1/2)*(A1+B1)


Cllr<-data.frame(LR_g=Cllr,LR_a=Cllr_a,LR_e=Cllr_e,LR_i=Cllr_i,LR_o=Cllr_o,LR_u=Cllr_u)
Cllr

