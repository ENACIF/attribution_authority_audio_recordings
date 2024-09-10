#Packages
library(ggplot2)
library(dplyr)
library(DiscriMiner) 
library(Rmpfr)

#Corpus
url<-file.choose()
VA<-read.csv(url)
head(VA)

dubi<-"Questioned" #Label
indu<-"Unquestioned" #Label

#Corpus without the Questioned
d_promedio=subset(VA,ID!=dubi & ID!=indu)

#Questioned information 
dubitada=subset(VA,ID==dubi)
#Unquestioned information
indubitada=subset(VA,ID==indu)

data1<-d_promedio
data2<-dubitada
data3<-indubitada

#Validation
length(names(table(data1$ID)))
length(names(table(data2$ID)))
length(names(table(data3$ID)))

table(data1$ID)
table(data2$ID)
table(data3$ID)

#Aitken 2004
M11<-function(data,dub,ind){
d_p_a<-data
dubi<-dub
indu<-ind

nam<-names(table(d_p_a$ID))
pos_dubi<-match(dubi,nam)
pos_indu<-match(indu,nam)
p<-nam[-pos_dubi]
pos_indu<-match(indu,p)
pob<-p[-pos_indu]
#Multi
d_p_a$fname<-as.factor(d_p_a$fname)
d_dubi<-subset(d_p_a, fname == dubi)
d_indu<-subset(d_p_a, fname == indu)
d_ds<-subset(d_p_a, fname != dubi)
d_pob<-subset(d_ds, fname != indu)

mesu<-c("F1","F2","F3","F4")
r1<-d_dubi[,mesu]
r2<-d_indu[,mesu]
r3<-d_pob[,mesu]
m<-length(pob)

t<-table(d_pob$fname)
ts<-as.vector(t)
ts[ts==0] <- NA
ni<-ts[!is.na(ts)]

m_xi<-aggregate(d_pob[,mesu], list(d_pob[,"fname"]), mean,na.rm=TRUE, na.action=NULL)
m_x<-colMeans(m_xi[,mesu])

nl<-c(nrow(r1),nrow(r2))

m_yl<-rbind(colMeans(r1,na.rm = TRUE),colMeans(r2,na.rm = TRUE))

library(DiscriMiner) 
u<-withinCov(d_p_a[,mesu], d_p_a[,"fname"],div_by_n=FALSE)
c<-betweenCov(d_p_a[,mesu], d_p_a[,"fname"],div_by_n=FALSE)

C<-c-u

d1<-u/nl[1]
d2<-u/nl[2]

Y1<-(solve(d1)+solve(d2))
Y2<-solve(d1)%*%m_yl[1,]
Y3<-solve(d2)%*%m_yl[2,]
Y<-solve(Y1)%*%(Y2+Y3)

p=2
h<-((4/((2*p)+1))^(1/(p+4)))*(m^(-1/(p+4)))

LR11<-((2*pi)^-p)
LR12<-(det(d1)^(-1/2))
LR13<-(det(d2)^(-1/2))
LR14<-(det(C)^(-1/2))
LR15<-((m*(h^p))^-1)
LR16<-solve(d1)+solve(d2)+solve((h^2)*C)
LR161<-(det(LR16))^(-1/2)
LR1<-LR11*LR12*LR13*LR14*LR15*LR161

LR20<-(-1/2)
LR21<-t(m_yl[1,]-m_yl[2,])
LR22<-solve(d1+d2)
LR23<-m_yl[1,]-m_yl[2,]

LR2<-exp(LR20%*%LR21%*%LR22%*%LR23)

#Ojo tiene una sumatoria
LR3=0
for(i in nrow(m_xi)){
    dif<-(Y-m_xi[i,2:5]) #ojo número de formantes
    tdif<-t(dif)
    D01<-(-1/2)
    D011<-D01*dif
    D02<-solve(d1)+solve(d2)
    D021<-solve(D02)
    D03<-(h^2)*C
    D04<-D02+D03
    D05<-as.matrix(dif)%*%solve(D04)%*%as.matrix(tdif)
    LR31<-as.numeric(exp(mpfr(D01*D05,precBits = 120)))
    LR31<-ifelse(is.infinite(LR31), 1000000, LR31)
    LR3<-LR3+LR31
}

LR_up<-LR1*LR2*LR3

LR61_D=0
for(i in nrow(m_xi)){
    dif<-(m_yl[1,]-m_xi[i,2:5])#ojo número de formantes
    tdif<-t(dif)
    D01<-(-1/2)
    D02<-d1
    D03<-(h^2)*C
    D04<-D02+D03
    D05<-as.matrix(dif)%*%solve(D04)%*%as.matrix(tdif)
    LR31<-as.numeric(exp(mpfr(D01*D05,precBits = 120)))
    LR31<-ifelse(is.infinite(LR31), 1000000, LR31)
    LR61_D<-LR61_D+LR31
}

LR62_D=0
for(i in nrow(m_xi)){
    dif<-(m_yl[2,]-m_xi[i,2:5])#ojo número de formantes
    tdif<-t(dif)
    D01<-(-1/2)
    D02<-d2
    D03<-(h^2)*C
    D04<-D02+D03
    D05<-as.matrix(dif)%*%solve(D04)%*%as.matrix(tdif)
    LR31<-as.numeric(exp(mpfr(D01*D05,precBits = 120)))
    LR31<-ifelse(is.infinite(LR31), 1000000, LR31)
    LR62_D<-LR62_D+LR31
}

LR51_1<-(det(d1))^(-1/2)
LR52_1<-solve(d1)+solve((h^2)*C)
LR53_1<-(det(LR52_1))^(-1/2)
LR54_1<-LR51_1*LR53_1

LR51_2<-(det(d2))^(-1/2)
LR52_2<-solve(d2)+solve((h^2)*C)
LR53_2<-(det(LR52_2))^(-1/2)
LR54_2<-LR51_2*LR53_2

LR6_D<- (LR61_D*LR54_1) + (LR62_D*LR54_2)
LR71<-((2*pi)^-p)
LR72<-(det(C)^(-1))
LR73<-((m*(h^p))^-2)
LR_down<-LR71*LR72*LR73*LR6_D
LR<-as.numeric(LR_up/LR_down)

return(LR)
}

Dats1<-rbind(data1,data2)
dim(Dats1)
Dats1<-rbind(Dats1,data3)
dim(Dats1)
Dats2<-Dats1[complete.cases(Dats1), ]
dim(Dats2)

Dats2$fname<-as.factor(Dats2$ID)
nom<-names(table(Dats2$fname))

ind0<-nom[nom!= dubi]
mat<-matrix(0,length(ind0),7)
for(j in 1:length(ind0)){
    ind<-ind0[j]
    dprueba_a<-subset(Dats2, Labels == "a")
    LRaM_a<-M11(dprueba_a,dubi,ind)
    
    dprueba_e<-subset(Dats2, Labels == "e")
    LRaM_e<-M11(dprueba_e,dubi,ind)
    
    dprueba_i<-subset(Dats2, Labels == "i")
    LRaM_i<-M11(dprueba_i,dubi,ind)
    
    dprueba_o<-subset(Dats2, Labels == "o")
    LRaM_o<-M11(dprueba_o,dubi,ind)
    
    dprueba_u<-subset(Dats2, Labels == "u")
    LRaM_u<-M11(dprueba_u,dubi,ind)
    
    mat[j,1]<-dubi
    mat[j,2]<-ind[1]
    mat[j,3]<-LRaM_a
    mat[j,4]<-LRaM_e
    mat[j,5]<-LRaM_i
    mat[j,6]<-LRaM_o
    mat[j,7]<-LRaM_u
}

colnames(mat)<-c("Questioned","Unquestioned","LR_a","LR_e","LR_i","LR_o","LR_u")
LRS<-as.data.frame(mat)
LRS