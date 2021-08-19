
#--- parametros Gompertz-Makeham

M = matrix(0,4,3)
# mujeres-80-89:
a = 4.450e-03 
b = 2.814e-05 
C = 1.101e+00 
M[1,]=c(a,b,C)

# hombres-80-89:
a = 4.918e-03 
b = 1.559e-05 
C = 1.107e+00 
M[2,]=c(a,b,C)

# mujeres-00-08:
a = 4.444e-03 
b = 5.017e-07 
C = 1.143e+00 
M[3,]=c(a,b,C)

# hombres-00-08:
a = 7.121e-03 
b = 2.504e-06 
C = 1.126e+00 
M[4,]=c(a,b,C)

colnames(M)=c("a","b","c")
rownames(M)=c("m80-89","h80-89","m00-08","h00-08")
 
(M)

#-----------
require(eha)

#--- parametros en eha
x = 50
a2 = M[4,1]
sigma = 1/log(M[4,3])
a1 = M[4,2]*exp(x/sigma)


#------------tasas y beneficio
i = 0.06
v = (1+i)^(-1)
iq = 0.025
pago = 12
bt = function(t){
pago*(1+iq)^(floor(t))}

fn <- function(t){bt(t)*v^t}

#---------- simular
N = 1000
Bt = double(N)

require(rmutil)
for(j in 1:N){
Tx = rmakeham(1, shape = c(a1, a2), scale = sigma)
Bt[j]=int(fn,  0,  Tx)
}

hist(Bt,100)
points(mean(Bt),0,pch=20,col='red')

#-------------------aritmetica
ht = function(t){
(1+floor(t*q))/q}

hn <- function(t){ht(t)*v^t}


Ht = double(N)

require(rmutil)
for(j in 1:N){
Tx = rmakeham(1, shape = c(a1, a2), scale = sigma)
Ht[j]=int(hn,  0,  Tx)
}


#-------------------anualidad


dn <- function(t){v^t}


Dt = double(N)

require(rmutil)
for(j in 1:N){
Tx = rmakeham(1, shape = c(a1, a2), scale = sigma)
Dt[j]=int(dn,  0,  Tx)
}

#--------------lineal
rho = 0.05
Lt = (pago - rho)*Dt+rho*q*Ht


hist(Lt,100)
points(mean(Lt),0,pch=20,col='red')


hist(Bt,100,xlim=c(0,400),col='skyblue',border=F)
hist(Lt,100,add=T,col=scales::alpha('red',.5),border=F)

