# Ejemplo axm, relacion con renta continua, aproximacion

#------------- Gompertz-Makeham
tpx = function(t,x,pars){
a = pars[1];b = pars[2];C = pars[3];
s = exp(-a)
g = exp(-b/log(C))
p = s^t*g^(C^x*(C^t-1))
return(p)}
#-------------parametros GM
pars = c(0.0014440776, 0.0001815122, 1.0940478946)

#-----------
aaxm = function(x,m,i,pars){
v = 1/(1+i)
k = seq(0,m*(w-x)-1,1)
vkm = v^(k/m)
kmpx = tpx(k/m,x,pars)
a = sum(vkm*kmpx)/m
return(a)}
#-----------
x = 40; i = 0.07; m = 12; w = 110;
#-------pagos de 1/12 cada mes anticipado, total 1 año = 1

#----------respuesta
(axm = aaxm(x,m,i,pars))

#-------------anualidad vida entera pago continuo
C = 1;
fn = function(t){(1+i)^(-t)*tpx(t,x,pars)}
(acx = C*integrate(fn,0,w-x)$value)

#------aproximar con una anualidad en tiempo continuo

v=1/(1+i)
dm = m*(1-v^(1/m))
delta = log(1+i)

(acx*delta/dm)


#--simular el valor presente 
#--renta vitalicia anual anticipada

#-------------
require(eha)

#--- parametros en eha
a = pars[1];b = pars[2];C = pars[3];

a2 = a
sigma = 1/log(C)
a1 = b*exp(x/sigma)

N = 5000
Tx = rmakeham(n=N, 
c(a1,a2), 
scale = sigma)

Kmx = floor(m*Tx)/m

aaxm.s = (1-v^(Kmx+1/m))/dm

(mean(aaxm.s))

acx.s = (1-v^(Tx))/delta

(mean(acx.s))

(mean(acx.s)*delta/dm)


hist(aaxm.s,100,xlim=c(0,15),col='red',border=F)
hist(acx.s*delta/dm,100,add=T,col=scales::alpha('skyblue',.5),border=F)

