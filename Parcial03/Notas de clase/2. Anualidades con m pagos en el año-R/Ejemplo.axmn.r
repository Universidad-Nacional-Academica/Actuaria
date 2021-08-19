# Ejemplo axmn, relacion con renta continua, aproximacion

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
aaxmn = function(x,m,n,i,pars){
v = 1/(1+i)
k = seq(0,m*n-1,1)
vkm = v^(k/m)
kmpx = tpx(k/m,x,pars)
a = sum(vkm*kmpx)/m
return(a)}
#-----------
x = 40; i = 0.07; m = 12; w = 110; n = 20;
#-------pagos de 1/12 cada mes anticipado, total 1 año = 1

#----------respuesta
(axmn = aaxmn(x,m,n,i,pars))

#-------------anualidad vida entera pago continuo
C = 1;
fn = function(t){(1+i)^(-t)*tpx(t,x,pars)}
(acxn = C*integrate(fn,0,n)$value)

#------aproximar con una anualidad en tiempo continuo

v=1/(1+i)
dm = m*(1-v^(1/m))
delta = log(1+i)

(acxn*delta/dm)


