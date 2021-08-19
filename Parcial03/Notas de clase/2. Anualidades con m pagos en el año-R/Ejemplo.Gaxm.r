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
#---------anualidad geometrica anticipada
Gaaqmx = function(x,i,iq,m,q,pars){
try(if(iq > i) stop("tasa inflacion invalida"))
try(if(m %% q != 0) stop("m no es divisible por q"))
v = 1/(1+i)
k = seq(0,m*(110-x)-1)
kmpx = tpx(k/m,x,pars)
vkm = v^(k/m)
vqm = (1+iq)^(floor(k*q/m)/q)
a = sum(vkm*vqm*kmpx)/m
return(a)}
#-----------
x = 40; i = 0.07; m = 12; w = 110; q = 1; iq = 0.025;
#-------pagos de 1/12 cada mes anticipado, total 1 año = 1

#----------respuesta
(gaaxm = Gaaqmx(x,i,iq,m,q,pars))

#------pago continuo
#----- vida entera geometrica, creciente a una tasa iq 


v = (1+i)^(-1)

bt = function(t){
(1+iq)^(floor(t*q)/q)}

fn <- function(t){bt(t)*v^t*tpx(t,x,pars)}


(Gqcax=integrate(fn, lower = 0,
 upper = 110-x, subdivisions = 200)$value)


#------aproximar con una anualidad en tiempo continuo
dm = m*(1-v^(1/m))
delta = log(1+i)

(Gqcax*delta/dm)


