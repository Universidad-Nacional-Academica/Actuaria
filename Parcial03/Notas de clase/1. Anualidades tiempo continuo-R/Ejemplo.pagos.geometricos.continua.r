#-Ejemplo anualidad continua pagos geometricos

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

require(xtable)
print(xtable(M,digits=10))

pars = M[4,] 

muxt = function(t,x,pars){
a = pars[1]
b = pars[2]
C = pars[3]
mx = a + b*C^(x+t)
return(mx)}

tpx = function(t,x,pars){
a = pars[1]
b = pars[2]
C = pars[3]
p = exp(-a*t - b*C^x*(C^t-1)/log(C))
return(p)}
x = 58; w = 110;

#----- vida entera geometrica, creciente a una tasa iq 
#------------tasas y beneficio
i = 0.06
v = (1+i)^(-1)
iq = 0.025
q = 1
pago = 12
bt = function(t){
pago*(1+iq)^(floor(t*q)/q)}

fn <- function(t){bt(t)*v^t*tpx(t,x,pars)}


(Gqcax=integrate(fn, lower = 0,
 upper = 110-x, subdivisions = 200)$value)

q = 4



