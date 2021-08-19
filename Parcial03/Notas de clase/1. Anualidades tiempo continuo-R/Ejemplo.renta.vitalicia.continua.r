# Ejemplo anualidad de vida
#---------ley Makeham - Beard
muxt = function(t,x,pars){
a = pars[1]; b = pars[2]; C=pars[3]; k =pars[4];
C+( a*exp(b*(x+t))  )/( 1 + k*a*exp(b*(x+t)) )}

tpx = function(t,x,pars){
a = pars[1]; b = pars[2]; C=pars[3]; k =pars[4];
exp(-C*t)*( (exp(-b*x)  + a*k )/( exp(-b*x)+ a*k*exp(b*t)))^(1/(k*b))}
#------------- ejemplo parametros
pars = c(0.000010936, 0.105864876, 0.000227759, 0.327569880) 
#------------- 
x = 58;w = 110; i = 0.05; C = 12;
#-------------anualidad vida entera pago continuo

fn = function(t){(1+i)^(-t)*tpx(t,x,pars)}
(acx = C*integrate(fn,0,w-x)$value)


#------caso vida mortalidad subestandar
# ddx = function(x){(1+11.68784/(x-17.58258))^(1.2)}
 
ddx = 1.7
tpxs = function(t,x,pars){
tpx(t,x,pars)^ddx}

fns = function(t){(1+i)^(-t)*tpxs(t,x,pars)}

(acxs = C*integrate(fns,0,w-x)$value)
[1] 146.5465

((acx - acxs)/acxs)



#------caso temporal
n = 20
(acxn = C*integrate(fn,0,n)$value)


