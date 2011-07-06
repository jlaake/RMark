"deriv_inverse.link" <-
function(real,x,link)
{
if(substr(link,1,6)=="mlogit" | substr(link,1,6)=="MLogit")link="MLogit"
real=as.vector(real)
switch(link,
logit=x*real*(1-real),
log=x*real,
loglog=-real*x*log(real),
cloglog=-log(1-real)*x*(1-real),
identity=x,
mlogit=x*real*(1-real),
sin=x*cos(asin(2*real-1))/2,
Sin=x*cos(asin(2*real-1))/2,
Logit=x*real*(1-real),
Log=x*real,
LogLog=-real*x*log(real),
CLogLog=-log(1-real)*x*(1-real),
Identity=x,
MLogit=x*real*(1-real)
)
}
