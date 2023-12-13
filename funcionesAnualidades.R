
## Autor: Mr. Mario Nathaniel de la Vega Ramírez
#  Contenido: Funciones relacionadas a anualidades 
## Abs: Para aquellas con solucion cerrada unicamente dados los datos usar la 
# formula
# En el caso de las tasas se uso un aumento porcentual y en VAAnt incremntos

##  Funciones sufijo VFVenc
VFVenc = function(tasa,nper,pago)
{
  return ( pago/tasa *(((1+tasa)**nper)-1) )
}
pagoVFVenc = function(tasa,nper,VF)
{
  return ( VF*tasa /(((1+tasa)**nper)-1)   )
}
tasaVFVenc = function(nper,pago,VF)
{
  x0=1.001 
   
  while( (((((x0 + 1)**nper) - 1 )/x0 )- VF/pago) > 0.0001)   
   {
    if(VF/pago  >  ((x0 + 1)**nper -1)/x0 )
    {
      x0 = x0* 1.00001
    }  
    else 
    { 
     x0 = x0* 0.99999
    }
   }
  
  return (x0)
}  

periodoVFVenc=function(tasa,pago,VF)  
{
  return( log((VF*tasa/pago) + 1)/(log(1+tasa)))  
}
  
## Funciones sufijo VFAnt
VFAnt = function(tasa,nper,pago)
{
  return (pago/tasa *(((1+tasa)**nper)-1) *(1+tasa))
}
pagoVFAnt = function(tasa,nper,VF)
{
  return ( VF*tasa / ((((1+tasa)**nper)-1)*(1+tasa)) ) 
}
tasaVFAnt = function(nper,pago,VF)
{
  
  x0=1.001 
  
  while( ((((((x0 + 1)**nper) - 1 )*(x0+1 )) /x0 )- VF/pago) > 0.0001)   
  {
    if(VF/pago  >  (x0 + 1)*((x0 + 1)**nper -1)/x0 )
    {
      x0 = x0* 1.00001
    }  
    else 
    { 
      x0 = x0* 0.99999
    }
  }
  
  return (x0)
  
}
periodoVFAnt =function(tasa,pago,VF)  
{
  return( log( ((VF*tasa / (pago*(1+tasa)) ) + 1) ) /(log(1+tasa)) )  
}  

## Funciones sufijo VAVenc
VAVenc = function(tasa,nper,pago)
{
  return ( pago/tasa * (1-((1+tasa)**(-nper))) )
}
pagoVAVenc = function(tasa,nper,VA)
{
  return ( VA*tasa / (1-((1+tasa)**(-nper))) )
}
tasaVAVenc = function(nper,pago,VA)
{
  x0= .1
 
   while(  ((VA/pago)* x0 )/((( 1- (x0 + 1)**(-nper)) ) ) - 1 > (0.0000001) )   
  {

    if(VA/pago  <  (1 - (x0 + 1)**(-nper) )/x0 )
    {
      x0 = x0* 1.00001
    }  
    else 
    { 
      x0 = x0* 0.99999
    }
  }
  
  return (x0)
}
periodoVAVenc=function(tasa,pago,VA)  
{
 return( - log(1- (VA*tasa /pago)) / log((1+tasa)) )
}          
  
## funciones sufijo VAAnt

VAAnt = function(tasa,nper,pago)
{
  return ( pago/tasa * (1-((1+tasa)**(-nper))) * (1+tasa) )
}
pagoVAAnt = function(tasa,nper,VA)
{
  return ( VA*tasa / ((1-((1+tasa)**(-nper)))*(1+tasa)) )
}

tasaVAAnt = function(nper,pago,VA)
{
  x0 =.001
  xp1 = Inf
  xp2= 0
  while( (VA - (pago/x0 * (1-((1+x0)**(-nper))) * (1+x0))  )**2 - .000001 > .000001 )   
  { 
    ##print( paste(VA ,  pago/x0 * (1-((1+x0)**(-nper))) * (1+x0)) )
    ##print( paste(xp1 ,  xp2 ))
    if(xp1 == xp2)
    {break}
    
    if (VA < (pago/x0 * (1-((1+x0)**(-nper))) * (1+x0) ) )
    { xp1 = x0 
      x0 =  x0+ 0.0000001 }
    else 
    { x0 =   x0-0.0000001
      xp2 = x0}  
  
     
  }
  
  return (x0)
}

periodoVAAnt =function(tasa,pago,VA)
{
  return( - log(1- ((VA*tasa)/(pago*(1+tasa)))) / log((1+tasa)) )
}  

## Funciones sufijo VADif

VADif = function(tasa,nper,pago,difper)
{
  return( ( (pago/tasa) * (1-((1+tasa)**(-nper)))) /((1+tasa)**(difper)) )
}
pagoVADif = function(tasa,nper,difper,VA)
{
  return( (VA* ((1+tasa)**difper)*(tasa))/(1 - ((1+tasa )**(-nper)) ))
}  

