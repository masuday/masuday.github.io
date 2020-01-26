program test_cdflib77
   use cdflib77
   implicit none
   integer :: stat
   double precision :: p1,p2,q2,x,bound
   x = 1.567890
   p1 = 0.5*(1.0+erf(x/sqrt(2.0)))
   call cdfnor(1,p2,q2,x,0d0,1d0,stat,bound)
   ! comparison
   print *,"CDFLIB77 TEST"
   print *,"average difference =",abs(p1-p2)
end program test_cdflib77
