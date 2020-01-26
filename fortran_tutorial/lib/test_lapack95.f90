program test_lapack95
   use f95_lapack
   implicit none
   integer,parameter :: n=3
   integer :: info
   double precision :: alpha,beta,A(n,n),b(n),x(n)
   A(1,1:n) = [5,4,1]
   A(2,1:n) = [4,6,1]
   A(3,1:n) = [1,1,7]
   x = 1.0
   b = matmul(A,x)
   ! lapack
   call LA_POSV(A,b,info=info)
   if(info/=0) stop "error in DPOSV"
   ! comparison
   print *,"LAPACK95 TEST"
   print *,"average difference =",sum(abs(b-x))/n
end program test_lapack95

