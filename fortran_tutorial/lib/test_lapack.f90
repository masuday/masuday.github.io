program test_lapack
   implicit none
   integer,parameter :: n=3
   integer :: info
   double precision :: alpha,beta,A(n,n),b(n),x(n)
   interface
     SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, N, NRHS
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
     END SUBROUTINE DPOSV
   end interface
   A(1,1:n) = [5,4,1]
   A(2,1:n) = [4,6,1]
   A(3,1:n) = [1,1,7]
   x = 1.0
   b = matmul(A,x)
   ! lapack
   call DPOSV("L",n,1,A,n,b,n,info)
   if(info/=0) stop "error in DPOSV"
   ! comparison
   print *,"LAPACK TEST"
   print *,"average difference =",sum(abs(b-x))/n
end program test_lapack

