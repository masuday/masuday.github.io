program test_blas
   implicit none
   integer,parameter :: m=5, n=3, k=6
   double precision :: alpha,beta,A(m,k),B(k,n),C(m,n),D(m,n)
   interface
     SUBROUTINE DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, &
           BETA, C, LDC)
     CHARACTER* 1 TRANSA, TRANSB
     INTEGER M, N, K, LDA, LDB, LDC
     DOUBLE PRECISION ALPHA, BETA
     DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*)
     END SUBROUTINE DGEMM
   end interface
   A = 2.0
   B = 0.5
   C = 1.5
   D = C
   alpha = 1.1
   beta = 0.7
   ! reference
   C = alpha*matmul(A,B) + beta*C
   ! blas
   call DGEMM("N","N",m,n,k,alpha,A,m,B,k,beta,D,m)
   ! comparison
   print *,"BLAS TEST"
   print *,"average difference =",sum(abs(C-D))/m*n
end program test_blas

