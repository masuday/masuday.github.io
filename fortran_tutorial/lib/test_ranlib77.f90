program test_ranlib77
   use ranlib77
   implicit none
   integer,parameter :: n=10
   integer :: seed1,seed2,i
   real :: r1(n),r2(n)
   seed1 = 1234567890
   seed2 = 987654321
   call setall(seed1,seed2)
   do i=1,n
      r1(i) = snorm()
   end do
   call setall(seed1,seed2)
   do i=1,n
      r2(i) = snorm()
   end do
   ! comparison
   print *,"RANLIB77 TEST"
   print *,"average difference =",sum(abs(r1-r2))/n
end program test_ranlib77
