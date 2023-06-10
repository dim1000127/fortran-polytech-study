module Matrix_process
   use Environment
   
   implicit none
contains
   pure real(R_) function MinimumValue(A)
      real(R_)    A(:)
      intent(in)  A

      integer i

      MinimumValue = A(1)
      do i = 2, Size(A)
         if (A(i) < MinimumValue) &
            MinimumValue = A(i)
      end do
   end function MinimumValue
end module Matrix_process
