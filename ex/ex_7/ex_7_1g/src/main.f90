program exercise_7_1g
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_), allocatable   :: A(:), AbsA(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (A(N))
      read (In, *) A
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//N//"f6.2)") A
   close (Out)
 
   allocate(AbsA(N))
   
   call SortByModule(A, AbsA)
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(/"//N//"f6.2)") A
   close (Out)

contains
   pure subroutine SortByModule(A, AbsA)
      real(R_), intent(inout) :: A(:), AbsA(:)

     ! real(R_) :: Tmp
      integer  :: i, MaxInd

      AbsA = Abs(A)

      do i = 1, Size(AbsA)-1
         MaxInd = MaxLoc(AbsA(i:), 1) + i - 1
         if (i /= MaxInd) then      
            
            AbsA([i, MaxInd]) = AbsA([MaxInd, i])
            A([i, MaxInd]) = A([MaxInd, i])

            !Tmp = AbsA(i)
            !AbsA(i) = AbsA(MaxInd)
            !AbsA(MaxInd) = Tmp
            
            !Tmp = A(i)
            !A(i) = A(MaxInd)
            !A(MaxInd) = Tmp
         end if
      end do
   end subroutine SortByModule
end program exercise_7_1g
