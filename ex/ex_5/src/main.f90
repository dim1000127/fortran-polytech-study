program exercise_5_13a_12_11v
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, Pos = 0
   real(R_)                :: MaxValueNegative
   real(R_), allocatable   :: Z(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Z
   close (In)
  
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(f0.2, 1x))") Z
   close (Out)

   if (Equal(Z)) then
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, *) "All elements are equal"
      close (Out)
   else
      call Negative(Z, MaxValueNegative, Pos)

     open (file=output_file, encoding=E_, newunit=Out, position='append')
       write (Out, '(a, T10, "= ", i0)') "Position", Pos
       write (Out, '(a, T20, "= ", f0.2)') "Max negative value",  MaxValueNegative
       write (Out, "("//N//"(f0.2, 1x))") Z
     close (Out)
   end if

contains
   pure subroutine Negative(Z, MaxValueNegative, Pos)
      integer     Pos
      real(R_)    Z(:), MaxValueNegative
      intent(out) Z, MaxValueNegative, Pos

      Pos = MaxLoc(Z, 1, Z < 0)
      MaxValueNegative = Z(Pos)
      Z([1, Pos]) = Z([Pos, 1])
   end subroutine Negative
   

   pure function Equal(Z) result(Flag)
      real(R_)             Z(:)
      logical              Flag
      intent(in)           Z
  
      Flag = All(Z(2:) == Z(1))

      !Flag = Count(Z == Z(1)) == Size(Z)
   end function Equal 
end program exercise_5_13a_12_11v
