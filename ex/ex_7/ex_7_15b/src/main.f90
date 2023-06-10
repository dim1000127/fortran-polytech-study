program exercise_7_15b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: A(:, :)
   real(R_)                :: x = 0

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (A(N, N))
      read (In, *) (A(i, :), i = 1, N)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') (A(i, :), i = 1, N)
   close (Out)

   x = MaxVal(MinVal(A, dim=1))

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, T5, "= ", f6.2)') "x", x
   close (Out)
end program exercise_7_15b
