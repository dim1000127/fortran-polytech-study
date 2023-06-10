program exercise_3_10
   use Environment
   
   implicit none
   character(*), parameter   :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                   :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_)                  :: Amount = 0
   real(R_), allocatable     :: B(:, :) 
   character(:), allocatable :: fmt

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (B(N, M))

      read (In, *) (B(i, :), i = 1, N)
   close (In)

   Amount = Sum(B(3:N,:))

   open (file=output_file, encoding=E_, newunit=Out)
      fmt  = "("//M//"f6.2)"
      write (Out, "(2(a, i0/))") "Строк ", N, "Столбцов ", M
      write (Out,"("//M//"f6.2)") (B(i, :), i = 1, N)
      write (Out, *)
      write (Out, "('Amount = ', f0.2)") Amount
   close (Out)
end program exercise_3_10
