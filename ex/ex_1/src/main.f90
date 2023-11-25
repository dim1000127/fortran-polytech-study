program exercise_1_5
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 0, i = 0
   real(R_)                   :: x = 0, squ = 0, cos_x
   real(R_)                   :: Items(4) = 0 

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "x", x
   close (Out)

   squ = -x ** 2
   Items(1) = 1
   
   do i = 2, 4
      Items(i) = Items(i-1) * squ / (((2 * i - 3)) * (2 * i - 2))
   end do
   
   cos_x = Sum(Items)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, fmt) "cos(x)", cos_x
      ! Проверка:
      write (Out, fmt) "error", cos(x) - cos_x
   close (Out)
end program exercise_1_5
