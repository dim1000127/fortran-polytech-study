program exercise_2_13
   use Environment
      
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: x = 0, y = 0, z = 0, u = 0, v = 0

   open (file=input_file, newunit=In)
      read (In, *) x, y, z
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(3(a, f0.2/))") "x = ", x, "y = ", y, "z = ", z
   close (Out)
   
   u = Max(x, y, z)
   v = Min(x, y ,z)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "('Max = ', f0.2)") u
      write (Out, "('Min = ', f0.2)") v
   close (Out)
end program exercise_2_13
