program exercise_6_1b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: cos_x = 0, x = 0

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)
   
   cos_x = CosX(x)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", e13.6/))') 'x', x, "Cos(x)", cos_x, "Fortran Cos(x)", Cos(x), "Error", cos_x - Cos(x)
   close (Out)

contains
   real(R_) pure function CosX(x)
      real(R_), intent(in) :: x
      
      real(R_), parameter :: double_PI = 8 * Atan(1._R_)
      real(R_) R(4), Numerators(4), Denominators(4), n_fact, x_c, x_8
      integer  Ns(8)

      x_c = Mod(x, double_PI)
     
      Numerators = x_c ** [2, 4, 6, 8]
      Numerators = Numerators * [-1, 1, -1, 1]
      
      x_8 = Numerators(4)
      Denominators = [2, 2*3*4, 2*3*4*5*6, 2*3*4*5*6*7*8]
      Ns = [1, 3, 5, 7, 2, 4, 6, 8]
      R = Numerators / Denominators

      CosX = 1._R_ + Sum(R)
      
      do while (CosX + R(4) /= CosX)
         Numerators = Numerators * x_8
         n_fact = Denominators(4)
         Ns = Ns + 8
         
         Denominators = Ns(1:4) * Ns(5:8)
         Denominators(1) = n_fact * Denominators(1)
         Denominators(2) = Denominators(1) * Denominators(2)
         Denominators(3) = Denominators(2) * Denominators(3)
         Denominators(4) = Denominators(3) * Denominators(4)
         
         R = Numerators / Denominators

         CosX = CosX + Sum(R)
      end do

      !print "('Число членов суммы: ', i0)", (Ns(8)+1) / 2
      !print "('Число итераций: ', i0)", (Ns(8)-1) / 8
   end function CosX 
end program exercise_6_1b
