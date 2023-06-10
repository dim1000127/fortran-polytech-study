program exercise_4_4v
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_)                :: a = 0, b = 0, h = 0, I = 0
   real(R_), allocatable   :: X(:) 
   
   open (file=input_file, newunit=In)
      read (In, *) a, b, h
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "x1", a, "x2", b, "h", h
   close (Out)
   
   N = Int((b-a) / h + .5_R_)
  
   allocate (X(N))
   call Integrate(a, h, X, I)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, T4, "= ", f0.4)') "I", I
   close (Out)
   
contains
   pure subroutine Integrate(a, h, X, I)
      real(R_) a, h, X(:), I
      intent(in) a, h
      intent(out) X, I
      integer j
      
      X = [(a+j*h , j = 1, Size(X))]
      X = Exp(X)*X**2
      I = Sum(X) * h
   end subroutine Integrate 
end program exercise_4_4v
