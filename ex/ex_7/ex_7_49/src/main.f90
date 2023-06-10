program exercise_7_49
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: B(:, :), C(:, :)
   integer, allocatable    :: Indexes(:)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (B(N, M))
      read (In, *) (B(i, :), i = 1, N)
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (B(i, :), i = 1, N)
   close (Out)

   allocate (Indexes(Size(Shape(B))))
   allocate (C(N-1, M-1))
  
   call GetWithoutRowColumnMax(B, C, Indexes)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *) "Индексы максимального элемента"
      write (Out, '(2(i0, 1x))') Indexes
      write (Out, '('//M-1//'f6.2)') (C(i, :), i = 1, N-1)  
   close (Out)

contains
   pure subroutine GetWithoutRowColumnMax(B, C, Indexes)
      real(R_), intent(in)    :: B(:, :)
      real(R_), intent(out)   :: C(:, :)
      integer, intent(out)    :: Indexes(:) 

      integer iMax, jMax
     
      Indexes = MaxLoc(B)

      iMax = Indexes(1)
      jMax = Indexes(2)

      C(:iMax - 1, :jMax - 1 ) = B(:iMax - 1, :jMax - 1)
      C(:iMax - 1, jMax:) = B(:iMax - 1, jMax + 1:)    
      C(iMax:, :jMax - 1) = B(iMax + 1:, :jMax - 1)
      C(iMax:, jMax:) = B(iMax + 1:, jMax + 1:) 
   end subroutine GetWithoutRowColumnMax
end program exercise_7_49
