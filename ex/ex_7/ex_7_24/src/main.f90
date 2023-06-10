program exercise_7_24
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, H = 0, i = 0, k = 0
   real(R_), allocatable   :: A(:, :), B(:) 
   integer, allocatable    :: Initial(:)

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) k
      read (In, *) N, M
      allocate (A(N, M))
      read (In, *) (A(i, :), i = 1, N)
   close (In)

   ! Вывод данных.
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") K
      write (Out, '('//M//'f6.2)') (A(i, :), i = 1, N)
   close (Out)

   H = N*M/k

   if (k == (N + M)) then
      H = H -1
   end if

   if (k <= (N + M)) then
      allocate (Initial(0:k-1))
      allocate (B(H))

      call SumIndexesMultiplyK(A,B, Initial, M, k)
   
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out,"(/"//H//"f6.2)") B
      close (Out)
   else
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, *) "Нет элементов, сумма индексов которых кратна K"
      close (Out)
   end if

contains
  pure  subroutine SumIndexesMultiplyK(A, B, Initial, M, k)
      real(R_), intent(in)    :: A(:, :)
      integer, intent(in)     :: M, k
      real(R_), intent(out)   :: B(:)
      integer, intent(out)    :: Initial(0:k-1) 

      integer j

      Initial = [(k - j, j = 0, k - 1)]
      B = [((A(Initial(Mod(j, k))::k, j)), j = 1, M)]
       
      !Quanity = [((N + j) / k, j = 0, k - 1)] 

      !Last = 1

      !do j = 1, N
      !   i = Mod(j, k)
      !   First = Last
      !   Last = First + Quanity(i)
      !   B(First:Last) = A(Initial(i)::k, j)
      !end do
   end subroutine SumIndexesMultiplyK
end program exercise_7_24
