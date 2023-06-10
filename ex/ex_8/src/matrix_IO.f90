module Matrix_IO
   use Environment
   
   implicit none
contains
   ! Чтение матрицы.
   function ReadMatrix(input_file) result(B)
      character(*), intent(in)   :: input_file
      real(R_), allocatable      :: B(:, :)

      integer :: In = 0, i = 0, N = 0, M = 0

      open (file=input_file, newunit=In)
         read (In, *) N, M  
         ! Т. к. будет вестись сравнение строк, то эффективнее будет размещать
         ! матрицу по строкам -- A(M, N). Тогда любая i-ая строка A(:, i) будет сплошной.
         allocate (B(M, N))
         read (In, *) B
      close (In)
   end function ReadMatrix
  
   ! Вывод матрицы.
   subroutine OutputMatrix(output_file, B)
      character(*), intent(in)   :: output_file
      real(R_), intent(in)       :: B(:, :)

      integer :: Out = 0, i = 0

      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, '('//UBound(B, 1)//'f6.2)') B
      close (Out)
   end subroutine OutputMatrix

   ! Вывод массива.
   subroutine OutputArray(output_file, C)
      character(*), intent(in)   :: output_file
      real(R_), intent(in)       :: C(:)

      integer :: Out = 0

      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, *) "Одномерный массив"
         write (Out, '('//Size(C)//'f6.2)') C
      close (Out)
   end subroutine OutputArray
end module Matrix_IO
