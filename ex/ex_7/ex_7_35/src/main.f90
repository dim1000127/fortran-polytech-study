program exercise_7_35
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0
   real(R_), allocatable   :: A(:,:), Sums(:)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      ! Т. к. будет вестись сравнение строк, то эффективнее будет размещать
      ! матрицу по строкам -- A(M, N). Тогда любая i-ая строка A(:, i) будет сплошной.
      allocate (A(M, N))
      read (In, *) A
   close (In)
   
   open (file=output_file, encoding=E_, position='rewind', newunit=Out)
       write (Out, '('//M//'f6.2)') A
   close (Out)

   allocate (Sums(N)) 
      
   call SortBySumRow(A, Sums)
    
   open (file=output_file, encoding=E_, position='append', newunit=Out)  
       write (Out, *)
       write (Out, *) "Сумма элементов строк матрицы (в порядке убывания)"
       write (Out, '('//N//'f6.2)') Sums   
       write (Out, *)
       write (Out, *) "Отсортированный массив A в порядке убывания сумм строк"
       write (Out, '('//M//'f6.2)') A
   close (Out)
   
contains
   ! Чистая подпрограмма в регулярном стиле.
   pure subroutine SortBySumRow(A, Sums)
      real(R_), intent(inout)    :: A(:, :), Sums(:)
   
      integer i, MaxInd

      Sums = Sum(A, dim=1)

      do i = 1, Size(Sums)-1
         MaxInd = MaxLoc(Sums(i:), 1) + i - 1
         if (i /= MaxInd) then      
            Sums([i, MaxInd]) = Sums([MaxInd, i])
            A(:, [i, MaxInd]) = A(:, [MaxInd,i])
         end if
      end do
   end subroutine SortBySumRow
end program exercise_7_35
