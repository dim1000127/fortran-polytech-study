program exercise_8_13
   use Environment
   use Matrix_IO
   use Matrix_process

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   real(R_), allocatable   :: B(:, :), C(:)
   integer                 :: i = 0, N = 0

   B = ReadMatrix(input_file)

   call OutputMatrix(output_file, B)

   N = UBound(B, 2)
   allocate (C(N))
   do concurrent (i = 1:N)
      C(i) = MinimumValue(B(:, i))
   end do

   !C = MinVal(B, dim=2) ! Оформлять отдельную чистую функцию в регулярном стиле нет необходимости.

   call OutputArray(output_file, C)
end program exercise_8_13
