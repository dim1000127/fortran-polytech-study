module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains
   ! Поиск трех наиболее молодых петербуржцев мужчин
   pure recursive subroutine Students_By_Mask(Group, Boys, Mask, j)
      type(student)                 :: Group
      type(boy)                     :: Boys
      logical                       :: Mask(:)
      integer                       :: j

      intent(in)                    :: Group, j
      intent(inout)                 :: Boys, Mask 

      integer                       :: Ind 

      Ind = MaxLoc(Group%Year_Birth, 1, Mask)
      
      Boys%Surname(j) = Group%Surname(Ind)
      Boys%Initials(j) = Group%Initials(Ind)
      Boys%Year_Birth(j) = Group%Year_Birth(Ind)
      
      Mask(Ind) = .false.
      
      if (j < BOYS_AMOUNT .and. Count(Mask) /= 0) &
         call Students_By_Mask(Group, Boys, Mask, j+1)
   end subroutine Students_By_Mask
end module group_process
