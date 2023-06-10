module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains

   pure recursive subroutine Get_Boys_Peter(Stud, Boy_1, Boy_2, Boy_3, Gender, Regis)
      type(student), pointer                  :: Stud,  Boy_1, Boy_2, Boy_3
      character(kind=CH_), intent(in)         :: Gender, Regis

      if (Stud%Gender == Gender .and. Stud%Registration == Regis) then  
         if (.not. Associated(Boy_1) .or. Stud%Year_Birth > Boy_1%Year_Birth) then
            Boy_3 => Boy_2
            Boy_2 => Boy_1
            Boy_1 => Stud
         else if (.not. Associated(Boy_2) .or. Stud%Year_Birth > Boy_2%Year_Birth) then
            Boy_3 => Boy_2
            Boy_2 => Stud
         else if (.not. Associated(Boy_3) .or. Stud%Year_Birth > Boy_3%Year_Birth) then
            Boy_3 => Stud
         end if

         ! Если ещё остались студенты, сканируем дальше
         if (Associated(Stud%next)) &
            call Get_Boys_Peter(Stud%next, Boy_1, Boy_2, Boy_3, Gender, Regis)
         ! Если ещё остались студенты, сканируем дальше
      else if (Associated(Stud%next)) then
         call Get_Boys_Peter(Stud%next, Boy_1, Boy_2, Boy_3, Gender, Regis)
      end if
   end subroutine Get_Boys_Peter
end module group_process
