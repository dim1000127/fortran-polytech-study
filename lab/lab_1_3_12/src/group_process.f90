module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains
   ! Поиск трех наиболее молодых петербуржцев мужчин
   pure subroutine Search_Male_Regis(Group, Boys, Gender_Sym, Regis_Sym) 
      type(student)                 :: Group(:), Boys(:)
      character(kind=CH_)           :: Gender_Sym, Regis_Sym
      
      intent(in)                    :: Group, Gender_Sym, Regis_Sym
      intent(out)                   :: Boys
     
      integer                       :: i, Ind_Boy
      logical, allocatable          :: Is_A_Boy_Peter(:)

      Is_A_Boy_Peter = (Group%Registration == Regis_Sym .and. Group%Gender == Gender_Sym) 

      do i = 1, BOYS_AMOUNT
         Ind_Boy = MaxLoc(Group%Year_Birth, 1, Is_A_Boy_Peter)
          
         Boys(i) = Group(Ind_Boy)
         Is_A_Boy_Peter(Ind_Boy) = .false.
      end do
   end subroutine Search_Male_Regis
end module group_process
