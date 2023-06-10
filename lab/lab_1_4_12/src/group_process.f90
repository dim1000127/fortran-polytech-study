module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains
   !Поиск трех наиболее молодых петербуржцев мужчин
   subroutine Search_Male_Regis(Group, Boys, Gender_Sym, Regis_Sym) 
      type(student)                 :: Group
      type(boy)                     :: Boys 
      character(kind=CH_)           :: Gender_Sym, Regis_Sym
      
      intent(in)                    :: Group, Gender_Sym, Regis_Sym
      intent(out)                   :: Boys
     
      integer                       :: i, Ind_Boy
      logical, allocatable          :: Is_A_Boy_Peter(:)

      Is_A_Boy_Peter = (Group%Gender == Gender_Sym .and. Group%Registration == Regis_Sym) 

      do i = 1, BOYS_AMOUNT
         Ind_Boy = MaxLoc(Group%Year_Birth, 1, Is_A_Boy_Peter)
         
         Boys%Surname(i) = Group%Surname(Ind_Boy)
         Boys%Initials(i) = Group%Initials(Ind_Boy)
         Boys%Year_Birth(i) = Group%Year_Birth(Ind_Boy)
         Is_A_Boy_Peter(Ind_Boy) = .false.
      end do
   end subroutine Search_Male_Regis
end module group_process
