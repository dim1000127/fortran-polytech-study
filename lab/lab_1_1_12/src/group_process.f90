module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains
   ! Поиск трех наиболее молодых петербуржцев мужчин
   pure subroutine Search_Male(Surnames, Initials, Year_Birth, Registration, Genders, &
         Boys_Surnames, Boys_Initials, Boys_Year_Birth, Gender_Sym, Regis_Sym) 
      character(SURNAME_LEN, kind=CH_)           :: Surnames(:), Boys_Surnames(:)
      character(INITIALS_LEN, kind=CH_)          :: Initials(:), Boys_Initials(:)
      character(kind=CH_)                        :: Registration(:), Genders(:)
      integer                                    :: Year_Birth(:), Boys_Year_Birth(:)
      character(kind=CH_)                        :: Gender_Sym, Regis_Sym
      intent(in)                                 :: Surnames, Initials, Year_Birth, Registration,&
                                                    Genders, Gender_Sym, Regis_Sym
      intent(out)                                :: Boys_Surnames, Boys_Initials, Boys_Year_Birth
     
      integer                                    :: i, Ind_Boy
      logical, allocatable                       :: Is_A_Boy_Peter(:)
      
      Is_A_Boy_Peter = (Genders == Gender_Sym .and. Registration == Regis_Sym) 

      do i = 1, BOYS_AMOUNT
         Ind_Boy = MaxLoc(Year_Birth, 1, Is_A_Boy_Peter)
          
         Boys_Surnames(i) = Surnames(Ind_Boy)
         Boys_Initials(i) = Initials(Ind_Boy)
         Boys_Year_Birth(i) = Year_Birth(Ind_Boy)
         Is_A_Boy_Peter(Ind_Boy) = .false.
      end do
   end subroutine Search_Male
end module group_process
