program lab_1_2_12
   use Environment
   use Group_IO
   use Group_Process

   implicit none
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETER = Char(1055, CH_)

   character(:), allocatable  :: input_file, output_file
   
   !У массивов Surnames и Initials индексы поменяны местами, чтобы
   !чтобы при выборе строки, она была сплошной
   character(kind=CH_),allocatable    :: Surnames(:, :), &
                                         Initials(:, :), &
                                         Registration(:), &
                                         Genders(:)
   integer, allocatable               :: Year_Birth(:)
   character(kind=CH_), allocatable   :: Boys_Surnames(:, :), Boys_Initials(:, :)
   integer, allocatable               :: Boys_Year_Birth(:)

   real  :: start = 0, finish = 0
   input_file = "../data/class1.txt"
   output_file = "output.txt"

   allocate (Surnames(SURNAME_LEN, STUD_AMOUNT), Initials(INITIALS_LEN, STUD_AMOUNT), &
      Registration(STUD_AMOUNT), Genders(STUD_AMOUNT), Year_Birth(STUD_AMOUNT))
   call Read_Class_Data(input_file, Surnames, Initials, Registration, Genders, Year_Birth)

   call Output_Class_Data(output_file, Surnames, Initials, Registration, Genders, Year_Birth, &
      "Исходный список:")   

   allocate(Boys_Surnames(SURNAME_LEN, BOYS_AMOUNT), Boys_Initials(INITIALS_LEN, BOYS_AMOUNT), &
               Boys_Year_Birth(BOYS_AMOUNT))
  
   call cpu_time(start)
   call Search_Male(Surnames, Initials, Year_Birth, Registration, Genders, &
      Boys_Surnames, Boys_Initials, Boys_Year_Birth, MALE, PETER)
   call cpu_time(finish)
   print '("Time = ", f0.9)', (finish-start)

   call Output_Boy_Peter_Data(output_file, Boys_Surnames, Boys_Initials, Boys_Year_Birth, &
      "Список самых молодых юношей петербуржцев:", "append")
end program lab_1_2_12
