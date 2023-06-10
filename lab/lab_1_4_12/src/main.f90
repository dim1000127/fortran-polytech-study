program lab_1_4_12
   use Environment
   use Group_IO
   use Group_Process

   implicit none
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETER = Char(1055, CH_)

   character(:), allocatable  :: input_file, output_file, data_file
   type(student)              :: Group
   type(boy)                  :: Boys
   real :: start = 0, finish = 0

   input_file  = "../data/class1.txt"
   output_file = "output.txt"
   data_file   = "class.dat"

   call Create_Data_File(input_file, data_file)
   
   Group = Read_Class_List(data_file)

   call Output_Class_Data(output_file, Group, "Исходный список:")   

  ! allocate(Boys%Surname(BOYS_AMOUNT), Boys%Initials(BOYS_AMOUNT), Boys%Year_Birth(BOYS_AMOUNT))
   call cpu_time(start)
   call Search_Male_Regis(Group, Boys, MALE, PETER)
   call cpu_time(finish)
   print '("Time = ", f0.9)', (finish-start)

   call Output_Boy_Peter_Data(output_file, Boys, &
      "Список самых молодых юношей петербуржцев:", "append")
end program lab_1_4_12
