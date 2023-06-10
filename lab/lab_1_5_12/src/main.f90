program lab_1_5_12
   use Environment
   use Group_IO
   use Group_Process

   implicit none
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETER = Char(1055, CH_)

   character(:), allocatable  :: input_file, output_file, data_file
   !type(student), allocatable  :: Group(:)      
   !type(student), allocatable :: Boys(:)

   type(student)              :: Group
   type(boy)                  :: Boys   

   logical, allocatable       :: Is_A_Boy_Peter(:)
   integer                    :: Count_By_Mask
   real                       :: start = 0, finish = 0

   input_file  = "../data/class1.txt"
   output_file = "output.txt"
   data_file   = "class.dat"

   call Create_Data_File(input_file, data_file)
   
   Group = Read_Class_List(data_file)

   call Output_Class_Data(output_file, Group, "Исходный список:")   


   Is_A_Boy_Peter = (Group%Gender == MALE .and. Group%Registration == PETER) 
   Count_By_Mask = Count(Is_A_Boy_Peter)

   !if (Count_By_Mask >= BOYS_AMOUNT) then
      !allocate(Boys(BOYS_AMOUNT))
   !else
      !allocate(Boys(Count_By_Mask))
   !end if

   call cpu_time(start)
   call Students_By_Mask(Group, Boys, Is_A_Boy_Peter, 1)
   call cpu_time(finish)
   print '("Time = ", f0.9)', (finish-start)
   
   call Output_Boy_Peter_Data(output_file, Boys, &
      "Список самых молодых юношей петербуржцев:", "append")
end program lab_1_5_12
