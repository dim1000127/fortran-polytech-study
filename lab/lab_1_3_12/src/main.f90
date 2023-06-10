program lab_1_3_12
   use Environment
   use Group_IO
   use Group_Process

   implicit none
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETER = Char(1055, CH_)

   character(:), allocatable  :: input_file, output_file, data_file
   type(student), allocatable :: Group(:)      
   type(student), allocatable :: Boys(:)
   real           :: start = 0, finish = 0

   input_file  = "../data/class1.txt"
   output_file = "output.txt"
   data_file   = "class.dat"

   call Create_Data_File(input_file, data_file)
   
   Group = Read_Class_List(data_file)

   call Output_Class_Data(output_file, Group, "Исходный список:", "rewind")   

   allocate(Boys(BOYS_AMOUNT))
   call cpu_time(start)
   call Search_Male_Regis(Group, Boys, MALE, PETER)
   call cpu_time(finish)
   print '("Time = ", f0.9)', (finish-start)

   call Output_Class_Data(output_file, Boys, &
      "Список самых молодых юношей петербуржцев:", "append")
end program lab_1_3_12
