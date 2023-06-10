program lab_1_6_12
   use Environment
   use Group_IO
   use Group_Process

   implicit none
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETER = Char(1055, CH_)

   character(:), allocatable  :: input_file, output_file
   type(student), pointer     :: Group_List => Null()  

   type(student), pointer     :: Boy_1 => Null()
   type(student), pointer     :: Boy_2 => Null()
   type(student), pointer     :: Boy_3 => Null()

   real :: start = 0, finish = 0

   input_file  = "../data/class1.txt"
   output_file = "output.txt"

   Group_List => Read_Class_List(input_file)

   if (Associated(Group_List)) then
      call Output_Class_List(output_file, Group_List, "Исходный список:", "rewind")
      
      call cpu_time(start)
      call Get_Boys_Peter(Group_List, Boy_1, Boy_2, Boy_3, MALE, PETER)
      call cpu_time(finish)
      print '("Time = ", f0.9)', (finish-start)

      call Output_List_Name(output_file, "Список самых молодых юношей петербуржцев:", "append")
      if (Associated(Boy_1)) &
         call Output_Boy (output_file, Boy_1, "append")

      if (Associated(Boy_2)) &
         call Output_Boy (output_file, Boy_2, "append")
      
      if (Associated(Boy_3)) &
         call Output_Boy (output_file, Boy_3, "append")
      !call Output_Boys(output_file, Boys, &
      !   "Список самых молодых юношей петербуржцев:", "append")
   end if
end program lab_1_6_12
