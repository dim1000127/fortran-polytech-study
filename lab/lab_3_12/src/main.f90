program reference_lab_3_12
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:), allocatable :: output_file, F1, F2

   type(student), allocatable   :: Sorted_Students_List
   type(student), allocatable   :: Students_List

   F1 = "../data/sort_students.txt"
   F2 = "../data/students.txt" 
   output_file = "output.txt"
   
   Sorted_Students_List = Read_list(F1)
   Students_List = Read_List(F2)

   call Output_list(output_file, Sorted_Students_List, "Исходный список:", "rewind")
  
   call Insert(Sorted_Students_List, Students_List)
   call Update_Numerable(Sorted_Students_List, 1)
   
   call Output_list(output_file, Sorted_Students_List, "После добавления новых студентов:", "append")
end program reference_lab_3_12
