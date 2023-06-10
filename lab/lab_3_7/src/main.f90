program reference_lab_3_7
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:), allocatable :: output_file, F1, F2

   type(word), allocatable   :: Words
   type(word), allocatable   :: Del_Words

   F1 = "../data/words.txt"
   F2 = "../data/del_words.txt" 
   output_file = "output.txt"
   
   Words = Read_list(F1)
   Del_Words = Read_List(F2)

   call Output_list(output_file, Words, "Исходный список слов:", "rewind")
   call Output_list(output_file, Del_Words, "Список слов для удаления:", "append")
  
   call Delete(Words, Del_Words)
   
   call Output_list(output_file, Words, "После удаления:", "append")
end program reference_lab_3_7
