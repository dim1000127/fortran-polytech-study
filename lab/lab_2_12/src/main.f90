program lab_2_12
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, F3

   type(LineElement), allocatable :: SourceLine
   type(LineElement), allocatable :: MaskLine

   integer                        :: Index_entry

   F1 = "../data/source_line.txt"
   F2 = "../data/mask_line.txt"
   F3 = "output.txt"

   SourceLine = Read_Line(F1)
   call Output_Line(F3, SourceLine, "Исходная строка", "rewind")
   
   MaskLine = Read_Line(F2)
   call Output_Line(F3, MaskLine, "Искомая строка", "append")

   
   Index_Entry = Search(SourceLine, MaskLine, 1)
   !call Search(SourceLine, MaskLine, Index_Entry)
   call Output_Index_Entry(F3, Index_Entry, "append")

end program lab_2_12
