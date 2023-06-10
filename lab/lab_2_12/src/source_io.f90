module Source_IO
   use Environment

   implicit none
   !Структура данных для хранения символа строки
   type LineElement
      character(CH_)                   :: Symbol = ""
      type(LineElement), allocatable   :: Next
   end type LineElement

contains
   function Read_Line(input_file) result (Line)
      character(*), intent(in)        :: input_file
      type(LineElement), allocatable  :: Line
      integer :: In

      open (file=input_file, newunit=In)
         call Read_Symbol(In, Line)
      close(In)
   end function Read_Line

   recursive subroutine Read_Symbol(In, Symbol)
      integer, intent(in)              :: In
      type(LineElement), allocatable   :: Symbol
      integer                          :: IO

      allocate(Symbol)
      read(In, "(a1)", iostat=IO, advance="no") Symbol%Symbol 
      call Handle_IO_Status(IO, "Reading symbol from source line")
      if(IO == 0) then
         call Read_Symbol(In, Symbol%Next)
      else
         deallocate(Symbol)
      end if
   end subroutine Read_Symbol

   subroutine Output_Line(output_file, Line, List_Name, Position)
      character(*), intent(in)       :: output_file, List_Name, Position 
      type(LineElement), allocatable :: Line 
      integer  :: Out

      open (file=output_file, position=Position, newunit=Out)
         write (Out, '(/a)') List_Name
         call Output_Symbol(Out, Line)
      close (Out)
   end subroutine Output_Line

   recursive subroutine Output_Symbol(Out, Symbol)
      integer, intent(in)            :: Out
      type(LineElement), allocatable  :: Symbol
      integer  :: IO

      if(allocated(Symbol)) then
         write (Out, "(a1)", iostat=IO, advance="no") Symbol%Symbol
         call Handle_IO_Status(IO, "Writing symbol to file")
         call Output_Symbol(Out, Symbol%Next)
      end if
   end subroutine Output_Symbol
   
   subroutine Output_Index_Entry(output_file, Index_Entry, Position)
      character(*), intent(in)   :: output_file, Position 
      integer, intent(in)        :: Index_Entry  
      integer  :: Out

      open (file=output_file, position=Position, newunit=Out)
         write (Out, '(/a)') "Индекс первого вхождения"
         write (Out, '(i4)') Index_Entry
      close (Out)
   end subroutine Output_Index_Entry
end module Source_IO 
