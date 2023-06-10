module List_IO
   use Environment

   implicit none
   integer, parameter               :: WORD_LEN = 20, REC_LEN = 20

   type word
      character(WORD_LEN, kind=CH_) :: string = ""
      type(word), allocatable       :: next
   end type word

contains
   function Read_list(Input_File) result(List)
      type(word), allocatable   :: List
      character(*), intent(in)      :: Input_File
      integer  :: In

      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Input_File, encoding=E_, newunit=In)
      open (file=Input_File, newunit=In)
        call Read_value(In, List)
      close (In)
   end function Read_list

   ! Чтение следующего значения.
   recursive subroutine Read_value(In, Elem)
      type(word), allocatable  :: Elem
      integer, intent(in)          :: In
      integer  :: IO

      character(REC_LEN, kind=CH_)        :: str
      character(WORD_LEN, kind=CH_)    :: surname = ""
      
      read (In, '(a)', iostat=IO) str
      if (IO == 0) then
         read (str, '(a, 1x)', iostat=IO) surname
         if(IO == 0) then
            allocate (Elem, source=word(string=str))
            call Read_value(In, Elem%next)
         end if
      end if
   end subroutine Read_value
   
   ! Вывод списка.
   subroutine Output_list(Output_File, List, List_Name, Position)
      character(*), intent(in)      :: Output_File, Position, List_Name
      type(word), allocatable, intent(in)    :: List
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value(Out, List)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_value(Out, Stud)
      integer, intent(in)        :: Out
      type(word), allocatable, intent(in) :: Stud
      
      integer  :: IO

      if (allocated(Stud)) then 
         write (Out, '(a)', iostat=IO) Stud%string
         call Handle_IO_status(IO, "Writing student")
         call Output_value(Out, Stud%next)
      end if
   end subroutine Output_value
end module List_IO 
