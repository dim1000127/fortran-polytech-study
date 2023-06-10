module List_IO
   use Environment

   implicit none
   integer, parameter               :: SURNAME_LEN = 15, REC_LEN = 20

   type student
      character(SURNAME_LEN, kind=CH_) :: surname = ""
      integer(I_)                      :: number = 0
      type(student), allocatable       :: next
   end type student

contains
   function Read_list(Input_File) result(List)
      type(student), allocatable   :: List
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
      type(student), allocatable  :: Elem
      integer, intent(in)          :: In
      integer  :: IO

      character(REC_LEN, kind=CH_)        :: string
      character(SURNAME_LEN, kind=CH_)    :: surname = ""
      integer                             :: number = 0
      
      read (In, '(a)', iostat=IO) string
      if (IO == 0) then
          read(string, '((i4, 1x), (a, 1x))', iostat=IO) number, surname
          if(IO == 0) then
             allocate (Elem, source=student(number=number, surname=surname))
             call Read_value(In, Elem%next)
          else
            read (string, '(a, 1x)', iostat=IO) surname
            if(IO == 0) then
               allocate (Elem, source=student(surname=surname))
               call Read_value(In, Elem%next)
            end if
          end if
      end if
   end subroutine Read_value
   
   ! Вывод списка.
   subroutine Output_list(Output_File, List, List_Name, Position)
      character(*), intent(in)      :: Output_File, Position, List_Name
      type(student), allocatable, intent(in)    :: List
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
      type(student), allocatable, intent(in) :: Stud
      
      integer  :: IO

      if (allocated(Stud)) then 
         write (Out, '((i0, 1x), a)', iostat=IO) Stud%number, Stud%surname
         call Handle_IO_status(IO, "Writing student")
         call Output_value(Out, Stud%next)
      end if
   end subroutine Output_value
end module List_IO 
