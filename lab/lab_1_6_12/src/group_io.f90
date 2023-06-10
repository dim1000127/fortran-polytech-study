module Group_IO
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 10000, SURNAME_LEN = 15, INITIALS_LEN = 5, BOYS_AMOUNT = 3
 
   type student
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
      character(kind=CH_)                 :: Registration         = ""
      character(kind=CH_)                 :: Gender               = ""
      integer(I_)                         :: Year_Birth           = 0
      type (student), pointer             :: next                 => Null()
   end type student

contains
   function Read_Class_List(input_file) result(Group_List)
      type(student), pointer     :: Group_List
      character(*), intent(in)   :: input_file
      
      integer  In
      
      open (file=Input_File, encoding=E_, newunit=In)
         Group_List => Read_student(In)
      close (In)
   end function Read_Class_List

   recursive function Read_student(In) result(Stud)
      type(student), pointer  :: Stud
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (Stud)
      format = '(4(a, 1x), i4)'
      read (In, format, iostat=IO) Stud%Surname, Stud%Initials, Stud%Registration, Stud%Gender, Stud%Year_Birth
      call Handle_IO_status(IO, "Reading line from file")
      if (IO == 0) then
          Stud%next => Read_student(In)
      else
         deallocate (Stud)
      end if
   end function Read_student
   
   subroutine Output_Class_List(output_file, Group_List, List_name, Position)
      character(*), intent(in)   :: output_file, List_name, Position
      type(student), intent(in)  :: Group_List

      integer                    :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_student(Out, Group_List)
      close (Out)
   end subroutine Output_Class_List
   
   recursive subroutine Output_student(Out, Stud)
      integer, intent(in)        :: Out
      type(student), intent(in)  :: Stud
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(4(a, 1x), i4)'
      write (Out, format, iostat=IO) Stud%Surname, Stud%Initials, Stud%Registration, Stud%Gender, Stud%Year_Birth
      call Handle_IO_status(IO, "Writing student")
      if (Associated(Stud%next)) &
         call Output_student(Out, Stud%next)
   end subroutine Output_student

   subroutine Output_List_Name(output_file, List_Name, Position)
      character(*), intent(in)   :: output_file, List_Name, Position
   
      integer                    :: Out, IO

      open (file=output_file, encoding=E_, position=Position, newunit=Out)
         write(Out, '(/a)', iostat=IO) List_Name   
         call Handle_IO_Status(IO, "Writting ListName")
      close(Out)
   end subroutine Output_List_Name
   
   subroutine Output_Boy(output_file, Boy, Position)
      character(*), intent(in)   :: output_file, Position
      type(student), intent(in)  :: Boy
   
      integer                    :: Out, IO
      character(:), allocatable  :: format

      format = '(4(a, 1x), i4)'
      
      open (file=output_file, encoding=E_, position=Position, newunit=Out)
         write (Out, format, iostat=IO) Boy%Surname, Boy%Initials, Boy%Registration, & 
            Boy%Gender, Boy%Year_Birth
         call Handle_IO_Status(IO, "Writting boy")
      close(Out)
   end subroutine Output_Boy
   
   subroutine Output_Boys(output_file, Boys, List_Name, Position)
      character(*), intent(in)   :: output_file, List_name, Position
      type(student), intent(in)  :: Boys(:)
   
      integer                    :: Out, IO, i
      character(:), allocatable  :: format

      format = '(4(a, 1x), i4)'
      
      open (file=output_file, encoding=E_, position=Position, newunit=Out)
         write(Out, '(/a)') List_Name   
         write (Out, format, iostat=IO) (Boys(i)%Surname, Boys(i)%Initials, Boys(i)%Registration, & 
            Boys(i)%Gender, Boys(i)%Year_Birth, i = 1, BOYS_AMOUNT)
         call Handle_IO_Status(IO, "Writting boys")
      close(Out)
   end subroutine Output_Boys
end module Group_IO 
