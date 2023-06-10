module Group_IO
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 360000, SURNAME_LEN = 15, INITIALS_LEN = 5, BOYS_AMOUNT = 3
 
   type student
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
      character(kind=CH_)                 :: Registration         = ""
      character(kind=CH_)                 :: Gender               = ""
      integer(I_)                         :: Year_Birth           = 0
   end type student

contains
   ! Создание неформатированного файла данных.
   subroutine Create_Data_File(input_File, data_File)
      character(*), intent(in)   :: input_File, data_file
      
      type(student)              :: stud
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=input_File, encoding=E_, newunit=In)
      recl = (SURNAME_LEN + INITIALS_LEN + 2)*CH_ + I_
      open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = "(4(a, 1x), i4)"
         do i = 1, STUD_AMOUNT
            read (In, format, iostat=IO) stud
            call Handle_IO_status(IO, "Reading formatted class list, line " // i)
            
            write (Out, iostat=IO, rec=i) stud
            call Handle_IO_status(IO, "Creating unformatted file with class list, record " // i)
         end do
      close (In)
      close (Out)
   end subroutine Create_Data_File

   function Read_Class_List(data_file) result(Group)
      type(student),allocatable  :: Group(:)
      character(*), intent(in)   :: data_file

      integer In, IO, recl
      
      allocate(Group(STUD_AMOUNT))
      recl = ((SURNAME_LEN + INITIALS_LEN + 2)*CH_ + I_) * STUD_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) Group
         call Handle_IO_status(IO, "Reading unformatted class list")
      close (In)
   end function Read_Class_List

   subroutine Output_Class_Data(output_file, Group, List_name, Position)
      character(*), intent(in)   :: output_file, List_name, Position
      type(student), intent(in)  :: Group(:)

      integer                    :: Out, IO
      character(:), allocatable  :: format
      
      open (file=output_file, encoding=E_, position=Position, newunit=Out)
         write (out, '(a)') List_name
         format = "(4(a, 1x), i4)"
         write (Out, format, iostat=IO) Group
         call Handle_IO_status(IO, "Writing " //List_name)
      close (Out)
   end subroutine Output_Class_Data
end module Group_IO 
