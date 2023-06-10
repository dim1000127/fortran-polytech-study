module Group_IO
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 360000, SURNAME_LEN = 15, INITIALS_LEN = 5, BOYS_AMOUNT = 3

contains
   subroutine Read_Class_Data(input_file, Surnames, Initials, Registration, Genders, Year_Birth)
      character(*), intent(in)                       :: input_file
      character(SURNAME_LEN, kind=CH_), intent(out)  :: Surnames(:)
      character(INITIALS_LEN, kind=CH_), intent(out) :: Initials(:)
      character(kind=CH_), intent(out)               :: Registration(:), Genders(:)
      integer, intent(out)                           :: Year_Birth(:)

      integer                    :: In, IO, i
      character(:), allocatable  :: format
     
      open (file=input_file, encoding=E_, newunit=In)
         format = "(4(a, 1x), i4)"
         read (In, format, iostat=IO) (Surnames(i), Initials(i), Registration(i), Genders(i), Year_Birth(i), i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "Reading class list")
      close(In)
   end subroutine Read_Class_Data

   subroutine Output_Class_Data(output_file, Surnames, Initials, Registration, Genders, Year_Birth, List_name)   
      character(*), intent(in)                       :: output_file, List_name
      character(SURNAME_LEN, kind=CH_), intent(in)   :: Surnames(:)
      character(INITIALS_LEN, kind=CH_), intent(in)  :: Initials(:)
      character(kind=CH_), intent(in)                :: Registration(:), Genders(:)
      integer, intent(in)                            :: Year_Birth(:)
      
      integer                    :: Out, IO, i
      character(:), allocatable  :: format

      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, '(a)') List_name
         format = "(4(a, 1x), i4)"
         write (Out, format, iostat=IO) (Surnames(i), Initials(i), Registration(i), Genders(i), Year_Birth(i), i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "Writting " //List_name)  
      close (Out)
   end subroutine Output_Class_Data

   subroutine Output_Boy_Peter_Data(output_file, Boys_Surnames, Boys_Initials, Boys_Year_Birth, &
         List_name, Position)
      character(*), intent(in)                       :: output_file, List_name, Position
      character(SURNAME_LEN, kind=CH_), intent(in)   :: Boys_Surnames(:)
      character(INITIALS_LEN, kind=CH_), intent(in)  :: Boys_Initials(:)
      integer, intent(in)                            :: Boys_Year_Birth(:)
      
      integer                    :: Out, IO, i
      character(:), allocatable  :: format
      
      open (file=output_file, encoding=E_, newunit=Out, position=Position)
         write (Out, '(a)') List_name
         format = "(2(a, 1x), i4)"
         write (Out, format, iostat=IO) (Boys_Surnames(i), Boys_Initials(i), Boys_Year_Birth(i), i = 1, BOYS_AMOUNT)
         call Handle_IO_status(IO, "Writting " //List_name)  
      close (Out)
   end subroutine Output_Boy_Peter_Data 
end module Group_IO 
