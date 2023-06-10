module Group_IO
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 360000, SURNAME_LEN = 15, INITIALS_LEN = 5, BOYS_AMOUNT = 3
 
   type student
      character(SURNAME_LEN, kind=CH_),allocatable   :: Surname(:)      
      character(INITIALS_LEN, kind=CH_), allocatable :: Initials(:)     
      character(kind=CH_),allocatable                :: Registration(:) 
      character(kind=CH_),allocatable                :: Gender(:)       
      integer(I_),allocatable                        :: Year_Birth(:)   
   end type student

   type boy 
      character(SURNAME_LEN, kind=CH_)    :: Surname(BOYS_AMOUNT)  = ""     
      character(INITIALS_LEN, kind=CH_)   :: Initials(BOYS_AMOUNT) = ""     
      integer(I_)                         :: Year_Birth(BOYS_AMOUNT) = 0   
   end type boy

contains
   ! Создание неформатированного файла данных.
   subroutine Create_Data_File(input_File, data_File)
      character(*), intent(in)          :: input_File, data_file
      
      character(SURNAME_LEN, kind=CH_)  :: Surnames(:)
      character(INITIALS_LEN, kind=CH_) :: Initials(:)
      character(kind=CH_)               :: Registration(:), Genders(:)
      integer                           :: Year_Birth(:)
      integer                           :: In, Out, IO, i
      character(:)                      :: format
      allocatable                       :: format, Surnames, Initials, Registration, &
                                           Genders, Year_Birth
     
      allocate (Surnames(STUD_AMOUNT), Initials(STUD_AMOUNT), Registration(STUD_AMOUNT), &
                Genders(STUD_AMOUNT), Year_Birth(STUD_AMOUNT))

      open (file=input_File, encoding=E_, newunit=In)
         format = "(4(a, 1x), i4)"
         read (In, format, iostat=IO) (Surnames(i), Initials(i), Registration(i), Genders(i), Year_Birth(i),&
            i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "Reading class list")
      close (In)
      
      open (file=Data_File, form='unformatted', newunit=Out, access='stream')
          write (Out, iostat=IO) Surnames, Initials, Registration, Genders, Year_Birth
          call Handle_IO_status(IO, "Creating unformatted file with class list")
      close (Out)
   end subroutine Create_Data_File

   function Read_Class_List(data_file) result(Group)
      type(student)              ::  Group
      character(*), intent(in)   :: data_file

      integer In, IO

      allocate(Group%Surname(STUD_AMOUNT), Group%Initials(STUD_AMOUNT), &
               Group%Registration(STUD_AMOUNT), Group%Gender(STUD_AMOUNT), Group%Year_Birth(STUD_AMOUNT))
     
      open (file=Data_File, form='unformatted', newunit=In, access='stream')
         read (In, iostat=IO) Group%Surname, Group%Initials, Group%Registration, Group%Gender, Group%Year_Birth
         call Handle_IO_status(IO, "Reading unformatted class list")
      close (In)
   end function Read_Class_List

   
   subroutine Output_Class_Data(output_file, Group, List_name)
      character(*), intent(in)   :: output_file, List_name
      type(student), intent(in)  :: Group

      integer                    :: Out, IO, i
      character(:), allocatable  :: format
      
      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, '(a)') List_name
         format = '(4(a, 1x), i4)'
         write (Out, format, iostat=IO) (Group%Surname(i), Group%Initials(i), Group%Registration(i), &
            Group%Gender(i), Group%Year_Birth(i), i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "Writing " //List_name)
      close (Out)
   end subroutine Output_Class_Data

   subroutine Output_Boy_Peter_Data(output_file, Boys, List_name, Position)
      character(*), intent(in)    :: output_file, List_name, Position
      type(boy), intent(in)   :: Boys
      
      integer                     :: Out, IO, i
      character(:), allocatable   :: format
      
      open (file=output_file, encoding=E_, newunit=Out, position=Position)
         write (Out, '(a)') List_name
         format = "(2(a, 1x), i4)"
         write (Out, format, iostat=IO) (Boys%Surname(i), Boys%Initials(i), Boys%Year_Birth(i), i = 1, BOYS_AMOUNT)
         call Handle_IO_status(IO, "Writting " //List_name)  
      close (Out)
   end subroutine Output_Boy_Peter_Data 
end module Group_IO 
