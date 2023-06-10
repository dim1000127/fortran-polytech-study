module List_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use List_IO

   implicit none

contains
   pure recursive subroutine Insert(Sorted_Students_List, Students_List)
      type(student), allocatable, intent(inout) :: Sorted_Students_List, Students_List    
      type(student), allocatable :: tmp

      if (allocated(Sorted_Students_List) .and. allocated(Students_List)) then
         call move_alloc(Students_List%next, tmp)
         call Insert_Student(Sorted_Students_List, Students_List)
         call Insert(Sorted_Students_List, tmp)
      end if
   end subroutine Insert

   pure recursive subroutine Insert_Student(Current_Stud, Stud)
      type(student), allocatable, intent(inout) :: Current_Stud, stud 
      !type(student), allocatable :: tmp

      if (allocated(Current_Stud) .and. allocated(Stud)) then
         if (Stud%surname <= Current_Stud%surname) then
            !allocate (tmp)
            !tmp%surname = Stud%surname
            call move_alloc (Current_stud, Stud%next)
            call move_alloc (Stud, Current_Stud)
         else if (.not. Allocated(Current_Stud%next)) then
            !allocate (tmp)
            !tmp%surname = Stud%surname
            call move_alloc(Stud, Current_Stud%next)
         else
            call Insert_Student(Current_Stud%next, Stud)
         end if
      end if
   end subroutine Insert_Student

   pure recursive subroutine Update_Numerable(Stud, Index)
      type (student), allocatable, intent(inout) :: Stud
      integer, intent(in)          :: Index

      if (Allocated(Stud)) then
         Stud%number = Index
         call Update_Numerable(Stud%next, Index + 1)
      end if
   end subroutine Update_Numerable
end module List_Process
