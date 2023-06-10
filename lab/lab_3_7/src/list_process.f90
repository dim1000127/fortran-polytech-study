module List_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use List_IO

   implicit none

contains
   pure recursive subroutine Delete(Words, Del_Words)
      type(word), allocatable, intent(inout) :: Words, Del_Words    

      if (allocated(Words) .and. allocated(Del_Words)) then
         call Delete_Word(Words, Del_Words)
         call Delete(Words, Del_Words%next)
      end if
   end subroutine Delete

   pure recursive subroutine Delete_Word(Current_Word, Del_Word)
      type(word), allocatable, intent(inout) :: Current_Word, Del_Word 
      type(word), allocatable :: tmp

      if (allocated(Current_Word) .and. allocated(Del_Word)) then
         if (Del_Word%string == Current_Word%string) then
            call move_alloc(Current_Word%next, tmp)
            call move_alloc(tmp, Current_Word)
            call Delete_Word(Current_Word, Del_Word)
         else
            call Delete_Word(Current_Word%next, Del_Word)
         end if
      end if
   end subroutine Delete_Word
end module List_Process
