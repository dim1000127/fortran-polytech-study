module Source_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Source_IO

   implicit none

contains

   pure recursive integer function Search(SourceLine, MaskLine, Index_Entry) result (S)   
      type(LineElement), allocatable  :: SourceLine, MaskLine
      intent(in)                      :: SourceLine, MaskLine

      integer, intent(in)          :: Index_Entry

      if (.not. Is_String_Equal(SourceLine, MaskLine)) then
         if ((Allocated(SourceLine%Next))) then
             S = Search(SourceLine%Next, MaskLine, Index_Entry + 1)
         else
             S = 0
         end if
      else
         S = Index_Entry
      end if
   end function Search
  
   pure function Is_String_Equal(SourceLine, MaskLine) result(Is_Equal)
      type(LineElement), allocatable  :: SourceLine, MaskLine
      intent(in)                      :: SourceLine, MaskLine
   
      logical                         :: Is_Equal

      call Equal(SourceLine, MaskLine, Is_Equal)
   end function Is_String_Equal

   pure recursive subroutine Equal(SourceLine, MaskLine, Is_Equal)
      logical, intent(inout)          :: Is_Equal
      type(LineElement), allocatable  :: SourceLine, MaskLine
      intent(in)                      :: SourceLine, MaskLine

      Is_Equal = .true.
      if (SourceLine%Symbol == MaskLine%Symbol) then
         if(Allocated(MaskLine%Next)) then
            if (Allocated(SourceLine%Next)) then
               call Equal(SourceLine%Next, MaskLine%Next, Is_Equal)
            else
               Is_Equal = .false.
            end if
         end if
      else
         Is_Equal = .false.
      end if
   end subroutine Equal
end module Source_process
