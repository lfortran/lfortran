program where_07
   implicit none

   integer :: call_num

   logical(4), dimension(4) :: l

   ! l = [.true., .false., .true., .false.]
   ! call_num = 1

   ! neqv operator
   where (l .neqv. .true.)
      l = .true.
   end where

   ! print *, l
   ! IF (all(l .neqv. [.true., .true., .true., .true.])) ERROR STOP

   ! ! or operator
   ! where (l .or. .false.)
   !    l = get_boolean_false()
   ! end where

   ! print *, l
   ! IF (all(l .neqv. [.false., .true., .false., .true.])) ERROR STOP

   ! ! and operator
   ! where (l .and. get_boolean_true())
   !    l = .false.
   ! end where

   ! print *, l
   ! IF (all(l .neqv. [.false., .false., .false., .false.])) ERROR STOP

   ! ! eqv operator
   ! where (l .eqv. get_boolean_false())
   !    l = .true.
   ! end where

   ! print *, l
   ! IF (all(l .neqv. [.true., .false., .true., .true.])) ERROR STOP

contains

   pure logical function get_boolean_true() result(value)
      implicit none
      value = .true.
   end function get_boolean_true

   pure logical function get_boolean_false() result(value)
      implicit none
      value = .false.
   end function get_boolean_false

end program where_07
