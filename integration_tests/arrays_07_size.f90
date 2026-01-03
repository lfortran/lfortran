program arrays_07_size
   integer :: y = 2
   call temp(y)

contains
   subroutine temp(x)
     integer, intent(inout) :: x
     integer :: keep(x)
     keep = [1,2]
     x = 1
     print * , size(keep)
     print * , keep
     if (size(keep) /= 2) error stop
     if (keep(1) /= 1) error stop
     if (keep(2) /= 2) error stop
   end subroutine
end program
