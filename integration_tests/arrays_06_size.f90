program arrays_06_size 
   integer :: x = 2
   call temp(x)

contains
   subroutine temp(x)
     integer, intent(inout) :: x
     integer :: keep(x)
     keep = [1,2]
     print *, keep
     if (keep(1) /= 1) error stop
     if (keep(2) /= 2) error stop
     x = 1
     print *, keep
     if (keep(1) /= 1) error stop
     if (keep(2) /= 2) error stop
   end subroutine
end program