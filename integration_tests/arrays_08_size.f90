program arrays_08_size
   integer :: y = 2
   call temp(y)

contains
   subroutine temp(x)
        integer, intent(inout) :: x
        logical :: keep(x)
        integer :: result(x)
        keep = [.false., .true.]
        x = 1
        result = trueloc(keep)
        print * , result
        if (result(1) /= 2) error stop
   end subroutine
   function trueloc(x) result(loc)
        logical, intent(in) :: x(:)
        integer(4), allocatable :: loc(:)
        integer :: i, j
        allocate(loc(count(x)))
        j = 1
        do i = 1, size(x)
            if( x(i) ) then
                loc(j) = i
                j = j + 1
            end if
        end do
    end function trueloc

end program