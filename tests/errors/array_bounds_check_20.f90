program array_bounds_check_20
    implicit none
    type :: alloc_st
        integer, allocatable :: arr(:)
    end type
    type(alloc_st) :: t(3)
    integer :: i
    forall (i = 1:3) t(i)%arr = [i, 2*i]
    print *, t(1)%arr
end program
