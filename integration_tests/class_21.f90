module class_21_mod
    type :: val_type
        integer :: origin = 3
    end type
end module
program class_21
    use class_21_mod
    integer :: stat
    class(val_type), allocatable :: val
    logical :: temp = .false.
    type(val_type), parameter :: val_par = val_type()
    type(val_type) :: tmp
    allocate(val)
    stat = merge(val%origin, stat, .true.)
    tmp = val_type(merge(val%origin - 1, stat, any([stat, val%origin] /= 0)))
    if (stat /= 3) error stop
    stat = merge(val_par%origin, stat - 1, temp)
    if (stat /= 2) error stop
    if (tmp%origin /= 2) error stop
    call sub1()
contains 
    subroutine sub1()
        call sub2(val_par%origin)
    end subroutine
    subroutine sub2(x)
        integer :: x
        if (x /= 3) error stop
    end  subroutine
end program
