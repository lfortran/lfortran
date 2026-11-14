module intent_out_inherited_alloc_cleanup_01_m
    implicit none
    type :: base
        integer, allocatable :: payload(:)
    end type
    type, extends(base) :: middle
        integer, allocatable :: more(:)
    end type
    type, extends(middle) :: child
        integer :: tag
    end type
    type :: host
        type(child), allocatable :: owned
    end type
contains
    subroutine reset(h)
        type(host), intent(out) :: h
    end subroutine
end module

program intent_out_inherited_alloc_cleanup_01
    use intent_out_inherited_alloc_cleanup_01_m
    implicit none
    type(host) :: h
    integer :: i
    do i = 1, 2
        allocate(h%owned)
        allocate(h%owned%payload(i))
        allocate(h%owned%more(i + 1))
        h%owned%payload = 11
        h%owned%more = 12
        h%owned%tag = 42
        if (h%owned%payload(1) /= 11 .or. h%owned%more(1) /= 12) error stop 1
        if (h%owned%tag /= 42) error stop 2
        call reset(h)
        if (allocated(h%owned)) error stop 3
    end do
end program
