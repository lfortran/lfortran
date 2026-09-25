! The default initialization of module variables of derived types, driven by
! a Fortran main program: the variables are declared in a module other than
! the one that defines their types, so the module's initializer names the
! types' members, initial targets and procedures across modules. Arrays of a
! derived type get their elements' defaults from one loop each, whether or
! not an element needs storage of its own set up. A declaration initializer
! of an array whose element needs none is static data instead, and each
! element is still a copy of its own.
module global_init_12_types
    implicit none
    abstract interface
        integer function iface(x)
            integer, intent(in) :: x
        end function iface
    end interface
    integer, target :: tgt = 5
    type :: pair
        integer :: n = 7
        real :: w = 1.5
    end type
    type :: fixed
        integer :: v(2) = [1, 2]
    end type
    type :: holder
        integer :: n = 1
        procedure(iface), pointer, nopass :: fp => twice
        integer, pointer :: p => tgt
        character(len=3) :: s = "abc"
        type(pair) :: pr
    end type
contains
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function twice
end module global_init_12_types

module global_init_12_vars
    use global_init_12_types, only: pair, fixed, holder
    implicit none
    type(holder) :: h
    type(holder) :: hs(2)
    type(pair) :: grid(2, 3)
    type(fixed) :: fs(3) = fixed([4, 5])
end module global_init_12_vars

program global_init_12
    use global_init_12_types, only: tgt
    use global_init_12_vars
    implicit none
    integer :: i, j
    if (h%n /= 1 .or. h%s /= "abc") error stop 1
    if (.not. associated(h%fp)) error stop 2
    if (h%fp(21) /= 42) error stop 3
    if (.not. associated(h%p, tgt)) error stop 4
    if (h%pr%n /= 7 .or. h%pr%w /= 1.5) error stop 5
    do i = 1, 2
        if (hs(i)%n /= 1 .or. hs(i)%s /= "abc") error stop 6
        if (.not. associated(hs(i)%fp)) error stop 7
        if (hs(i)%fp(i) /= 2*i) error stop 8
        if (.not. associated(hs(i)%p, tgt)) error stop 9
        if (hs(i)%pr%n /= 7) error stop 10
    end do
    do j = 1, 3
        do i = 1, 2
            if (grid(i, j)%n /= 7 .or. grid(i, j)%w /= 1.5) error stop 11
        end do
    end do
    do i = 1, 3
        if (any(fs(i)%v /= [4, 5])) error stop 12
    end do
    fs(2)%v(1) = 40
    if (fs(1)%v(1) /= 4 .or. fs(3)%v(1) /= 4) error stop 13
    hs(1)%s = "xyz"
    if (hs(2)%s /= "abc" .or. h%s /= "abc") error stop 14
    grid(2, 2)%n = 70
    if (grid(1, 2)%n /= 7 .or. grid(2, 3)%n /= 7) error stop 15
    print *, "ok"
end program global_init_12
