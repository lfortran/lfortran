! An omitted component whose derived type has default initialization takes
! the default-initialized value of that type (#14488): character defaults
! take their component's length, array components get the value in each
! element, also in named constants and module variable initializers, and
! defaults come from parent types and private components
module structure_constructor_args_32_m
    implicit none
    private
    public :: names, with_names, blah, with_array, outer, mv, get

    type :: names
        character(len=8) :: empty = ''
        character(len=5) :: short = "hi"
        character(len=2) :: long = "hello"
        integer :: i = 1
    end type names

    type :: with_names
        type(names) :: n
        integer :: j = 2
    end type with_names

    type :: blah
        integer :: i = 1
        integer, private :: h = 9
    end type blah

    type :: with_array
        type(blah) :: bs(2)
        integer :: j = 2
    end type with_array

    type :: outer
        type(with_array) :: c
        type(names) :: ns(3)
    end type outer

    type(outer) :: mv = outer()

contains

    integer function get(x)
        type(blah), intent(in) :: x
        get = x%h
    end function get

end module structure_constructor_args_32_m

program structure_constructor_args_32
    use structure_constructor_args_32_m
    implicit none

    type :: base
        integer :: i = 1
    end type base

    type, extends(base) :: child
        integer :: k = 7
    end type child

    type :: holder
        type(child) :: c
        integer :: k = 3
    end type holder

    type :: parent
        type(blah) :: b
        integer :: m = 5
    end type parent

    type, extends(parent) :: extended
        type(blah) :: e
        integer :: j = 2
    end type extended

    type(with_names), parameter :: pn = with_names()
    type(with_array), parameter :: pa = with_array()
    type(outer), parameter :: po = outer()
    type(with_names) :: wn
    type(with_array) :: wa = with_array(j=4)
    type(outer) :: o
    type(holder) :: h
    type(extended) :: x
    integer :: i

    call check_names(pn%n)
    if (pn%j /= 2) error stop

    wn = with_names(j=3)
    call check_names(wn%n)
    if (wn%j /= 3) error stop

    if (pa%bs(1)%i /= 1 .or. pa%bs(2)%i /= 1) error stop
    if (get(pa%bs(2)) /= 9) error stop
    if (pa%j /= 2) error stop
    if (any(wa%bs%i /= 1)) error stop
    if (wa%j /= 4) error stop

    if (po%c%bs(1)%i /= 1 .or. po%c%bs(2)%i /= 1) error stop
    if (po%c%j /= 2) error stop
    if (any(mv%c%bs%i /= 1)) error stop
    if (mv%c%j /= 2) error stop
    o = outer()
    if (any(o%c%bs%i /= 1)) error stop
    if (get(o%c%bs(1)) /= 9) error stop
    do i = 1, 3
        call check_names(po%ns(i))
        call check_names(mv%ns(i))
        call check_names(o%ns(i))
    end do

    h = holder()
    if (h%c%i /= 1) error stop
    if (h%c%k /= 7) error stop
    if (h%k /= 3) error stop

    x = extended(m=6)
    if (x%b%i /= 1) error stop
    if (x%e%i /= 1) error stop
    if (x%m /= 6) error stop
    if (x%j /= 2) error stop
    x = extended(parent=parent(blah(8)))
    if (x%b%i /= 8) error stop
    if (x%e%i /= 1) error stop
    if (x%m /= 5) error stop
    print *, "ok"

contains

    subroutine check_names(n)
        type(names), intent(in) :: n
        if (n%empty /= '') error stop
        if (len(n%empty) /= 8) error stop
        if (n%short /= "hi   ") error stop
        if (n%long /= "he") error stop
        if (n%i /= 1) error stop
    end subroutine check_names

end program structure_constructor_args_32
