module mod_a_32
    implicit none
    type :: type_a
    end type
    interface operator(==)
        module procedure eq_a
    end interface
contains
    elemental logical function eq_a(l, r)
        type(type_a), intent(in) :: l
        character(len=*), intent(in) :: r
        eq_a = .false.
    end function
end module

module mod_b_32
    implicit none
    type :: type_b
    end type
    interface operator(==)
        module procedure eq_b
    end interface
contains
    pure logical function eq_b(l, r)
        type(type_b), intent(in) :: l
        character(len=*), dimension(:), intent(in) :: r
        eq_b = .false.
    end function
end module

program main
    use mod_a_32, only: operator(==)
    use mod_b_32, only: type_b, operator(==)
    implicit none
    type(type_b) :: x
    character(len=1) :: arr(1)
    logical :: res
    arr(1) = "a"
    res = (x == arr)
    if (res) error stop
    print *, "PASS"
end program
