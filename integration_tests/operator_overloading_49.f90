module operator_overloading_49_a
    implicit none
    type :: a_t
        integer :: x
    end type
    interface operator(.neqv.)
        module procedure a_neqv
    end interface
contains
    logical function a_neqv(a, b)
        type(a_t), intent(in) :: a, b
        a_neqv = a%x /= b%x
    end function
end module

module operator_overloading_49_b
    implicit none
    type :: b_t
        integer :: x
    contains
        procedure :: b_neqv
        generic :: operator(.neqv.) => b_neqv
    end type
contains
    logical function b_neqv(a, b)
        class(b_t), intent(in) :: a, b
        b_neqv = a%x /= b%x
    end function

    subroutine check_b()
        type(b_t) :: b1, b2
        logical :: l
        b1%x = 3; b2%x = 3
        l = b1 .neqv. b2
        print *, l
        if (l) error stop
    end subroutine
end module

program operator_overloading_49
    use operator_overloading_49_a
    use operator_overloading_49_b, only: check_b
    implicit none
    type(a_t) :: a1, a2
    logical :: l
    a1%x = 1; a2%x = 2
    l = a1 .neqv. a2
    print *, l
    if (.not. l) error stop
    l = l .neqv. .true.
    print *, l
    if (l) error stop
    call check_b()
end program
