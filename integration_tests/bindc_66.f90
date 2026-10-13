subroutine sub_c(x) bind(c)
    use iso_c_binding, only: c_int
    implicit none
    type, bind(c) :: t
        integer(c_int) :: i
    end type
    type(t), intent(in) :: x(*)
    if (x(2)%i /= 7) error stop
end subroutine

module bindc_66_mod
    use iso_c_binding, only: c_int
    implicit none
    type, bind(c) :: t
        integer(c_int) :: i
    end type
    type :: w_t
        type(t), allocatable :: a(:)
    end type
    interface
        subroutine sub_c(x) bind(c)
            import :: t
            type(t), intent(in) :: x(*)
        end subroutine
    end interface
end module bindc_66_mod

program bindc_66
    use bindc_66_mod
    implicit none
    type(w_t) :: w
    allocate(w%a(2))
    w%a(2)%i = 7
    call sub_c(w%a)
end program bindc_66
