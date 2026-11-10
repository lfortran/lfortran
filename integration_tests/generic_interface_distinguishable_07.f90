! A procedure name is a valid actual argument for an INTENT(IN) procedure
! pointer dummy, so the generic specific must be selected rather than the
! structure constructor of the type with the same name.
module generic_interface_distinguishable_07_a
    implicit none
    abstract interface
        integer function f_i()
        end function
    end interface
    type :: desc_t
        character(len=:), allocatable :: d
        procedure(f_i), pointer, nopass :: f => null()
    end type
    interface desc_t
        module procedure from_chars
    end interface
contains
    function from_chars(d, f) result(r)
        character(len=*), intent(in) :: d
        procedure(f_i), intent(in), pointer, optional :: f
        type(desc_t) :: r
        r%d = "g:" // d
        if (present(f)) r%f => f
    end function
end module

module generic_interface_distinguishable_07_b
    use generic_interface_distinguishable_07_a, only: desc_t, f_i
    implicit none
contains
    subroutine from_module_procedure(x)
        type(desc_t), intent(out) :: x
        x = desc_t("abc", chk)
    end subroutine
    subroutine from_dummy_procedure(x, g)
        type(desc_t), intent(out) :: x
        procedure(f_i) :: g
        x = desc_t("def", g)
    end subroutine
    integer function chk()
        chk = 42
    end function
end module

program generic_interface_distinguishable_07
    use generic_interface_distinguishable_07_a, only: desc_t
    use generic_interface_distinguishable_07_b, only: from_module_procedure, &
        from_dummy_procedure, chk
    implicit none
    type(desc_t) :: x
    call from_module_procedure(x)
    if (x%d /= "g:abc") error stop
    if (.not. associated(x%f)) error stop
    if (x%f() /= 42) error stop
    call from_dummy_procedure(x, chk)
    if (x%d /= "g:def") error stop
    print *, "ok"
end program
