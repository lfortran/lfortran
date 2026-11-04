! Uses of elemental procedures (#16741 group 4). A nonintrinsic elemental
! procedure cannot be an actual argument (C1533, #16667), a dummy procedure
! cannot have an elemental interface (C1517, #12936), and an array section
! with a repeated vector subscript value is not definable (9.5.3.3.3), so it
! cannot be the actual for an intent(out) or intent(inout) dummy of an
! elemental subroutine or the target of an elemental defined assignment.
module cc_ec_mod
    implicit none
    abstract interface
        integer function cc_ec_iface(x)
            integer, intent(in) :: x
        end function
        real function cc_ec_riface(x)
            real, intent(in) :: x
        end function
    end interface
    type :: cc_ec_t
        integer :: k = 0
    contains
        procedure, nopass :: get => cc_ec_call_f
    end type
    interface assignment(=)
        module procedure cc_ec_copy
    end interface
contains
    elemental integer function cc_ec_ef(x)
        integer, intent(in) :: x
        cc_ec_ef = x + 1
    end function
    elemental subroutine cc_ec_es(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine
    elemental subroutine cc_ec_copy(to, from)
        type(cc_ec_t), intent(out) :: to
        type(cc_ec_t), intent(in) :: from
        to%k = from%k
    end subroutine
    integer function cc_ec_call_f(f)
        procedure(cc_ec_iface) :: f
        cc_ec_call_f = f(1)
    end function
    real function cc_ec_call_r(f)
        procedure(cc_ec_riface) :: f
        cc_ec_call_r = f(0.0)
    end function
    subroutine cc_ec_call_s(s)
        interface
            subroutine s(x)
                integer, intent(inout) :: x
            end subroutine
        end interface
        integer :: i
        i = 1
        call s(i)
    end subroutine
    integer function cc_ec_apply(h, x)
        interface
            elemental integer function h(x)  ! {Error} dummy procedure 'h' cannot have an elemental interface
                integer, intent(in) :: x
            end function
        end interface
        integer, intent(in) :: x
        cc_ec_apply = h(x)
    end function
end module

program continue_compilation_elemental_call
    use cc_ec_mod
    implicit none
    intrinsic :: sin
    type(cc_ec_t) :: o, a(2), c(3)
    integer :: b(3)
    b = [1, 2, 3]
    print *, cc_ec_call_r(sin)
    print *, cc_ec_call_f(cc_ec_ef)  ! {Error} nonintrinsic elemental procedure 'cc_ec_ef' cannot be an actual argument
    print *, o%get(cc_ec_ef)  ! {Error} nonintrinsic elemental procedure 'cc_ec_ef' cannot be an actual argument
    call cc_ec_call_s(cc_ec_es)  ! {Error} nonintrinsic elemental procedure 'cc_ec_es' cannot be an actual argument
    call cc_ec_es(b([3, 1]))
    call cc_ec_es(b([1, 1]))  ! {Error} array section with a repeated vector subscript value cannot be associated with intent(inout) dummy argument 'x' of an elemental subroutine
    c([2, 2]) = a  ! {Error} array section with a repeated vector subscript value cannot be associated with intent(out) dummy argument 'to' of an elemental subroutine
end program
