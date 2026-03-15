module test_struct_kind_shadow_complex
    implicit none
    integer, parameter :: q = selected_real_kind(10)
    type ty
        complex(q) :: q = (1.0_q, 2.0_q)
    end type
end module
program test
    use test_struct_kind_shadow_complex
    implicit none
    type(ty) :: t
    if (real(t%q) /= 1.0 .or. aimag(t%q) /= 2.0) error stop "value incorrect"
    print *, "PASS"
end program test
