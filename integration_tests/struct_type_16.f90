module test_struct_kind_shadow
    implicit none
    integer, parameter :: q = selected_real_kind(10)
    type ty
        real(q) :: q = 1._q
    end type
end module

program test
    use test_struct_kind_shadow
    implicit none
    type(ty) :: t
    if (t%q /= 1.0) error stop "value incorrect"
    print *, "PASS"
end program test