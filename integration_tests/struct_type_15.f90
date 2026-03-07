module test_struct_kind_shadow_logical
    implicit none
    integer, parameter :: l = 4
    type ty
        logical(l) :: l = .true.
    end type
end module
program test
    use test_struct_kind_shadow_logical
    implicit none
    type(ty) :: t
    if (.not. t%l) error stop "value incorrect"
    print *, "PASS"
end program test
