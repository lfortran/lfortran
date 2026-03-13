module test_struct_kind_shadow_int
    implicit none
    integer, parameter :: k = 4
    type ty
        integer(k) :: k = 42_k
    end type
end module

program test
    use test_struct_kind_shadow_int
    implicit none
    type(ty) :: t
    if (t%k /= 42) error stop "value incorrect"
    print *, "PASS"
end program test