module struct_member_order_fortran_13789_m
    implicit none
    type :: rec
        real :: z
        integer :: a
    end type
    type, bind(c) :: crec
        real(8) :: z
        integer(4) :: a
    end type
end module

program struct_member_order_fortran_13789
    use struct_member_order_fortran_13789_m, only: rec, crec
    implicit none
    type(rec) :: r
    type(crec) :: c
    r = rec(1.5, 7)
    c = crec(2.5_8, 9)
    if (r%a /= 7 .or. c%a /= 9) error stop
end program
