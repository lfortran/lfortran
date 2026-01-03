module expr_14_utils
    private
    integer, parameter :: a = 10

    public :: a
end module

program expr_14
    use expr_14_utils, only: a
    implicit none

    print *, a
    if (a /= 10) error stop
end program
