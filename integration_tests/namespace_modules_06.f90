! Named constants accessed through a module entity are constant expressions:
! kind selectors, array bounds, character lengths, parameter
! initialization, case selectors and enumerators.
module namespace_modules_06_consts
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: ik = selected_int_kind(15)
    integer, parameter :: n = 4
    integer, parameter :: nlen = 6
    integer, parameter :: primes(5) = [2, 3, 5, 7, 11]
    character(len=*), parameter :: greeting = "hello"
    enum, bind(c)
        enumerator :: red = 1, green, blue
    end enum
end module

program namespace_modules_06
    use, namespace :: c => namespace_modules_06_consts
    implicit none
    integer, parameter :: m = c%n*2 + c%primes(3)
    real(c%dp) :: x
    integer(c%ik) :: big
    integer :: arr(c%n), mat(c%n, m)
    character(len=c%nlen) :: s
    character(len=len(c%greeting)) :: g
    integer :: color

    if (m /= 13) error stop
    if (kind(x) /= kind(1.0d0)) error stop
    x = 1.0_8 / 3
    if (abs(x - 1.0d0/3) > 1d-15) error stop
    big = 2_8**40
    if (kind(big) /= c%ik) error stop
    if (big /= 1099511627776_8) error stop
    if (size(arr) /= 4) error stop
    if (any(shape(mat) /= [4, 13])) error stop
    if (len(s) /= 6) error stop
    g = c%greeting
    if (len(g) /= 5 .or. g /= "hello") error stop
    if (c%greeting(2:3) /= "el") error stop

    color = c%green
    select case (color)
    case (c%red)
        error stop
    case (c%green)
        print *, "green", c%green
    case (c%blue)
        error stop
    end select
    if (c%blue /= 3) error stop
    ! Named constants can be used for the kind argument of intrinsics
    if (kind(real(1, c%dp)) /= c%dp) error stop
    print *, m, x, big, s, g
end program
