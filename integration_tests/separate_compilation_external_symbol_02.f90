program test_sep_ext02
    use sep_ext02_mod_runner
    implicit none

    type(runner) :: r
    integer :: val

    val = 10
    call set_caller(r)
    call r%caller(val)
    if (val /= 11) error stop
    print *, "PASS"
end program
