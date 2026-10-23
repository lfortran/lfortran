! An optional dummy argument that one iteration of a `do concurrent`
! defines when it is present: the procedure is called with and without
! it. The loop runs either way, and the caller sees the value only when it
! passed the argument.
module gpu_liveout_07_m
    implicit none
contains
    subroutine s(n, a, k)
        integer, intent(in) :: n
        integer, intent(out) :: a(n)
        integer, intent(out), optional :: k
        integer :: i
        do concurrent (i = 1:n)
            a(i) = i
            if (present(k)) then
                if (i == 3) k = 30
            end if
        end do
    end subroutine
end module

program gpu_liveout_07
    use gpu_liveout_07_m
    implicit none
    integer :: a(100), k
    a = 0
    call s(100, a)
    print *, a(1), a(7), a(100)
    if (a(7) /= 7 .or. a(100) /= 100) error stop 1
    a = 0
    k = 0
    call s(100, a, k)
    print *, a(7), k
    if (a(7) /= 7) error stop 2
    if (k /= 30) error stop 3
end program
