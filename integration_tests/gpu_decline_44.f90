! gpu decline reason: LiveoutOptionalPassedOn
! An optional dummy argument the loop may define, whose value is kept after
! it, that the loop passes on to an optional dummy argument of a procedure
! which tests whether it is present: each thread would pass its private
! copy, which is present whether or not the argument is.
module gpu_decline_44_m
    implicit none
contains
    pure subroutine sub(v, k)
        real, intent(inout) :: v
        integer, intent(inout), optional :: k
        if (present(k)) then
            v = v + 100
            if (v > 1.0e9) k = 1
        else
            v = v + 1
        end if
    end subroutine
    subroutine run(y, k)
        real, intent(inout) :: y(:)
        integer, intent(inout), optional :: k
        integer :: i
        do concurrent (i = 1:size(y))
            call sub(y(i), k)
        end do
    end subroutine
end module
program gpu_decline_44
    use gpu_decline_44_m
    implicit none
    real :: y(10)
    integer :: k
    y = 0
    call run(y)
    print *, y(3)
    if (y(3) /= 1) error stop 1
    k = 0
    call run(y, k)
    print *, y(3), k
    if (y(3) /= 101) error stop 2
end program
