! gpu decline reason: LiveoutDerivedType
! A derived-type variable of which the loop passes an array component to
! a procedure that may define it, and also defines a scalar component and
! names the variable whole, so that each thread keeps a private copy of
! the whole value: the procedure may define just the elements the
! iteration owns, in place, which a copy of the component would not
! carry back.
module gpu_decline_31_m
    implicit none
    type :: t
        real :: c(4)
        integer :: k
    end type
contains
    pure subroutine fill(c, i)
        real, intent(inout) :: c(4)
        integer, intent(in) :: i
        if (i == 2) c = 9
    end subroutine

    pure real function f(y)
        type(t), intent(in) :: y
        f = y%k
    end function
end module

program gpu_decline_31
    use gpu_decline_31_m
    implicit none
    type(t) :: y
    real :: b(100)
    integer :: i
    y%c = 0
    y%k = 1
    do concurrent (i = 1:100)
        call fill(y%c, i)
        b(i) = 0
        if (i == 3) then
            y%k = 5
            b(i) = f(y)
        end if
    end do
    print *, y%c(1), b(3), y%k
    if (y%c(1) /= 9) error stop 1
    if (b(3) /= 5 .or. y%k /= 5) error stop 2
end program
