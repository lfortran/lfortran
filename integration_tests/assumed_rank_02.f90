! Test for https://github.com/lfortran/lfortran/issues/4749
! Assumed rank with pure function returning logical
program assumed_rank_02
    implicit none
    real :: x(2,2,2)
    x = 1.0
    if (.not. assumed(x)) error stop
    if (.not. all(elementl(x))) error stop
    print *, 'assumed rank:', assumed(x)
    print *, 'elemental   :', elementl(x)
contains
    pure logical function assumed(x)
        real, intent(in) :: x(..)
        assumed = .true.
    end function assumed

    elemental logical function elementl(x)
        real, intent(in) :: x
        elementl = .true.
    end function elementl
end program
