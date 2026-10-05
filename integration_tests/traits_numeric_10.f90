! Each module and this fresh client compile separately, including private proofs.
program traits_numeric_10
    use traits_numeric_10_ops_m, only: shift
    use traits_numeric_10_facade_m, only: renamed, again
    use traits_numeric_10_other_m, only: other => shift
    implicit none
    if (shift(3, 2) /= 5) error stop
    if (renamed{integer}(3, 2) /= 5) error stop
    if (again(3, 2) /= 5) error stop
    if (other{integer}(3, 2) /= 7) error stop
    if (other(3, 2) /= 7) error stop
    call expect_real64(shift(1.5d0, 2), 3.5d0)
    call expect_real64(renamed{real(8)}(1.5d0, 2), 3.5d0)
    call expect_real64(again{real(kind=8)}(0.d0, 16777217), 16777217.d0)
    call expect_real64(other(1.5d0, 2), 5.5d0)
    if (kind(again(0.d0, 1)) /= 8) error stop
contains
    subroutine expect_real64(actual, expected)
        real(8), intent(in) :: actual, expected
        if (.not. (abs(actual - expected) <= 32*epsilon(1.d0)*max(1.d0,abs(expected)))) error stop
    end subroutine
end program
