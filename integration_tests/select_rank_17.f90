! Test that subroutine calls inside select rank correctly track dependencies.
! Regression test: a subroutine's dependency list must include siblings called
! from inside a select rank construct (bug: asr_owner not set before transform_stmts).
module select_rank_17_mod
    implicit none
contains

    subroutine process_rank(x, result)
        integer, intent(in) :: x(..)
        integer, intent(out) :: result

        select rank (v => x)
            rank (1)
                call compute_1d(v, result)
            rank (2)
                call compute_2d(v, result)
            rank default
                result = -1
        end select

    end subroutine process_rank

    subroutine compute_1d(x, result)
        integer, intent(in) :: x(:)
        integer, intent(out) :: result
        result = sum(x)
    end subroutine compute_1d

    subroutine compute_2d(x, result)
        integer, intent(in) :: x(:,:)
        integer, intent(out) :: result
        result = sum(x)
    end subroutine compute_2d

end module select_rank_17_mod

program select_rank_17
    use select_rank_17_mod
    implicit none

    integer :: a(4), b(2,2), r

    a = [1, 2, 3, 4]
    call process_rank(a, r)
    if (r /= 10) error stop

    b = reshape([1, 2, 3, 4], [2, 2])
    call process_rank(b, r)
    if (r /= 10) error stop

end program select_rank_17
