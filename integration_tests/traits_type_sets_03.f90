! LFortran traits extension test; GFortran does not accept this syntax.
! Logical and character type sets read from a separately compiled module:
! inferred and explicit type arguments, a client generic forwarding its binder
! to the module's generics, and distinct character lengths.
module traits_type_sets_03_client
    use traits_type_sets_03_sets, only: IFlag, ICode, all_true, count_set, smallest
    implicit none
    private
    public :: summary, shortest

contains

    ! Forwards T to generics constrained by the same imported traits.
    function summary{IFlag :: T}(x) result(n)
        type(T), intent(in) :: x(:)
        integer             :: n
        n = count_set(x)
        if (all_true{T}(x)) n = -n
    end function summary

    function shortest{ICode :: T}(x, y) result(r)
        type(T), intent(in) :: x(:), y(:)
        type(T)             :: r
        r = smallest(x)
        if (smallest(y) < r) r = smallest(y)
    end function shortest
end module traits_type_sets_03_client

program traits_type_sets_03
    use traits_type_sets_03_sets, only: all_true, count_set, smallest, doubled, both
    use traits_type_sets_03_client, only: summary, shortest
    implicit none
    logical(1) :: f1(3)
    logical(8) :: f8(4)
    character(len=3) :: c3(3)
    character(len=5) :: c5(2)
    integer :: checks

    checks = 0
    f1 = [.true., .true., .false.]
    f8 = [.true., .true., .true., .true.]
    c3 = ['dog', 'cat', 'eel']
    c5 = ['zebra', 'horse']

    call check(.not. logical(all_true(f1)))
    call check(logical(all_true{logical(8)}(f8)))
    call check(count_set(f1) == 2)
    call check(count_set(f8(1:3)) == 3)
    call check(summary(f1) == 2)
    call check(summary(f8) == -4)
    call check(summary{logical(1)}(f1(1:2)) == -2)
    call check(smallest(c3) == 'cat')
    call check(len(smallest(c5)) == 5)
    call check(shortest(c3, c3(3:3)) == 'cat')
    call check(shortest(c5(1:1), c5(2:2)) == 'horse')
    call check(doubled('ab!') == 'ab!ab!')
    call check(logical(both(f1(1), f1(3))) .eqv. .false.)
    call check(both(.true., .true.))

    if (checks /= 14) error stop 2
    print '(a,i0,a)', 'traits_type_sets_03: ', checks, ' checks passed'

contains

    subroutine check(condition)
        logical, intent(in) :: condition
        checks = checks + 1
        if (.not. condition) then
            print '(a,i0,a)', 'check ', checks, ' failed'
            error stop 1
        end if
    end subroutine check
end program traits_type_sets_03
