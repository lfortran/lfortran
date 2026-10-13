! LFortran traits extension test; GFortran does not accept this syntax.
! Character type sets with the length wildcard read from a separately compiled
! module: each constant length of an actual argument is its own type argument
! and specialization, inferred or explicit, forwarded by a client generic, and
! dispatched statically through an implementing type; character(kind=*) is
! character of length 1 and every kind of CHARACTER_KINDS, and an inline set
! may use the wildcard too.
module traits_type_sets_04_client
    use traits_type_sets_04_sets, only: IText, smallest, ordered
    implicit none
    private
    public :: smaller_of, same

contains

    ! Forwards T to generics constrained by the same imported trait.
    function smaller_of{IText :: T}(x, y) result(r)
        type(T), intent(in) :: x(:), y(:)
        type(T)             :: r
        r = smallest(x)
        if (smallest(y) < r) r = smallest{T}(y)
    end function smaller_of

    function same{character(len=*) :: T}(a, b) result(r)
        type(T), intent(in) :: a, b
        logical             :: r
        r = a == b
    end function same
end module traits_type_sets_04_client

program traits_type_sets_04
    use traits_type_sets_04_sets, only: Least, smallest, count_equal, ordered, &
        last_letter, differ
    use traits_type_sets_04_client, only: smaller_of, same
    implicit none
    character(len=1) :: c1(4)
    character(len=3) :: c3(4)
    character(len=4) :: c4(3)
    character(len=7) :: c7(3)
    type(Least) :: picker
    integer :: checks

    checks = 0
    c1 = ['q', 'b', 'z', 'k']
    c3 = ['dog', 'cat', 'eel', 'cat']
    c4 = ['tree', 'bush', 'moss']
    c7 = ['zeppeli', 'balloon', 'gliders']

    call check(smallest(c1) == 'b')
    call check(smallest(c3) == 'cat')
    call check(smallest{character(len=4)}(c4) == 'bush')
    call check(smallest(c7(1:3:2)) == 'gliders')
    call check(len(smallest(c7)) == 7)
    call check(smallest(['xy', 'ab']) == 'ab')
    call check(count_equal(c3, 'cat') == 2)
    call check(count_equal(c7, 'balloon') == 1)
    call check(count_equal(key='moss', x=c4) == 1)
    call check(ordered([1, 2, 2, 5]))
    call check(.not. ordered([3, 1]))
    call check(ordered(['ant', 'bee', 'bee']))
    call check(.not. ordered(c4))
    call check(last_letter(c1) == 'z')
    call check(differ('abc', 'abd'))
    call check(.not. differ(c7(2), 'balloon'))
    call check(smaller_of(c3(1:1), c3(3:4)) == 'cat')
    call check(smaller_of(c7, c7(3:3)) == 'balloon')
    call check(same('moss', c4(3)))
    call check(.not. same{character(len=1)}('a', 'b'))
    call check(picker%least(c4) == 'bush')
    call check(picker%least(c1(3:4)) == 'k')

    if (checks /= 22) error stop 2
    print '(a,i0,a)', 'traits_type_sets_04: ', checks, ' checks passed'

contains

    subroutine check(condition)
        logical, intent(in) :: condition
        checks = checks + 1
        if (.not. condition) then
            print '(a,i0,a)', 'check ', checks, ' failed'
            error stop 1
        end if
    end subroutine check
end program traits_type_sets_04
