! LFortran traits extension test; GFortran does not accept this syntax.
! Character members and results of closed runtime slots across separately
! compiled modules: the contract module declares the character(len=3) |
! character(len=5) type set, the provider fills the member slots (with
! nopass, first and non-first passed-object bindings, a view inside a provider
! and static recursion over sections) and two ordinary messages returning
! character values, and the consumer and this driver reach them only through
! class(...) views, with keyword arguments and inferred and explicit type
! arguments. traits_runtime_numeric_separate_01.py --case character freezes and
! hides the provider before compiling the clients.
program traits_runtime_character_separate_01
    use traits_runtime_character_separate_01_contracts_m, only: IPick, ILabel, &
        make_picker, make_label
    use traits_runtime_character_separate_01_consumer_m, only: pick3, pick5, &
        reversed_pick, padded_label
    implicit none
    integer, parameter :: lengths(5) = [0, 1, 2, 5, 8]
    character(len=3) :: c3(8)
    character(len=5) :: c5(8)
    integer :: k, choice, n, i, checks
    class(IPick), allocatable :: picker
    class(ILabel), allocatable :: labeler

    checks = 0
    c3 = ['pqr', 'abc', 'xyz', 'mno', 'abd', 'zzz', 'aaa', 'klm']
    c5 = ['hello', 'apple', 'zebra', 'mango', 'apply', 'quart', 'bravo', 'delta']
    do k = 1, 4
        choice = k
        if (command_argument_count() > 0) choice = 5 - k
        call make_picker(choice, picker)
        do i = 1, size(lengths)
            n = lengths(i)
            call check3(pick3(picker, c3(1:n), 'abd'), reference3(c3(1:n), 'abd'))
            call check5(pick5(picker, c5(n:1:-1), 'apple'), reference5(c5(1:n), 'apple'))
            call check3(picker%pick(c3(1:n:2), 'b  '), reference3(c3(1:n:2), 'b  '))
            call check5(reversed_pick(picker, c5(1:n), 'b    '), &
                reference5(c5(1:n), 'b    '))
            call check3(picker%pick(floor='mmm', x=c3(n:1:-1)), reference3(c3(1:n), 'mmm'))
        end do
        deallocate(picker)
    end do

    call make_label(labeler)
    call check5(padded_label(labeler, 'ab'), '#:ab ')
    call check5(labeler%label('abcdef', 3), '#:abc')
    call check5(labeler%label(code='x', width=0), '#:   ')
    call check1(labeler%first_letter('zeta'), 'z')
    call check1(labeler%first_letter(text=''), ' ')

    if (checks /= 105) error stop 3
    print '(a,i0,a)', 'character separate: frozen provider, contract-only consumer, ', &
        checks, ' checks passed'

contains

    function reference3(x, floor) result(r)
        character(len=3), intent(in) :: x(:), floor
        character(len=3) :: r
        integer :: j
        r = floor
        do j = 1, size(x)
            if (j == 1) r = x(1)
            if (x(j) < r) r = x(j)
        end do
        if (r < floor) r = floor
    end function reference3

    function reference5(x, floor) result(r)
        character(len=5), intent(in) :: x(:), floor
        character(len=5) :: r
        integer :: j
        r = floor
        do j = 1, size(x)
            if (j == 1) r = x(1)
            if (x(j) < r) r = x(j)
        end do
        if (r < floor) r = floor
    end function reference5

    subroutine check1(actual, expected)
        character, intent(in) :: actual, expected
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,4a)', 'check ', checks, ': ', actual, ' /= ', expected
            error stop 1
        end if
    end subroutine check1

    subroutine check3(actual, expected)
        character(len=3), intent(in) :: actual, expected
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,4a)', 'check ', checks, ': ', actual, ' /= ', expected
            error stop 1
        end if
    end subroutine check3

    subroutine check5(actual, expected)
        character(len=5), intent(in) :: actual, expected
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,4a)', 'check ', checks, ': ', actual, ' /= ', expected
            error stop 1
        end if
    end subroutine check5
end program traits_runtime_character_separate_01
