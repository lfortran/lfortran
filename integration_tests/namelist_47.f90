module namelist_47_data
    implicit none
    integer :: ia(6), ib(6), ic(2)
    real :: ra(6)
    character(len=3) :: ca(3)
    logical :: la(3)
contains
    subroutine reset()
        ia = -1
        ib = -1
        ic = -1
        ra = -1.0
        ca = 'zzz'
        la = .false.
    end subroutine reset
end module namelist_47_data

program namelist_47
    ! An array object gets fewer values than it has elements and the record
    ! ends right after a value separator. The next record starts a new
    ! name-value subsequence and the remaining elements keep their values.
    use namelist_47_data
    implicit none
    character(len=1), parameter :: nl = new_line('a')

    ! integer array, comma + end of record, then the next object
    call run('&g' // nl // ' ia = 1,' // nl // ' ib = 2' // nl // '/')
    if (any(ia /= [1, -1, -1, -1, -1, -1])) error stop 1
    if (any(ib /= [2, -1, -1, -1, -1, -1])) error stop 2

    ! real array
    call run('&g' // nl // ' ra = 1.5,' // nl // ' ib = 2' // nl // '/')
    if (any(abs(ra - [1.5, -1.0, -1.0, -1.0, -1.0, -1.0]) > 1e-6)) error stop 3
    if (any(ib /= [2, -1, -1, -1, -1, -1])) error stop 4

    ! two values, every record ends in a comma
    call run('&g' // nl // ' ia = 1, 2,' // nl // ' ib = 3,' // nl // '/')
    if (any(ia /= [1, 2, -1, -1, -1, -1])) error stop 5
    if (any(ib /= [3, -1, -1, -1, -1, -1])) error stop 6

    ! a comment and an empty record between the comma and the next object
    call run('&g' // nl // ' ia = 1,   ! first' // nl // nl // &
        ' ! only a comment' // nl // ' ib = 2,' // nl // ' ic = 7,' // nl // '/')
    if (any(ia /= [1, -1, -1, -1, -1, -1])) error stop 7
    if (any(ib /= [2, -1, -1, -1, -1, -1])) error stop 8
    if (any(ic /= [7, -1])) error stop 9

    ! the next object is an array element or has a blank before the equals
    call run('&g' // nl // ' ic = 5,' // nl // ' ib(2) = 4,' // nl // &
        ' ia   = 9,' // nl // '/')
    if (any(ic /= [5, -1])) error stop 10
    if (any(ib /= [-1, 4, -1, -1, -1, -1])) error stop 11
    if (any(ia /= [9, -1, -1, -1, -1, -1])) error stop 12

    ! character and logical arrays
    call run('&g' // nl // " ca = 'abc'," // nl // ' la = .true.,' // nl // &
        ' ib = 2,' // nl // '/')
    if (ca(1) /= 'abc' .or. ca(2) /= 'zzz' .or. ca(3) /= 'zzz') error stop 13
    if (.not. la(1) .or. la(2) .or. la(3)) error stop 14
    if (any(ib /= [2, -1, -1, -1, -1, -1])) error stop 15

    ! values continued on the next record still belong to the same object
    call run('&g' // nl // ' ia = 1, 2,' // nl // '      3, 4,' // nl // &
        ' ib = 5' // nl // '/')
    if (any(ia /= [1, 2, 3, 4, -1, -1])) error stop 16
    if (any(ib /= [5, -1, -1, -1, -1, -1])) error stop 17

    print *, "PASS"

contains

    subroutine run(text)
        character(len=*), intent(in) :: text
        integer :: u, ios, i0, i
        namelist /g/ ia, ib, ic, ra, ca, la
        call reset()
        open(newunit=u, file='namelist_47.nml', status='replace')
        i0 = 1
        do i = 1, len(text)
            if (text(i:i) == nl) then
                write(u, '(a)') text(i0:i-1)
                i0 = i + 1
            end if
        end do
        write(u, '(a)') text(i0:)
        close(u)
        open(newunit=u, file='namelist_47.nml', status='old')
        read(u, nml=g, iostat=ios)
        close(u, status='delete')
        if (ios /= 0) error stop 100
    end subroutine run
end program namelist_47
