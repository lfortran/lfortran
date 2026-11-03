! Namelist I/O of arrays of an extended derived type uses the element size
! of the extended type. Not labelled gfortran: GFortran 15 writes only the
! first element of such arrays and cannot read them back.
program namelist_42
    implicit none
    type :: base
        integer :: k = 0
    end type
    type, extends(base) :: ext
        real :: r = 0
    end type
    type(ext) :: x(3)
    namelist /nx/ x
    character(len=200) :: buf(10)

    x%k = [1, 2, 3]
    x%r = [10., 20., 30.]
    buf = ""
    write(buf, nml=nx)
    call squeeze(buf)
    if (.not. has(buf, "X%K=1,2,3")) error stop 1
    if (.not. has(buf, "X%R=1.0000000E+01,2.0000000E+01,3.0000000E+01")) error stop 2

    x%k = 0
    x%r = 0
    buf = ""
    buf(1) = "&nx"
    buf(2) = " x%k = 4, 5, 6"
    buf(3) = " x%r = 10., 20., 30."
    buf(4) = "/"
    read(buf, nml=nx)
    if (any(x%k /= [4, 5, 6])) error stop 3
    if (any(abs(x%r - [10., 20., 30.]) > 1e-6)) error stop 4
    print *, "ok"

contains

    ! Remove blanks so that the check does not depend on value formatting.
    subroutine squeeze(lines)
        character(len=*), intent(inout) :: lines(:)
        integer :: n, m, c
        character(len=len(lines)) :: tmp
        do n = 1, size(lines)
            tmp = ""
            m = 0
            do c = 1, len_trim(lines(n))
                if (lines(n)(c:c) /= " ") then
                    m = m + 1
                    tmp(m:m) = lines(n)(c:c)
                end if
            end do
            lines(n) = tmp
        end do
    end subroutine

    logical function has(lines, s)
        character(len=*), intent(in) :: lines(:), s
        integer :: n
        has = .false.
        do n = 1, size(lines)
            if (index(lines(n), s) > 0) has = .true.
        end do
    end function

end program
