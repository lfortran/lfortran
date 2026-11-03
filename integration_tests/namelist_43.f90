! Namelist WRITE of arrays of a derived type with an array component in
! descriptor storage: an allocatable array, and a host array accessed from
! an internal procedure. Each element is written as `name(i)%component`,
! with the extents and lower bounds known only at run time.
module namelist_43_mod
    implicit none
    type :: t
        integer :: k = 0
        integer :: a(2) = 0
    end type
contains

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

    subroutine host()
        type(t) :: y(2)
        type(t), allocatable :: z(:, :)
        namelist /nly/ y
        namelist /nlz/ z
        y(1)%a = [1, 2]
        y(2)%a = [3, 4]
        y%k = [5, 6]
        allocate(z(0:1, -1:-1))
        z(0, -1)%a = [7, 8]
        z(1, -1)%a = [9, 10]
        call inner()
    contains
        subroutine inner()
            character(len=200) :: buf(20)
            buf = ""
            write(buf, nml=nly)
            call squeeze(buf)
            if (.not. has(buf, "Y(1)%A=1,2")) error stop 4
            if (.not. has(buf, "Y(2)%A=3,4")) error stop 5
            if (.not. has(buf, "%K=5,6") .and. .not. has(buf, "Y(2)%K=6")) error stop 6
            buf = ""
            write(buf, nml=nlz)
            call squeeze(buf)
            if (.not. has(buf, "Z(0,-1)%A=7,8")) error stop 7
            if (.not. has(buf, "Z(1,-1)%A=9,10")) error stop 8
        end subroutine
    end subroutine

end module

program namelist_43
    use namelist_43_mod
    implicit none
    type(t), allocatable :: x(:)
    namelist /nl/ x
    character(len=200) :: buf(20)

    allocate(x(0:2))
    x(0)%a = [1, 2]
    x(1)%a = [3, 4]
    x(2)%a = [5, 6]
    x%k = [7, 8, 9]
    buf = ""
    write(buf, nml=nl)
    call squeeze(buf)
    if (.not. has(buf, "X(0)%A=1,2")) error stop 1
    if (.not. has(buf, "X(2)%A=5,6")) error stop 2
    if (.not. has(buf, "%K=7,8,9") .and. .not. has(buf, "X(2)%K=9")) error stop 3

    call host()
    print *, "ok"
end program
