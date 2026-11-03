! Namelist groups that hold arrays of a derived type with array or
! derived-type components: such components are written once per element as
! `name(i,...)%component`, and the components that are scalars of an
! intrinsic type can still be read as a whole array (`name%component`).
program namelist_41
    implicit none
    type :: inner_t
        integer :: u = 0
        integer :: w = 0
    end type
    type :: t
        integer :: a(2) = 0
    end type
    type :: s_t
        integer :: k = 0
        real :: r(2) = 0
        type(inner_t) :: in
    end type
    type :: m_t
        integer :: k = 0
        type(inner_t) :: arr(2)
    end type
    type :: c_t
        integer :: k = 0
        character(len=2) :: cc(2) = "xy"
    end type
    type :: v_t
        integer :: k = 0
        integer :: a(2) = 0
        type(inner_t) :: in
    end type
    type(t) :: x(2), xl(0:1)
    type(s_t) :: y(2, 2)
    type(v_t) :: v(2)
    type(m_t) :: m(0:1, 2)
    type(c_t) :: c(2)
    type(t) :: z(2:3, -1:0)
    namelist /nl/ x
    namelist /nl2/ y
    namelist /nl3/ xl, z
    namelist /nl5/ v
    namelist /nl6/ m
    namelist /nl7/ c
    character(len=200) :: buf(40)
    integer :: i, j

    x(1)%a = [1, 2]
    x(2)%a = [3, 4]
    buf = ""
    write(buf, nml=nl)
    call squeeze(buf)
    if (.not. has(buf, "X(1)%A=1,2")) error stop 1
    if (.not. has(buf, "X(2)%A=3,4")) error stop 2

    do j = 1, 2
        do i = 1, 2
            y(i, j)%k = 10*i + j
            y(i, j)%r = [real(i), real(j) + 0.5]
            y(i, j)%in%u = i
            y(i, j)%in%w = 5 + j
        end do
    end do
    y(2, 1)%in%w = 9
    buf = ""
    write(buf, nml=nl2)
    call squeeze(buf)
    if (.not. has(buf, "Y(2,2)%R=2.")) error stop 3
    if (.not. has(buf, "Y(1,2)%R=1.")) error stop 4
    if (.not. has(buf, "%K=11,21,12,22") .and. .not. has(buf, "Y(2,1)%K=21")) error stop 5
    if (.not. has(buf, "%W=6,9,7,7") .and. .not. has(buf, "Y(2,1)%IN%W=9")) error stop 6

    ! Non-default lower bounds
    xl(0)%a = [5, 6]
    xl(1)%a = [7, 8]
    do j = -1, 0
        do i = 2, 3
            z(i, j)%a = [i, j]
        end do
    end do
    buf = ""
    write(buf, nml=nl3)
    call squeeze(buf)
    if (.not. has(buf, "XL(0)%A=5,6")) error stop 7
    if (.not. has(buf, "XL(1)%A=7,8")) error stop 8
    if (.not. has(buf, "Z(3,-1)%A=3,-1")) error stop 9
    if (.not. has(buf, "Z(2,0)%A=2,0")) error stop 10

    ! A derived-type array nested in a derived-type array
    do j = 1, 2
        do i = 0, 1
            m(i, j)%k = 10*i + j
            m(i, j)%arr(1)%u = 100*i + 10*j + 1
            m(i, j)%arr(2)%u = 100*i + 10*j + 2
            m(i, j)%arr%w = -j
        end do
    end do
    buf = ""
    write(buf, nml=nl6)
    call squeeze(buf)
    if (.not. has(buf, "M(0,1)%ARR(1)%U=11")) error stop 11
    if (.not. has(buf, "M(1,2)%ARR(2)%U=122")) error stop 12
    if (.not. has(buf, "M(1,1)%ARR(2)%U=112")) error stop 13
    if (.not. has(buf, "M(0,2)%ARR(1)%W=-2")) error stop 14
    if (.not. has(buf, "%K=1,11,2,12") .and. .not. has(buf, "M(1,2)%K=12")) error stop 15

    ! Scalar components of an intrinsic type are read as a whole array
    buf = ""
    buf(1) = "&nl5"
    buf(2) = " v%k = 5, 6"
    buf(3) = "/"
    read(buf, nml=nl5)
    if (any(v%k /= [5, 6])) error stop 16
    if (any(v%in%u /= 0)) error stop 17
    if (any(v(1)%a /= 0) .or. any(v(2)%a /= 0)) error stop 18

    ! Reading is not affected by components that are written element-wise
    buf = ""
    buf(1) = "&nl7"
    buf(2) = " c%k = 3, 4"
    buf(3) = "/"
    read(buf, nml=nl7)
    if (any(c%k /= [3, 4])) error stop 19
    if (c(2)%cc(2) /= "xy") error stop 20
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
