module lfortran_intrinsic_string
    use, intrinsic :: iso_fortran_env, only: i64 => int64
implicit none

interface repeat
    module procedure repeati32, repeati64
end interface

contains

integer elemental function len_trim(string) result(r)
    character(len=*), intent(in) :: string
    r = len(string)
    if (r == 0) return
    do while(string(r:r) == " ")
        r = r - 1
        if (r == 0) exit
    end do
end function

function trim(x) result(r)
    character(len=*),intent(in) :: x
    character(len=len_trim(x)) :: r
    ! This does not work yet in LFortran:
    !r = x(1:len(r))
    ! So we do this workaroud that works:
    integer :: i
    do i = 1, len(r)
        r(i:i) = x(i:i)
    end do
end function

integer elemental function index(string_, substring_) result(idx)
    character(len=*), intent(in) :: string_
    character(len=*), intent(in) :: substring_
    integer :: i, j, k, pos
    logical :: found
    found = .true.
    idx = 0
    i = 1
    do while (i < len(string_) .and. found)
        k = 0
        j = 1
        do while (j < len(substring_) .and. found)
            pos = i + k
            if( string_(pos:pos) /= substring_(j:j) ) then
                found = .false.
            end if
            k = k + 1
            j = j + 1
        end do
        if( found ) then
            idx = i
            found = .false.
        else
            found = .true.
        end if
        i = i + 1
    end do
end function

integer elemental function len_repeati32(n) result(r)
integer, intent(in) :: n
r = n
end function

function repeati32(s, n) result(r)
character(len=1), intent(in) :: s
integer, intent(in) :: n
character(len=len_repeati32(n)) :: r
integer :: i, i1
i1 = 1
do i = 1, n
    r(i:i) = s(i1:i1)
end do
end function

integer elemental function len_repeati64(n) result(r)
integer(i64), intent(in) :: n
r = n
end function

function repeati64(s, n) result(r)
character(len=1), intent(in) :: s
integer(i64), intent(in) :: n
character(len=len_repeati64(n)) :: r
integer :: i, i1, ni32
i1 = 1
ni32 = n
! TODO: Stop using ni32 here
do i = 1, ni32
    r(i:i) = s(i1:i1)
end do
end function

function new_line(c) result(r)
    character(len=1), intent(in) :: c
    character(len=1) :: r
    r = '\n'
end function

integer elemental function cnt_initial_spaces(string) result(r)
    character(len=*), intent(in) :: string
    logical :: is_all_spaces
    integer :: i

    r = len(string)
    if (r == 0) return

    is_all_spaces = .true.
    do i=1, len(string)
        if(string(i:i) /= " ") then
            is_all_spaces = .false.
            exit
        end if
    end do

    ! if string is all spaces, then no need to update r as it is already equal to len(string)
    if (.not. is_all_spaces) then
        r = i - 1
    end if
end function

function adjustl(x) result(r)
    character(len=*),intent(in) :: x
    character(len=len(x)) :: r
    integer :: i, j, initial_spaces_cnt
    j = 1
    initial_spaces_cnt = cnt_initial_spaces(x)

    do i = initial_spaces_cnt + 1, len(x)
        r(j:j) = x(i:i)
        j = j + 1
    end do

    do i = 1, initial_spaces_cnt
        r(j:j) = ' '
        j = j + 1
    end do
end function

function adjustr(x) result(r)
    character(len=*),intent(in) :: x
    character(len=len(x)) :: r
end function

function lgt(x, y) result(r)
    character(len=*),intent(in) :: x
    character(len=*),intent(in) :: y
    logical :: r
end function

function llt(x, y) result(r)
    character(len=*),intent(in) :: x
    character(len=*),intent(in) :: y
    logical :: r
end function

function lge(x, y) result(r)
    character(len=*),intent(in) :: x
    character(len=*),intent(in) :: y
    logical :: r
end function

function lle(x, y) result(r)
    character(len=*),intent(in) :: x
    character(len=*),intent(in) :: y
    logical :: r
end function

subroutine date_and_time(date, time, zone, values)
    character(len=*), intent(out), optional :: date, time, zone
    integer, intent(out), optional :: values(8)
end subroutine

function scan_kind4(string, set, back) result(r)
    character(len=*) :: string
    character(len=*) :: set
    logical, optional :: back
    integer :: r
end function

function scan_kind8(string, set, back) result(r)
    character(len=*) :: string
    character(len=*) :: set
    logical, optional :: back
    integer(8) :: r
end function

function verify_kind4(string, set, back) result(r)
    character(len=*) :: string
    character(len=*) :: set
    logical, optional :: back
    integer :: r
end function

function verify_kind8(string, set, back) result(r)
    character(len=*) :: string
    character(len=*) :: set
    logical, optional :: back
    integer(8) :: r
end function

end module
