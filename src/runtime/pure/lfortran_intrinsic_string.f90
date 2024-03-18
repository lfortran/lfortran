module lfortran_intrinsic_string
    use, intrinsic :: iso_fortran_env, only: i64 => int64
implicit none

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

integer elemental function index(string_, substring_, back) result(idx)
    character(len=*), intent(in) :: string_
    character(len=*), intent(in) :: substring_
    logical, optional, intent(in) :: back
    integer :: i, j, k, pos, len_str, len_sub
    logical :: found
    found = .true.
    idx = 0
    i = 1
    len_str = len(string_)
    len_sub = len(substring_)

    if (len_str < len_sub) then
        found = .false.
        return
    end if

    do while (i < len_str .and. found)
        k = 0
        j = 1
        do while (j <= len_sub .and. found)
            pos = i + k
            if( string_(pos:pos) /= substring_(j:j) ) then
                found = .false.
            end if
            k = k + 1
            j = j + 1
        end do
        if (found) then
            idx = i
            if (back .eqv. .true.) then
                found = .true.
            else
                found = .false.
            end if
        else
            found = .true.
        end if
        i = i + 1
    end do
end function

function new_line(c) result(r)
    character(len=1), intent(in) :: c
    character(len=1) :: r
    r = '\n'
end function

subroutine date_and_time(date, time, zone, values)
    character(len=*), intent(out), optional :: date, time, zone
    integer, intent(out), optional :: values(8)
end subroutine

end module
