program select_type_05
implicit none

type point
    real :: x, y
end type point

type line
    type(point) :: start
    type(point) :: end
end type line

! TODO: test with point_arr(10)
type(point) :: point_arr
type(line) :: line_arr

integer :: case_int, case_real, case_default, case_point, case_line
integer :: intarr(10)
real(8) :: realarr(10)
complex :: complexarr(10)

call get_sum(intarr, case_int)
call get_sum(realarr, case_real)
call get_sum(complexarr, case_default)
call get_sum_polymorphic(point_arr, case_point)
call get_sum_polymorphic(line_arr, case_line)

print *, case_int, case_real, case_default, case_point, case_line

if (case_int /= 0) error stop
if (case_real /= 1) error stop
if (case_default /= 2) error stop
if (case_point /= 3) error stop
if (case_line /= 4) error stop

contains

subroutine get_sum(generic, selected_case)
    class(*) :: generic(:)
    integer, intent(out) :: selected_case
    integer :: i, isum
    real :: rsum

    ! TODO: Add case type is (point)
    select type(generic)
        type is (integer)
            isum = 0
            do i = 1, 10
                isum = isum + generic(i)
            end do
            print *, isum
            selected_case = 0
        type is (real(8))
            rsum = 0.0
            do i = 1, 10
                rsum = rsum + generic(i)
            end do
            print *, rsum
            selected_case = 1
        class default
            print *, '*get_sum* crud -- procedure does not know about this type'
            selected_case = 2
    end select
end subroutine get_sum

subroutine get_sum_polymorphic(generic, selected_case)
    class(*) :: generic
    integer, intent(out) :: selected_case

    select type(generic)
        type is (point)
            print *, "point"
            selected_case = 3
        type is (line)
            print *, "line"
            selected_case = 4
        class default
            print *, '*get_sum_polymorphic* crud -- procedure does not know about this type'
            selected_case = 2
    end select
end subroutine get_sum_polymorphic

end program select_type_05
