module json_loc_module_m
! Used by json_loc_use_module.f90: the `loc` of the product nodes read back
! from this module's modfile (`dimension`, `do_loop_head`, `array_index`,
! `call_arg`) must point into this file.
implicit none
integer, parameter :: ik(2) = [1, 2]
contains
subroutine s(x)
integer, intent(inout) :: x(2)
integer :: i
do i = 1, 2
    x(i) = ik(i)
end do
call t(x(1))
end subroutine

subroutine t(y)
integer, intent(in) :: y
end subroutine
end module
