module json_loc_module_loop_m
! Used by json_loc_use_module_loop.f90: the `do_loop_head` of the loop and
! the `call_arg` of `call s(i)` are read back from this module's modfile, and
! their `loc` must point into this file.
contains
recursive subroutine s(i)
integer :: i
do i = 1, 1
end do
call s(i)
end subroutine
end module
