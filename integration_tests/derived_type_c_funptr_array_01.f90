! Test derived type with c_funptr array field initialized to c_null_funptr
! Issue: https://github.com/lfortran/lfortran/issues/11482
program derived_type_c_funptr_array_01
    use, intrinsic :: iso_c_binding, only: c_funptr, c_null_funptr
    implicit none
    type :: handle_t
        type(c_funptr) :: procs(5) = c_null_funptr
    end type
    type(handle_t) :: h
    print *, "ok"
end program derived_type_c_funptr_array_01
