module class_78_module
    public :: fpm_registry_settings, fpm_global_settings
    type :: fpm_global_settings
        type(fpm_registry_settings), allocatable :: registry_settings
    end type
    type :: fpm_registry_settings
        integer :: tag = 0
    end type
end module class_78_module

program class_78
    use class_78_module
    implicit none

    type(fpm_global_settings)     :: global_settings
    type(fpm_registry_settings)   :: tmp
    logical :: ok

    if (allocated(global_settings%registry_settings)) then
        print *, "FAIL: registry_settings should start unallocated."
        stop 1
    else
        print *, "PASS: registry_settings initially unallocated."
    end if

    allocate(global_settings%registry_settings)
    if (.not. allocated(global_settings%registry_settings)) then
        print *, "FAIL: allocation failed."
        stop 1
    else
        print *, "PASS: allocation succeeded."
    end if

    tmp%tag = 42
    global_settings%registry_settings = tmp

    if (global_settings%registry_settings%tag /= 42) then
        print *, "FAIL: assignment into allocatable component did not propagate value."
        stop 1
    else
        print *, "PASS: assignment propagated correctly."
    end if

    call validate_settings(global_settings, ok)
    if (.not. ok) then
        print *, "FAIL: validate_settings detected incorrect state."
        stop 1
    else
        print *, "PASS: validate_settings confirmed correct state."
    end if

    deallocate(global_settings%registry_settings)
    if (allocated(global_settings%registry_settings)) then
        print *, "FAIL: deallocation failed."
        stop 1
    else
        print *, "PASS: deallocation succeeded."
    end if

contains

    subroutine validate_settings(gs, ok)
        type(fpm_global_settings), intent(in) :: gs
        logical, intent(out) :: ok

        ok = .true.

        if (.not. allocated(gs%registry_settings)) ok = .false.

        if (gs%registry_settings%tag /= 42) ok = .false.
    end subroutine validate_settings

end program class_78
