type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarray_sync_01
implicit none
integer(4), dimension(2) :: images
integer(4) :: stat
call __module_prif_prif_init(stat)
call __module_prif_prif_sync_all()
call __module_prif_prif_sync_all()
call __module_prif_prif_sync_memory()
images(1) = 1
images(2) = 2
call __module_prif_prif_sync_images(images)
call __module_prif_prif_sync_images()
call __module_prif_prif_stop(.false.)

contains

interface
    subroutine __module_prif_prif_init(exit_code)
        integer(4), intent(out) :: exit_code
    end subroutine __module_prif_prif_init
end interface

interface
    subroutine __module_prif_prif_stop(quiet, stop_code_int, stop_code_char)
        logical(1), intent(in), value :: quiet
        character(len=*, kind=1), intent(in), optional, value :: stop_code_char
        integer(4), intent(in), optional, value :: stop_code_int
    end subroutine __module_prif_prif_stop
end interface

interface
    subroutine __module_prif_prif_sync_all(stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_sync_all
end interface

interface
    subroutine __module_prif_prif_sync_images(image_set, stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), dimension(:), intent(in), optional :: image_set
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_sync_images
end interface

interface
    subroutine __module_prif_prif_sync_memory(stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_sync_memory
end interface

end program coarray_sync_01
