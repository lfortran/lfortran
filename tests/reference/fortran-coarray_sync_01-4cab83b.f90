type, bind(c) :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarray_sync_01
implicit none
interface
    subroutine f__module_prif_prif_stop(quiet, stop_code_int, stop_code_char)
        logical(1), intent(in) :: quiet
        character(len=*, kind=1), intent(in), optional :: stop_code_char
        integer(4), intent(in), optional :: stop_code_int
    end subroutine f__module_prif_prif_stop
end interface
interface
    subroutine f__module_prif_prif_sync_all(stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_sync_all
end interface
interface
    subroutine f__module_prif_prif_sync_images(image_set, stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), dimension(:), intent(in), optional :: image_set
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_sync_images
end interface
interface
    subroutine f__module_prif_prif_sync_memory(stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_sync_memory
end interface
interface
    subroutine lcompilers_prif_start(stat) bind(c, name = "lcompilers_prif_start")
        integer(4), intent(out) :: stat
    end subroutine lcompilers_prif_start
end interface
integer(4), dimension(2) :: images
call f__module_prif_prif_sync_all()
call f__module_prif_prif_sync_memory()
images(1) = 1
images(2) = 2
call f__module_prif_prif_sync_images(images)
call f__module_prif_prif_sync_images()
call f__module_prif_prif_stop(.false.)

contains

subroutine f__lcompilers_collective_bootstrap()
    integer(4) :: stat
    call lcompilers_prif_start(stat)
    if (stat /= 0) then
        error stop
    end if
end subroutine f__lcompilers_collective_bootstrap

end program coarray_sync_01
