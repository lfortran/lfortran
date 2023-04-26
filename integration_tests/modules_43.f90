module fpm_sources
implicit none
private

type srcfile_t
    character(:), allocatable :: file_name
    character(:), allocatable :: exe_name
    integer(8) :: digest
end type srcfile_t

contains

subroutine add_sources_from_dir(sources)
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)

    logical :: exclude_source(:)
    type(srcfile_t), allocatable :: dir_sources(:)

    if (.not. allocated(sources)) then
        sources = pack(dir_sources, .not. exclude_source)
    else
        sources = [sources, pack(dir_sources, .not. exclude_source)]
    end if

end subroutine add_sources_from_dir

end module fpm_sources
