module modules_43_fpm_sources
implicit none

type srcfile_t
    character(:), allocatable :: file_name
    character(:), allocatable :: exe_name
    integer(8) :: digest
end type srcfile_t

contains

subroutine add_sources_from_dir(sources)
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)

    logical :: exclude_source(10)
    type(srcfile_t), allocatable :: dir_sources(:)

    allocate(dir_sources(1))

    if (.not. allocated(sources)) then
        sources = pack(dir_sources, .not. exclude_source)
    else
        sources = [sources, pack(dir_sources, .not. exclude_source)]
    end if

end subroutine add_sources_from_dir

end module modules_43_fpm_sources

program modules_43
use modules_43_fpm_sources
implicit none

type(srcfile_t), allocatable, target :: sources(:)
allocate(sources(1))
call add_sources_from_dir(sources)

end program modules_43
