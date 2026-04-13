module separate_compilation_48_targets
use separate_compilation_48a_types
implicit none
type :: build_target_ptr
    type(build_target_t), pointer :: ptr => null()
end type
type :: build_target_t
    type(string_t), allocatable :: macros(:)
end type
contains
subroutine build_target_list(targets, model)
    type(build_target_ptr), intent(out), allocatable :: targets(:)
    type(fpm_model_t), intent(inout), target :: model
    integer :: i, j
    do j=1,size(model%packages)
        associate(sources=>model%packages(j)%sources)
            do i=1,size(sources)
                call add_new_target(targets, package=model%packages(j)%name, &
                    type=3, output_name='obj', source=sources(i))
            end do
        end associate
    end do
end subroutine
subroutine add_new_target(targets, package, type, output_name, source)
    type(build_target_ptr), allocatable, intent(inout) :: targets(:)
    character(*), intent(in) :: package
    integer, intent(in) :: type
    character(*), intent(in) :: output_name
    type(srcfile_t), intent(in), optional :: source
end subroutine
end module

program separate_compilation_48
use separate_compilation_48a_types
use separate_compilation_48_targets
implicit none
type(fpm_model_t) :: model
allocate(model%packages(0))
print *, "ok"
end program
