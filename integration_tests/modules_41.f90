module modules_41_fpm_targets
implicit none

type :: fpm_model_t
    character(len=40) :: package_name
end type fpm_model_t

contains

subroutine build_target_list(model)

    type(fpm_model_t), intent(inout), pointer :: model
    integer :: result

    if( model%package_name /= "fpm_targets" ) error stop

    associate (x=>model)
         result = get_object_name()
         if( result /= 0 ) error stop
         print *, result
    end associate

    print *, model%package_name
    if( model%package_name /= "fpm_targets_new" ) error stop

    contains

        function get_object_name() result(int)
            integer :: int
            int = 0
            model%package_name = "fpm_targets_new"
        end function get_object_name

end subroutine build_target_list

end module modules_41_fpm_targets

program modules_41
use modules_41_fpm_targets
implicit none

type(fpm_model_t), target :: fpm_model
type(fpm_model_t), pointer :: fpm_model_ptr

fpm_model%package_name = "fpm_targets"
print *, fpm_model%package_name
fpm_model_ptr => fpm_model
call build_target_list(fpm_model_ptr)

end program
