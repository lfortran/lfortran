module modules_41_fpm_targets
implicit none
private

type :: fpm_model_t
    character(len=40) :: package_name
end type fpm_model_t

contains

subroutine build_target_list(model)

    type(fpm_model_t), intent(inout), pointer :: model

    associate (x=>model)
        print *, get_object_name()
    end associate

    contains

        function get_object_name() result(int)
            integer :: int
            int = 0
            print *, model%package_name
        end function get_object_name

end subroutine build_target_list

end module modules_41_fpm_targets
