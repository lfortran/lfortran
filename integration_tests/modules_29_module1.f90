module fpm_git1
    implicit none

    type :: git_target_t
        character(len=:), allocatable :: url
        character(len=:), allocatable :: object
    end type git_target_t

contains

    function git_target_branch(url, branch) result(self)
        character(len=*), intent(in) :: url
        character(len=*), intent(in) :: branch
        type(git_target_t) :: self
        self%url = url
        self%object = branch
    end function git_target_branch

end module fpm_git1
