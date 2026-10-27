module submodule_58_n
    implicit none
    character(len=4) :: n_tag = "nnnn"
contains
    integer function twice(i)
        integer, intent(in) :: i
        twice = 2*i
    end function
end module

module submodule_58_m
    use submodule_58_n
    implicit none
    private
    public :: counter, label, get_value, bump, get_label, priv_helper
    integer :: counter = 1
    character(len=8) :: label = "start"
    interface
        module function get_value() result(r)
            integer :: r
        end function
        module subroutine bump(k)
            integer, intent(in) :: k
        end subroutine
        module function get_label() result(r)
            character(len=8) :: r
        end function
    end interface
contains
    ! Public: older GFortran does not export a private procedure that only a
    ! separately compiled submodule calls.
    integer function priv_helper(i)
        integer, intent(in) :: i
        priv_helper = i + 100
    end function
end module
