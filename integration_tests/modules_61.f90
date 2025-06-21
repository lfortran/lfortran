module modules_61_module
    implicit none
    private
    public :: public_n_lines

contains

    integer function n_lines(me)
        integer, intent(in) :: me
        n_lines = me * 2
    end function n_lines

    integer function public_n_lines(me)
        integer, intent(in) :: me
        if (me /= 12) error stop
        public_n_lines = n_lines(me)
    end function public_n_lines

end module modules_61_module

program modules_61
    use modules_61_module
    implicit none
    integer :: n_lines
    n_lines = public_n_lines(12)
    print *, "n_lines = ", n_lines
    if (n_lines /= 24) error stop
end program modules_61
