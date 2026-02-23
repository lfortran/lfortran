program slash_init_warning_paths
    implicit none

    enum, bind(c)
        enumerator :: red/1/
    end enum

    type :: t
        integer :: i
    end type t

    type(t) :: x/t(1)/
    integer :: y/2/
end program slash_init_warning_paths
