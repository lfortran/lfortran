program test_c_interface
    integer  , parameter :: i_max    =  600
    integer  , parameter :: j_max    =  450
    integer :: image_t(j_max, i_max)
    interface
        subroutine show_img(n, m, A) bind(c)
        integer, intent(in) :: n, m
        integer, intent(in) :: A(n,m)
        end subroutine
    end interface

    call show_img(j_max, i_max, image_t)

end program
