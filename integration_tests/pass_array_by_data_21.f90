program pass_array_by_data_21
    implicit none
    interface
        subroutine pass_array_by_data_21_first(x, r)
            integer, intent(in) :: x(:)
            integer, intent(out) :: r
        end subroutine
        subroutine pass_array_by_data_21_add(x, s)
            integer, intent(inout) :: x(:)
            integer, intent(out) :: s
        end subroutine
    end interface
    integer, target :: a(3)
    integer, pointer :: p(:)
    integer :: r
    a = [1, 2, 3]
    p => a
    call pass_array_by_data_21_first(p, r)
    if (r /= 1) error stop
    call pass_array_by_data_21_add(p, r)
    if (r /= 9) error stop
    if (any(a /= [2, 3, 4])) error stop
    print *, "OK"
end program
