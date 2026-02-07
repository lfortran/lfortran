module modules_66_worker
    use modules_66_base, only: container_t
    implicit none

contains

    function extract_int(c) result(res)
        type(container_t), intent(in) :: c
        integer :: res
        res = -1
        select type (x => c%item)
        type is (integer)
            res = x
        class default
            res = 0
        end select
    end function extract_int

end module modules_66_worker
