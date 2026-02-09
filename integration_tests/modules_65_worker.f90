module modules_65_worker
    use modules_65_base, only: holder_t
    use modules_65_child, only: child_t
    implicit none

contains

    function classify(h) result(res)
        type(holder_t), intent(in) :: h
        integer :: res
        res = -1
        select type (x => h%item)
        class is (child_t)
            res = x%extra
        class default
            res = x%width
        end select
    end function classify

end module modules_65_worker
