module select_type2
implicit none

contains

subroutine get_sum(generic)
    class(*), intent(in) :: generic(:)
    integer :: i, isum
    real :: rsum
    select type(generic)
        type is (integer)
        isum = 0
        do i = 0, 10
            isum = isum + generic(i)
        end do
        print(isum)
        type is (real(8))
        rsum = 0.0
        do i = 0, 10
            rsum = rsum + generic(i)
        end do
        print(rsum)
        class default
            print *, '*get_sum* crud -- procedure does not know about this type'
    end select
end subroutine get_sum

end module select_type2
