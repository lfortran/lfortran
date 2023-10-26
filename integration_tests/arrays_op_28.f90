program arrays_op_28
implicit none

    real :: R(5), V(5), Vmid(3)
    R = 45
    V = 23
    Vmid(:3) = func(R(:4), V(:4))
    print '(f10.2)', Vmid
    if( any(Vmid /= 1035.00) ) error stop

contains

function func(R, V) result(Vmid)
    real, intent(in) :: R(:), V(:)
    real :: Vmid(size(R)-1)
    integer :: i
    do i = 1, size(Vmid)
        Vmid(i) = R(i)*V(i)
    end do
end function

end program arrays_op_28
