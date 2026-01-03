program arrays_16_func

real :: arr(17)
integer :: i
real :: sum2

do i = 1, 17
    arr(i) = i
end do

call imply(arr, 17, sum2)

print *, sum2
if( sum2 /= 153 ) error stop

contains

subroutine imply(f1, l1out, sum2)
    integer, intent(in) :: l1out
    real :: f1(l1out)
    real, intent(out) :: sum2
    integer :: i1

    sum2 = 0.0
    do i1 = 1, l1out
        sum2 = sum2 + f1(i1)
    end do

end subroutine

end program
