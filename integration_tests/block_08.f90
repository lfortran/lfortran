function diff_1_int32(n_) result(y)
integer, allocatable :: y(:)
integer :: size_work
integer, intent(in) :: n_

if (n_ <= 0) return
allocate(y(n_))

block
integer :: work(size_work)
y = 13
end block

end function diff_1_int32

program block_08
integer, allocatable :: x(:)
interface
    function diff_1_int32(n_) result(y)
        integer, allocatable :: y(:)
        integer, intent(in) :: n_
    end function diff_1_int32
end interface

allocate(x(10))
x = diff_1_int32(10)
print *, x
if (any(x /= 13)) error stop
end program
