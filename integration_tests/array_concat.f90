program array_concat
integer :: A = 2
integer :: B(3) = [1,2,3]
real :: x(4) = [1.0, 2.0, -3.0, 4.0]
integer, allocatable :: B2(:)
integer :: C(4) 
logical :: D(4)

C = [B, A]
print *, C
if( C(1) /= 1 .or. C(2) /= 2 .or. C(3) /= 3 .or. C(4) /= 2 ) error stop
print *, B(1:3)

D = [B, A] > 1
print *, D
if( any(D .neqv. [.false., .true., .true., .true.]) ) error stop

allocate(B2(size(B)))
B2 = B
D = [B2, A] > 1
print *, D
if( any(D .neqv. [.false., .true., .true., .true.]) ) error stop

D = [B([3,2,1]), A] > 1
print *, D
if( any(D .neqv. [.true., .true., .false., .true.]) ) error stop

D = [B2(2:3), A, A] > 1
print *, D
if( any(D .neqv. [.true., .true., .true., .true.]) ) error stop

call temp(x)
contains
subroutine temp(x)
     real, intent(in) :: x(:)
     real :: xx(size(x)) 
     real :: tn(size(x) - 1) 
    logical :: y
     tn = x(1:size(x)-1)
     xx = x
    y = any(xx <= 0 .and. abs([tn, 0.0]) > 0)
    print *, y 
    if( y .neqv. .true.) error stop
end subroutine

end program