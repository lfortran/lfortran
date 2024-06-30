program character_06
integer :: a
character :: b(1)
a = 1
call error_subroutine(b,a)
end program

subroutine error_subroutine(b, a)
integer :: a, i
character :: b(a)
b = 'x'
print *, b
do i = 1, a
    if (b(i) /= 'x') error stop
end do 
return
end
