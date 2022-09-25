program main
integer:: check_save
check_save = f()
print *, check_save
save
print *, f()
print *, f()
print *, f2(1, 2, 3)
call f3(2, 4, 1, 5)

contains

integer function f()
integer:: done
save
done = 1
done = done + 1
print *, done
f = done
end

integer function f2(a, b, c)
integer, intent(in) :: a, b, c
f2 = a + b + c
save
end

subroutine f3(a, b, c, d)
integer, intent(in) :: a, c, d, b
save
integer :: j
j = a + 1 + c + d
end subroutine

end program
