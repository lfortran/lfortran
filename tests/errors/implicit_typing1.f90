subroutine sub1()
implicit none
x = 1
print *, x
end subroutine sub1

program main
    call sub1()
end program main
