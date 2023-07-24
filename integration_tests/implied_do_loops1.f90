program implied_do_loop1
    integer :: j
    integer :: a(10) !=(/(j,j=1,10)/)
    a = [(j,j=1,10)]
    call s()
    print*, (a(j),j=1,10)

contains
    subroutine s()
        print *, (j, j = 1, 3)
    end subroutine s
end program
