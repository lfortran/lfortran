program nested_external_dedup_01
    implicit none
    call outer()
contains
    subroutine outer()
        integer :: a
        a = 0
        call inner1()
        call inner2()
    contains
        subroutine inner1()
            a = a + 1
        end subroutine inner1

        subroutine inner2()
            a = a + 2
        end subroutine inner2
    end subroutine outer
end program nested_external_dedup_01
