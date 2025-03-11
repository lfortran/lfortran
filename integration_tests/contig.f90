module test_mod
    implicit none
 contains
    subroutine sub_test(a,b)
       real, dimension(:), intent(in) , contiguous :: a
       real, dimension(:), intent(out), contiguous :: b
       b = a
    end subroutine sub_test
 end module test_mod
 
 program contig
    use test_mod
    implicit none
    real, dimension(5) :: a,b
    a = 1.0
    call sub_test(a,b)
    print *, b
 end program contig