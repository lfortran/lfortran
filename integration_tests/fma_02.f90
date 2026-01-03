module fma_02_mod

    contains

subroutine ss(x)
    integer, intent(in) :: x
    real(4) :: b(x)
    b  = 1 
    b = -b + 1.0*b ! Test FMA opt. with `--fast` (pointerToDataArray)
    print *, b
    if(any(b /= 0.0)) error stop
    
end subroutine 

end module 

program fma_02
    use fma_02_mod

    implicit none
    real ::b(10)

    b = 1
    b = -b + b*1.0 ! Test FMA opt. with `--fast` (fixedSizeAsrray)
    print *, b
    if(any(b /= 0.0)) error stop

    call ss(10)

end program 