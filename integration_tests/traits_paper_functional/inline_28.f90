  real(real64) :: dtot(2)
  real(real32) :: stot(2)
  procedure(sum_real64), pointer :: dsum

  abstract interface
     function sum_real64(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64)             :: s
     end function sum_real64  
  end interface
  
  dsum => sum{real(real64)}
  dtot(1) = dsum([1.d0,2.d0,3.d0,4.d0,5.d0])
  dtot(2) = dsum([2.d0,4.d0,6.d0,8.d0])

  associate( ssum => sum{real(real32)} )
     stot(1) = ssum([1.,2.,3.,4.,5.])
     stot(2) = ssum([2.,4.,6.,8.])     
  end associate
