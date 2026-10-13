  type(SimpleSum) :: simple

  integer      :: integer_total
  real(real64) :: double_total
  
  integer_total = simple%sum{integer}([1,2,3,4,5])
  double_total  = simple%sum{real(real64)}([1.d0,2.d0,3.d0,4.d0,5.d0])
