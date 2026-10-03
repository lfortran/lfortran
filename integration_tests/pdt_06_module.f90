module pdt_06_inner_m
  implicit none
  type inner(k)
    integer, kind :: k = kind(1.)
    real(k) :: v
  end type
end module
