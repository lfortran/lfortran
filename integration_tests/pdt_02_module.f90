module pdt_02_mod
  type :: container(rk, ik)
    integer, kind :: rk = 4
    integer, kind :: ik = 8
    integer(kind=ik)  :: i_val(20)
    real(kind=rk)     :: r_val(20)
  end type container
end module