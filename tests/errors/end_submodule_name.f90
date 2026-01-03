module mother

    implicit none
    real, parameter :: pi = 4.*atan(1.)
    real :: tau
    
    interface
    module real elemental function pi2tau(pi)
    real, intent(in) :: pi
    end function pi2tau
    end interface
    
    contains
    
end module mother

submodule (mother) daughter
implicit none
contains
module procedure pi2tau
  pi2tau = 2*pi
end procedure pi2tau

end submodule son
