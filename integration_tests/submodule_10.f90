module math_submodule_10
    implicit none

    interface
        module function func(n, base) result(res)
            integer, intent(in) :: n
            integer, intent(in) :: base
            real :: res(max(n,0))
        end function func
    end interface

end module

submodule (math_submodule_10) log_submodule_10
  implicit none

contains

  module procedure func
    real, parameter :: array(2) = [1.0 , 2.0]
    res = array
  end procedure

end submodule

program submodule_10
    use math_submodule_10
    implicit none

    integer, parameter :: n = 2
    integer, parameter :: base = 1
    real :: result(n)

    result = func(n, base)

    print *, result
    if (.not. all(result == [1.0, 2.0])) error stop
end program