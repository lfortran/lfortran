module math_submodule_10
    implicit none

    interface logspace
        module function func1(n, base) result(res)
            integer, intent(in) :: n
            integer, intent(in) :: base
            real :: res(max(n,0))
        end function func1

        module function func2(n, base) result(res)
            integer, intent(in) :: n
            real, intent(in) :: base
            integer :: res(max(n, 0))
        end function func2
    end interface

end module

submodule (math_submodule_10) log_submodule_10
  implicit none

contains

  module procedure func1
    real, parameter :: array(2) = [1.0 , 2.0]
    res = array
  end procedure

  module procedure func2
    integer, parameter :: array(2) = [1 , 2]
    res = array
  end procedure

end submodule

program submodule_10
    use math_submodule_10
    implicit none

    integer, parameter :: n = 2
    integer, parameter :: base = 1

    real :: result1(n)
    integer :: result2(n)

    result1 = logspace(n, base)
    result2 = logspace(n, base)

    print *, result1
    print *, result2
    if (.not. all(result1 == [1.0, 2.0])) error stop
    if (.not. all(result2 == [1, 2])) error stop
end program