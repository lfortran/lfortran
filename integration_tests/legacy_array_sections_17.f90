module legacy_array_sections_17_linalg_mod
  implicit none
  interface
    module function stdlib_clange(work) result(res)
      real, intent(inout) :: work(*)   ! assumed-size
      real :: res
    end function stdlib_clange
  end interface
end module legacy_array_sections_17_linalg_mod


submodule (legacy_array_sections_17_linalg_mod) legacy_array_sections_17_linalg_impl
  implicit none
contains

  module function stdlib_clange(work) result(res)
    real, intent(inout) :: work(*)
    real :: res
    res = work(1)
  end function stdlib_clange

end submodule legacy_array_sections_17_linalg_impl

module legacy_array_sections_17_driver_mod
  use legacy_array_sections_17_linalg_mod, only: stdlib_clange
  implicit none
  interface
    module subroutine repro(work)
      real, intent(inout) :: work(*)
    end subroutine repro
  end interface
end module legacy_array_sections_17_driver_mod


submodule (legacy_array_sections_17_driver_mod) legacy_array_sections_17_driver_impl
  implicit none
contains

  module subroutine repro(work)
    real, intent(inout) :: work(*)
    real :: abnrm
    abnrm = stdlib_clange(work(1))

    ! Basic validation
    if (abnrm /= work(1)) then
      error stop "ERROR: stdlib_clange returned wrong value"
    end if

  end subroutine repro

end submodule legacy_array_sections_17_driver_impl



program legacy_array_sections_17
  use legacy_array_sections_17_driver_mod, only: repro
  implicit none

  real :: work(5)

  work = [1.0, 2.0, 3.0, 4.0, 5.0]

  call repro(work)

  print *, "Test passed"

end program legacy_array_sections_17