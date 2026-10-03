! Test for https://github.com/lfortran/lfortran/issues/7952
! move_alloc with unallocated source must deallocate destination
! Exact MRE from issue body
program badalloc
  implicit none
  type,abstract :: foo
  end type foo
  type, extends(foo) :: bar
  end type bar
  type(bar), allocatable :: x
  class(foo), allocatable :: y
  call move_alloc(x, y)
  if(allocated(y)) error stop 'Y SHOULD BE UNALLOCATED!'
  print "(A)", 'Y IS UNALLOCATED.'
end program badalloc
