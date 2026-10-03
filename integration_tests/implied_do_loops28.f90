module mre_types_m
    implicit none
  
    type :: operands_t
      real :: actual, expected
    end type
  
    type :: diagnosis_t
      logical :: passed = .false.
      character(len=:), allocatable :: diagnostics_string
    end type
  
    type :: tensor_t
      real, allocatable :: vals(:)
    contains
      procedure :: values
    end type
  
    interface operator(.approximates.)
      module procedure approx
    end interface
  
    interface operator(.within.)
      module procedure within_tol
    end interface
  
    interface operator(.all.)
      module procedure all_pass
    end interface
  
  contains
  
    pure function values(self) result(v)
      class(tensor_t), intent(in) :: self
      real, allocatable :: v(:)
      v = self%vals
    end function
  
    elemental function approx(a, b) result(ops)
      real, intent(in) :: a, b
      type(operands_t) :: ops
      ops%actual = a
      ops%expected = b
    end function
  
    elemental function within_tol(ops, tol) result(res)
      type(operands_t), intent(in) :: ops
      real, intent(in) :: tol
      type(diagnosis_t) :: res
      res%passed = abs(ops%actual - ops%expected) <= tol
    end function
  
    pure function all_pass(diagnoses) result(res)
      type(diagnosis_t), intent(in) :: diagnoses(:)
      type(diagnosis_t) :: res
      res%passed = all(diagnoses%passed)
    end function
  
  end module mre_types_m
  
    program implied_do_loops28
    use mre_types_m
    implicit none
  
    type(tensor_t), allocatable :: inputs(:), outputs(:)
    type(diagnosis_t) :: test_result, diagnoses_i
    integer :: i, n
    real, parameter :: tolerance = 1.0e-6
  
    n = 5
    allocate(inputs(n), outputs(n))
    do i = 1, n
      inputs(i)  = tensor_t([real(i), real(2*i)])
      outputs(i) = tensor_t([real(i), real(2*i)])
    end do
  
    do i = 1, n
      diagnoses_i = all_pass(outputs(i)%values() .approximates. inputs(i)%values() .within. tolerance)
      if (.not. diagnoses_i%passed) error stop
    end do
  
    test_result = .all. [(outputs(i)%values() .approximates. inputs(i)%values() .within. tolerance, i=1,n)]
    if (.not. test_result%passed) error stop
  
  end program implied_do_loops28