module averager_library

    use, intrinsic :: iso_fortran_env, only: real64
 
    implicit none
    private
    
    public :: simple_average, pairwise_average
    
    abstract interface :: INumeric
       integer | real(real64)
    end interface INumeric
 
 contains
    
    function simple_sum{INumeric :: T}(x) result(s)
       type(T), intent(in) :: x(:)
       type(T)             :: s
       integer :: i
       s = T(0)
       do i = 1, size(x)
          s = s + x(i)
       end do
    end function simple_sum
    
    function pairwise_sum{INumeric :: T}(x) result(s)
       type(T), intent(in) :: x(:)
       type(T)             :: s
       integer :: m
       if (size(x) <= 2) then
          s = simple_sum(x)
       else
          m = size(x) / 2
          s = pairwise_sum(x(:m)) + pairwise_sum(x(m+1:))
       end if
    end function pairwise_sum
 
    function simple_average{INumeric :: T}(x) result(a)
       type(R), intent(in) :: x(:) ! TypeType vs TypeReal
       type(T)             :: a
       a = simple_sum(x) / T(size(x)) ! FuncCallOrArray T vs FuncCallOrArray real
    end function simple_average
 
    function pairwise_average{INumeric :: T}(x) result(a)
       type(T), intent(in) :: x(:) ! should it be DimensionExpr?
       type(T)             :: a
       a = pairwise_sum(x) / T(size(x))
    end function pairwise_average
 
 end module averager_library
 
 program main
 
    ! dependencies on intrinsic constants
    use, intrinsic :: iso_fortran_env, only: real64
 
    ! dependencies on implementations
    use averager_library, only: simple_average, pairwise_average
 
    implicit none
    
    ! declarations
    integer,      parameter :: xi(5) = [1, 2, 3, 4, 5]
    real(real64), parameter :: xf(5) = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]
 
    integer :: key
 
    write(*,'(a)') 'Simple   sum average: 1'
    write(*,'(a)') 'Pairwise sum average: 2'
    write(*,'(a)',advance='no') 'Choose an averaging method: '
    read(*,*) key
 
    select case (key)
    case (1)
       print '(i8)',   simple_average(xi)
       print '(f8.5)', simple_average(xf)
    case (2)
       print '(i8)',   pairwise_average(xi)
       print '(f8.5)', pairwise_average(xf)
    case default
       stop 'Case not implemented!'
    end select
 
 end program main