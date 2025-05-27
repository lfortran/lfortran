module interfaces

    use, intrinsic :: iso_fortran_env, only: real64
 
    implicit none
    private
    
    public :: INumeric, ISum, IAverager
    
    abstract interface :: INumeric
       integer | real(real64)
    end interface INumeric
 
    abstract interface :: ISum
       function sum{INumeric :: T}(x) result(s)
          type(T), intent(in) :: x(:)
          type(T)             :: s
       end function sum
    end interface ISum
 
    abstract interface :: IAverager
       function average{INumeric :: T}(x) result(a)
          type(T), intent(in) :: x(:)
          type(T)             :: a
       end function average
    end interface IAverager
 
 end module interfaces
 
 module simple_library
 
    use interfaces, only: ISum, INumeric
 
    implicit none
    private
 
    public :: SimpleSum
    
    type, sealed, implements(ISum) :: SimpleSum
    contains
       procedure, nopass :: sum
    end type SimpleSum
 
 contains
    
    function sum{INumeric :: T}(x) result(s)
       type(T), intent(in) :: x(:)
       type(T)             :: s
       integer :: i
       s = T(0)
       do i = 1, size(x)
          s = s + x(i)
       end do
    end function sum
 
 end module simple_library
 
 module pairwise_library
 
    use interfaces, only: ISum, INumeric
    
    implicit none
    private
 
    public :: PairwiseSum
    
    type, sealed, implements(ISum) :: PairwiseSum
       private
       class(ISum), allocatable :: other
    contains
       procedure, pass :: sum
    end type PairwiseSum
 
 contains
 
    function sum{INumeric :: T}(self,x) result(s)
       type(PairwiseSum), intent(in) :: self
       type(T),           intent(in) :: x(:)
       type(T)                       :: s
       integer :: m
       if (size(x) <= 2) then
          s = self%other%sum(x)
       else
          m = size(x) / 2
          s = self%sum(x(:m)) + self%sum(x(m+1:))
       end if
    end function sum
 
 end module pairwise_library
 
 module averager_library
 
    use interfaces, only: IAverager, ISum, INumeric
 
    implicit none
    private
 
    public :: Averager
    
    type, sealed, implements(IAverager) :: Averager
       private
       class(ISum), allocatable :: drv
    contains
       procedure, pass :: average
    end type Averager
 
 contains
 
    function average{INumeric :: T}(self,x) result(a)
       type(Averager), intent(in) :: self
       type(T),        intent(in) :: x(:)
       type(T)                    :: a
       a = self%drv%sum(x) / T(size(x))
    end function average
 
 end module averager_library
 
 program main
 
    ! dependencies on abstractions
    use interfaces,       only: IAverager
 
    ! dependencies on implementations
    use simple_library,   only: SimpleSum
    use pairwise_library, only: PairwiseSum
    use averager_library, only: Averager
 
    implicit none
    
    ! declarations
    integer :: key
    class(IAverager), allocatable :: avs, avp, av
 
    ! use of enhanced structure constructors
    avs = Averager(drv = SimpleSum())
    avp = Averager(drv = PairwiseSum(other = SimpleSum()))
 
    write(*,'(a)') 'Simple   sum average: 1'
    write(*,'(a)') 'Pairwise sum average: 2'
    write(*,'(a)',advance='no') 'Choose an averaging method: '
    read(*,*) key
 
    select case (key)
    case (1)
       ! simple sum case
       av = avs
    case (2)
       ! pairwise sum case
       av = avp 
    case default
       stop 'Case not implemented!'
    end select
 
    print '(i8)',   av%average([1, 2, 3, 4, 5])
    print '(f8.5)', av%average([1.d0, 2.d0, 3.d0, 4.d0, 5.d0])
 
 end program main