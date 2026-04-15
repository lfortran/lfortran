program fasta

 implicit none
  integer num, m
  character(len=8) argv
  logical, dimension(:), allocatable :: flags
  integer, parameter :: IM = 139968
  integer, parameter :: IA = 3877
  integer, parameter :: IC = 29573
  character(len=*), parameter :: alu = &
'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' // &
'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' // &
'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' // &
'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' // &
'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' // &
'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' // &
'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA'

  type pair
     character(len=1) c
     real*8 p
  end type pair
  type(pair), dimension(15) :: iub
  type(pair), dimension(4)  :: homosapiens
  homosapiens = (/ pair('a', 0.3029549426680d0), &
                   pair('c', 0.1979883004921d0), &
                   pair('g', 0.1975473066391d0), &
                   pair('t', 0.3015094502008d0) /)
  call makeCumulative(homosapiens)
  
  iub = (/ pair('a', 0.27d0), &
           pair('c', 0.12d0), &
           pair('g', 0.12d0), &
           pair('t', 0.27d0), &
           pair('B', 0.02d0), &
           pair('D', 0.02d0), &
           pair('H', 0.02d0), &
           pair('K', 0.02d0), &
           pair('M', 0.02d0), &
           pair('N', 0.02d0), &
           pair('R', 0.02d0), &
           pair('S', 0.02d0), &
           pair('V', 0.02d0), &
           pair('W', 0.02d0), &
           pair('Y', 0.02d0) /)
  call makeCumulative(iub)

  call get_command_argument(1,argv)
  read(argv,*) num
 

  call makeRepeatFasta('ONE','Homo sapiens alu',alu,num*2)

  call makeRandomFasta('TWO','IUB ambiguity codes',iub,num*3)

  call makeRandomFasta('THREE','Homo sapiens frequency',homosapiens,num*5)

     
contains

  real*8 function getRandom (maxval)
    real*8, intent(in) :: maxval
    integer, save :: last = 42

    last = mod(last * IA + IC, IM)
    getRandom = maxval * last / IM

  end function getRandom

  subroutine makeCumulative(a)
     type(pair), dimension(:), intent(inout) :: a
     integer i
     real*8 :: cp
     
     cp = 0.0d0
     do i=1,size(a)
        cp = cp + a(i)%p
        a(i)%p = cp
     end do
  end subroutine makeCumulative
  
  character(len=1) function selectRandom(a)
      type(pair), dimension(:), intent(inout) :: a
     integer i
     real*8 :: r
     
     r = getRandom(1.0d0)
     selectRandom = 'J'
     do i=1,size(a)
        if (r < a(i)%p) then
           selectRandom = a(i)%c
           return
        end if
     end do
  
  end function selectRandom
  
  subroutine makeRandomFasta(id,desc,a,n)
     character(len=*), intent(in) :: id
     character(len=*), intent(in) :: desc
     type(pair), dimension(:), intent(inout) :: a
     integer, intent(in) :: n
     integer :: todo, i
     integer, parameter :: length = 60
     character(len=length) :: buff

     write(*,'(4a)') '>',id,' ',desc
     todo = n
     do
        if (todo <= 0) exit
        if (todo < length) then
           m = todo
        else
           m = length
        end if
        do i=1,m
           buff(i:i) = selectRandom(a)
        end do
        write(*,'(a)') buff(1:m)
        todo = todo - length
     end do
  end subroutine makeRandomFasta

  subroutine makeRepeatFasta(id,desc,s,n)
     character(len=*), intent(in) :: id
     character(len=*), intent(in) :: desc
     character(len=*), intent(in) :: s
     integer, intent(in) :: n
     integer :: todo, i, j, k, kn
     integer, parameter :: length = 60
     character(len=length) :: buff
     intrinsic len

     write(*,'(4a)') '>',id,' ',desc
     todo = n; k = 1; kn = len(s)
     do
        if (todo <= 0) exit
        if (todo < length) then
           m = todo
        else
           m = length
        end if
        do j=1,m
           if (k > kn) then
              k = 1
           endif
           buff(j:j) = s(k:k)
           k = k + 1
        end do
        write(*,'(a)') buff(1:m)
        todo = todo - length
     end do

  end subroutine makeRepeatFasta

end program fasta
    