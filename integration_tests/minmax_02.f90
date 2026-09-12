program minmax_02
! The result of MAX/MIN with character arguments has the length of the longest
! argument, and a shorter selected argument is blank-padded on the right.
implicit none
character(len=1) :: a(2)
character(len=2) :: b(2), r(2)
character(len=1) :: s1
character(len=2) :: s2
character(len=6) :: s3
integer :: n

! Arrays: max(['A','Z'], ['BB','Y ']) == ['BB', 'Z ']
a(1) = 'A'
a(2) = 'Z'
b(1) = 'BB'
b(2) = 'Y '
r = max(a, b)
if (r(1) /= 'BB') error stop
if (r(2) /= 'Z ') error stop
if ('[' // r(1) // ']' /= '[BB]') error stop
if ('[' // r(2) // ']' /= '[Z ]') error stop
r = min(a, b)
if ('[' // r(1) // ']' /= '[A ]') error stop
if ('[' // r(2) // ']' /= '[Y ]') error stop

! Scalars, shorter argument first
s1 = 'Z'
s2 = 'BB'
if (len(max(s1, s2)) /= 2) error stop
if ('[' // max(s1, s2) // ']' /= '[Z ]') error stop
if (len(min(s1, s2)) /= 2) error stop
if ('[' // min(s1, s2) // ']' /= '[BB]') error stop

! Scalars, longer argument first
if (len(max(s2, s1)) /= 2) error stop
if ('[' // max(s2, s1) // ']' /= '[Z ]') error stop
if ('[' // min(s2, s1) // ']' /= '[BB]') error stop

! More than two arguments
s3 = 'cc'
if (len(max(s1, s2, s3)) /= 6) error stop
if ('[' // max(s1, s2, s3) // ']' /= '[cc    ]') error stop
if ('[' // min(s1, s2, s3) // ']' /= '[BB    ]') error stop

! Compile time
if (len(max('Z', 'BB')) /= 2) error stop
if ('[' // max('Z', 'BB') // ']' /= '[Z ]') error stop
if (len(min('Z', 'BB')) /= 2) error stop
if ('[' // min('Z', 'BB') // ']' /= '[BB]') error stop
if ('[' // max('a', 'bbbb', 'cc') // ']' /= '[cc  ]') error stop
if ('[' // min('a', 'bbbb', 'cc') // ']' /= '[a   ]') error stop

! Runtime length
n = 4
call check_runtime_length(n)

contains

    subroutine check_runtime_length(m)
    integer, intent(in) :: m
    character(len=3) :: c3
    character(len=m) :: v
    c3 = 'AAA'
    v = 'ZZ'
    if (len(max(c3, v)) /= 4) error stop
    if ('[' // max(c3, v) // ']' /= '[ZZ  ]') error stop
    if ('[' // min(c3, v) // ']' /= '[AAA ]') error stop
    end subroutine check_runtime_length

end program minmax_02
