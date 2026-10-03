program intrinsics_451
implicit none

character(len=3) :: arr(4)
integer :: idx

arr(1) = "abc"
arr(2) = "def"
arr(3) = "ghi"
arr(4) = "def"

idx = findloc(arr, "def", dim=1)
print *, idx
if (idx /= 2) error stop

idx = findloc(arr, "def", dim=1, back=.true.)
print *, idx
if (idx /= 4) error stop

idx = findloc(arr, "xyz", dim=1)
print *, idx
if (idx /= 0) error stop

call test_assumed_len(arr)

contains

subroutine test_assumed_len(w)
    character(len=*), intent(in) :: w(:)
    character(len=len(w)) :: v(4)
    integer :: i
    v = w
    i = findloc(v, "ghi", dim=1)
    print *, i
    if (i /= 3) error stop
end subroutine

end program
