program separate_compilation_67
use separate_compilation_67a, only: mseq
implicit none
interface
    function separate_compilation_67_ext(n) result(r)
        integer, intent(in) :: n
        real :: r(n)
    end function
end interface
integer :: k
k = -3
print *, sum(mseq(-3)), sum(separate_compilation_67_ext(-3))
if (sum(mseq(-3)) /= 0 .or. size(mseq(-3)) /= 0) error stop 1
if (sum(mseq(k)) /= 0 .or. size(mseq(k)) /= 0) error stop 2
if (sum(mseq(2)) /= 14) error stop 3
if (sum(separate_compilation_67_ext(-3)) /= 0) error stop 4
if (size(separate_compilation_67_ext(k)) /= 0) error stop 5
if (sum(separate_compilation_67_ext(2)) /= 14) error stop 6
end program
