program iand_ior_ieor_boz
implicit none
integer :: k = 42
if (iand(k, z'3456') /= 2) error stop
if (iand(z'1234', k) /= 32) error stop
if (ior(k, z'3456') /= 13438) error stop
if (ior(z'1234', k) /= 4670) error stop
if (ieor(k, z'3456') /= 13436) error stop
if (ieor(z'1234', k) /= 4638) error stop
end program
