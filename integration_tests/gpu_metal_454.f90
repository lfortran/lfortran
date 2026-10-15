program gpu_metal_454
! An associate name used inside a BLOCK nested in the ASSOCIATE of an
! offloaded do concurrent stands for its selector there too.
implicit none
integer :: k(4), r(4), s(4), u(4), w(4), i
k = [1, 2, 3, 4]

do concurrent (i = 1:4)
    associate (m => k(i))
        block
            integer :: t
            t = m * 10
            block
                r(i) = t + m
            end block
        end block
    end associate
end do

do concurrent (i = 1:4)
    block
        associate (m => k(i) + 1)
            block
                s(i) = m * 2
            end block
        end associate
    end block
end do

do concurrent (i = 1:4)
    associate (a => k(i))
        associate (b => a * 3)
            block
                u(i) = a + b
            end block
        end associate
    end associate
end do

w = 0
do concurrent (i = 1:4)
    associate (e => w(i))
        block
            e = k(i) * 7
        end block
    end associate
end do

print *, r
print *, s
print *, u
print *, w
if (any(r /= [11, 22, 33, 44])) error stop 1
if (any(s /= [4, 6, 8, 10])) error stop 2
if (any(u /= [4, 8, 12, 16])) error stop 3
if (any(w /= [7, 14, 21, 28])) error stop 4
end program
