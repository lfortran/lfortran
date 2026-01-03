program openmp_02
use omp_lib

call omp_set_num_threads(4)

print *, "Number of threads: ", omp_get_max_threads()
if (omp_get_max_threads() /= 4) error stop
end program
