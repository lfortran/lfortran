import subprocess
import sys
import tempfile
from pathlib import Path

SOURCE = """\
program p
implicit none
real, allocatable :: a(:)
integer :: i
allocate(a(1000))
do concurrent (i = 1:1000)
    a(i) = i
end do
print *, a(1)
end program
"""


def kernel_source(lfortran, source, flags):
    return subprocess.run(
        [lfortran, "--gpu=cuda", "--show-gpu-kernel-source"] + flags + [str(source)],
        check=True, capture_output=True, text=True).stdout


def main():
    lfortran = sys.argv[1]
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / "p.f90"
        source.write_text(SOURCE)
        plain = kernel_source(lfortran, source, [])
        overlap = kernel_source(lfortran, source, ["--gpu-overlap"])
        assert "griddepcontrol" not in plain, "wait without --gpu-overlap"
        assert "__global__" in overlap
        # The wait is the first statement of the kernel body, ahead of any
        # use of a buffer, and carries the memory clobber.
        assert 'asm volatile("griddepcontrol.wait;" ::: "memory");' in overlap, overlap
        assert "__CUDA_ARCH__ >= 900" in overlap
        kernel = overlap[overlap.index("__global__"):]
        assert (kernel.index("griddepcontrol.wait")
                < kernel.index("__scalar_args."))
    print("ok")


main()
