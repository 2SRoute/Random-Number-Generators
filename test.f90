program test ! compile using 'gfortran -g -Wall -fcheck=all -fmax-errors=1 -std=f2008 rng.o test.f90
    use rng
    implicit none
    real(kind = srk), parameter :: A = 1570, B = 2864, M = 2**32 - 1 ! rng parameters
    real(kind = srk) :: X = 2007 ! seeds
    integer, parameter :: N = 10000 ! total number of numbers to generate
    integer :: i
    real(kind = srk), dimension(:), allocatable :: Xvals, Xstats ! storage arrays for holding numbers

    allocate(Xvals(N))
    do i = 1, N ! iterates up to N, generating two numbers to be stored in an array for later
        X = lin_cong(X, A, B, M)
        Xvals(i) = X/M
    end do

    allocate(Xstats(3)) ! calculates stats of the generated numbers, prints to screen
    Xstats = stats(Xvals, N)
    print*, "Linear-congruential Algorithm:"
    print*, "Mean: ", Xstats(1)
    print*, "Variance: ", Xstats(2)
    print*, "Standard Error: ", Xstats(3)
    print*, " "
    Xvals = box_muller(Xvals, N, Xstats(1), sqrt(Xstats(2))) ! uses the box-muller method to generate numbers

    Xstats = stats(Xvals, N) ! calculates stats, prints to screen
    print*, "Box-Muller Algorithm:"
    print*, "Mean: ", Xstats(1)
    print*, "Variance: ", Xstats(2)
    print*, "Standard Error: ", Xstats(3)
end program