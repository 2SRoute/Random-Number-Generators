program test
    use rng
    implicit none
    real(kind = srk), parameter :: A = 1570, B = 2864, M = 2**32 - 1 ! rng parameters
    real(kind = srk) :: X = 2007, Y = 1977 ! seeds
    integer, parameter :: N = 10000 ! total number of numbers to generate
    integer :: i
    real(kind = srk), dimension(:), allocatable :: Xvals, Yvals ! storage arrays for holding numbers

    allocate(Xvals(N))
    allocate(Yvals(N))

    do i = 1, N ! iterates up to N, generating two numbers to be stored in an array for later
        X = lin_cong(X, A, B, M)
        Xvals(i) = X

        Y = lin_cong(Y, A, B, M)
        Yvals(i) = Y
    end do
end program ! compile using 'gfortran -g -Wall -fcheck=all -fmax-errors=1 -std=f2008 rng.o test.f90