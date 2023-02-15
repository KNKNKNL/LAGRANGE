module r128
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
end module r128

module pi_def
    use r128
    implicit none
    real(dp) :: pi_real = 4.0_dp*atan(1.0_dp) 
end module pi_def

module recurse_test
    use r128 
    implicit none
contains
    subroutine out128    
        real(dp) :: a = 3.14159265358901234567890123456789_dp
        write(*, *) a 
        print *, floor(a) 
    end subroutine out128
end module recurse_test

subroutine p2r
    print *, 'SSSSSSSSSSSSSSSSSSSS'
    call testtest
end subroutine p2r

subroutine testtest
    print *, 'BBBBBBBBBBBBBBBBBBBBBBBB'
end subroutine testtest

program eee
    use r128
    use recurse_test
    use pi_def
    implicit none
    integer i, j
    real(dp) :: abc(4) = 0
    integer :: x1 = 1, x_r = 15
    real, allocatable :: f(:)
    real, allocatable :: other(:)
    real g(15)
    integer a(3, 3)
    real(dp) pi
    integer :: aaa(3) = [1, 4, 6], bbb(3) = [2, 7, 9], ccc(3)
    ccc = aaa + 3
    print *, ccc 
    print *, 'sqrt', sqrt(4.0)
    pi = 4.0_dp*atan(1.0_dp)
    print *, pi 
    allocate(f(x_r))
    do i = 1, x_r
        f(i) = i
    end do
    print *, exp(1.0_dp )
    call out128
    call p2r
    a(:, :) = 2
    print *, a
    print *, f
    g(:) = f(:)
    print *, g
    g = 0
    print *, g 
    print *, 'AA', other
    print *, 'pi = ', pi_real
    call est(abc(2))
    print *, 'abc = ', abc(2)
    print *, modulo(-1, 10)
    print *, exp(4.0)*1.266065877752008
end program eee

subroutine est(x)
    use r128
    real(dp), intent(out) ::  x
    print *, 'x = ', x
    x = 128
end subroutine est