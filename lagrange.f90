module r64
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
end module r64

module math_const
    use r64
    implicit none
    real(dp) :: pi = 4.0_dp*atan(1.0_dp)
end module math_const

module main_var
    use r64
    use math_const
    implicit none
    real(dp) :: r_range = 1.0_dp, rho = 0.2_dp
    integer  :: n_p = 16, n_r = 100, n_t = 100, n_l = 2, &
    n_max 
    real(dp), allocatable :: f(:, :), theta_np(:), &
    f_gyr_standard(:, :), x_np(:), y_np(:), trans_r(:), &
    trans_t(:), x_plot(:, :), y_plot(:, :), f_gyr(:, :)
contains

    subroutine init_var
        integer i
        n_max = floor((r_range-rho)/r_range*n_r)- 1 - n_l
        allocate(f(n_r + 1, n_t + 1))
        allocate(x_plot(n_r + 1, n_t + 1))
        allocate(y_plot(n_r + 1, n_t + 1))
        allocate(f_gyr(n_r + 1, n_t + 1))
        allocate(theta_np(n_p + 1))
        allocate(f_gyr_standard(n_r + 1, n_t + 1))
        allocate(trans_r(n_r + 1))
        allocate(trans_t(n_t + 1))
        allocate(x_np(n_p + 1))
        allocate(y_np(n_p + 1))
        f = 0.0_dp
        f_gyr_standard = 0.0_dp
        f_gyr = 0.0_dp
        do i = 0, n_p
            theta_np(i + 1) = i*2.0_dp*pi/n_p
        end do
        x_np = rho*cos(theta_np)
        y_np = rho*sin(theta_np)
        do i = 0, n_r
            trans_r(i + 1) = i*r_range/n_r
        end do
        do i = 0, n_t
            trans_t(i + 1) = i*2.0_dp*pi/n_t
        end do
    end subroutine init_var 

    subroutine init_f 
        real(dp) :: temp_r, temp_t
        integer i, j
        do i = 1, n_r + 1
            do j = 1, n_t + 1
                temp_r=trans_r(i)
                temp_t=trans_t(j)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                f(i, j) = cos(10.0_dp*temp_t)*temp_r**2.0_dp* &
                & (exp(-(temp_r+0.5_dp)**2.0_dp/0.04_dp)+exp(-(temp_r-0.5_dp)**2.0_dp/0.04_dp))
                x_plot(i, j) = temp_r*cos(temp_t)
                y_plot(i, j) = temp_r*sin(temp_t)
            end do
        end do
    end subroutine init_f
end module main_var

module thick_var
    use r64
    use math_const
    use main_var
    implicit none
    integer :: n_r_th = 10000, n_t_th = 10000, n_l_th = 4, n_p_th = 128
    real(dp), allocatable :: f_th(:, :), theta_np_th(:), x_np_th(:), &
    y_np_th(:), trans_r_th(:), trans_t_th(:)
contains
    subroutine init_th_var
        implicit none
        integer i, j
        real(dp) temp_r, temp_t
        allocate(f_th(n_r_th + 1, n_t_th + 1))
        allocate(theta_np_th(n_p_th + 1))
        allocate(x_np_th(n_p_th + 1))
        allocate(y_np_th(n_p_th + 1))
        allocate(trans_r_th(n_r_th + 1))
        allocate(trans_t_th(n_t_th + 1))
        do i = 0, n_p_th
            theta_np_th(i + 1) = i*2.0_dp*pi/n_p_th
        end do
        x_np_th = rho*cos(theta_np_th)
        y_np_th = rho*sin(theta_np_th)
        do i = 0, n_r_th
            trans_r_th(i + 1) = i*r_range/n_r_th
        end do
        do i = 0, n_t_th
            trans_t_th(i + 1) = i*2.0_dp*pi/n_t_th
        end do
        do i = 1, n_r_th + 1
            do j = 1, n_t_th + 1
                temp_r = trans_r_th(i)
                temp_t = trans_t_th(j)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                f_th(i, j) = cos(10.0_dp*temp_t)*temp_r**2.0_dp* &
                & (exp(-(temp_r+0.5_dp)**2.0_dp/0.04_dp)+exp(-(temp_r-0.5_dp)**2.0_dp/0.04_dp))
            end do
        end do
    end subroutine init_th_var
end module thick_var

program main
    use r64
    use main_var
    use math_const
    use thick_var
    implicit none
    call init_var 
    call init_f 
    call self_plot 
    call cal_gyr
    call gyr_plot
    call cal_gyr_standard
    call err_plot
    call f_gyr_standard_plot
    print *, 'Finish!'
end program main

subroutine self_plot
    use r64
    use main_var
    implicit none
    integer io, i
    open(newunit = io, file = 'D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\f.txt')
        do i = 1, n_r + 1
            write(io, *) f(i, :)
        end do
    close(io)
    open(newunit = io, file = 'D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\x_plot.txt')
        do i = 1, n_r + 1
            write(io, *) x_plot(i, :)
        end do
    close(io)
    open(newunit = io, file = 'D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\y_plot.txt')
        do i = 1, n_r + 1
            write(io, *) y_plot(i, :)
        end do
    close(io)
end subroutine self_plot

subroutine gyr_plot
    use r64
    use main_var
    use math_const
    implicit none
    integer io, i
    open(newunit = io, file = 'D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\f_gyr.txt')
    do i = 1, n_r + 1
        write(io, *) f_gyr(i, :)
    end do
    close(io)
end subroutine gyr_plot

subroutine err_plot
    use r64
    use math_const
    use main_var
    implicit none
    real(dp), allocatable :: err(:, :), err_rel(:, :)
    integer io, i, j
    allocate(err(n_r + 1, n_t + 1))
    allocate(err_rel(n_r + 1, n_t + 1))
    err = abs(f_gyr-f_gyr_standard)
    err_rel = abs(err/f_gyr)
    do i = n_max + 1, n_r + 1
        do j = 1, n_t + 1
            err_rel(i, j) = 0
        end do
    end do
    open(newunit = io, file = 'D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\err.txt')
        do i = 1, n_r + 1
            write(io, *) err(i, :)
        end do
    close(io)
    open(newunit = io, file = 'D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\err_rel.txt')
        do i = 1, n_r + 1
            write(io, *) err_rel(i, :)
        end do
    close(io)
end subroutine err_plot

subroutine f_gyr_standard_plot
    use r64
    use math_const
    use main_var
    use thick_var
    implicit none
    integer io, i
    open(newunit = io, file = 'D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\f_gyr_standard.txt')
    do i = 1, n_r + 1
        write(io, *) f_gyr_standard(i, :)
    end do
    close(io)
end subroutine f_gyr_standard_plot

subroutine cal_gyr
    use r64
    use main_var
    use math_const
    implicit none
    integer i, j, i1
    real(dp) cen_r, cen_t, cen_x, cen_y, cir_r, cir_t, f_inter
    real(dp), allocatable :: x_np_rot(:), y_np_rot(:), &
    x_cir(:), y_cir(:)
    allocate(x_np_rot(n_p + 1))
    allocate(y_np_rot(n_p + 1))
    allocate(x_cir(n_p + 1))
    allocate(y_cir(n_p + 1))
    x_np_rot = 0
    y_np_rot = 0
    x_cir    = 0
    y_cir    = 0
    do i = 1, n_max + 1
        do j = 1, n_t + 1
            f_gyr(i, j) = 0
            cen_r = trans_r(i)
            cen_t = trans_t(j)
            cen_x = cen_r*cos(cen_t)
            cen_y = cen_r*sin(cen_t)
            x_np_rot = cos(cen_t)*x_np - sin(cen_t)*y_np
            y_np_rot = sin(cen_t)*x_np + cos(cen_t)*y_np
            x_cir = cen_x + x_np_rot
            y_cir = cen_y + y_np_rot
            do i1 = 1, n_p
                call rec2pol(x_cir(i1), y_cir(i1), cir_r ,cir_t)
                call interpolant(cir_r, cir_t, f_inter)
                f_gyr(i, j) = f_gyr(i, j) + f_inter
            end do
            f_gyr(i, j) = f_gyr(i, j)/n_p
        end do
    end do
end subroutine cal_gyr

subroutine interpolant(in_r, in_t, out_f)
    use r64
    use math_const
    use main_var
    implicit none
    real(dp), intent(in)  :: in_r, in_t
    real(dp), intent(out) ::  out_f
    real(dp) in_i, in_j, weight_i, weight_j
    integer i, j, i0, j0, i1, j1, ii, jj, iii, jjj, iw, jw
    call pol2ij(in_r, in_t, in_i, in_j)
    out_f = 0
    i0 = floor(in_i)-n_l
    j0 = floor(in_j)-n_l
    do i = 0, 2*n_l + 1
        do j = 0, 2*n_l + 1
            ii = i0 + i; jj = j0 + j
            call trans_ij(ii, jj, iii, jjj)
            weight_i = 1
            weight_j = 1
            do i1 = 0, 2*n_l + 1
                if ( i1 == i ) cycle
                iw = i0 + i1
                weight_i = weight_i*(in_i - iw)/(ii - iw)
            end do
            do j1 = 0, 2*n_l + 1
                if ( j1 == j ) cycle
                jw = j0 + j1
                weight_j = weight_j*(in_j - jw)/(jj - jw)
            end do
            out_f = out_f + f(iii, jjj)*weight_i*weight_j
        end do
    end do
end subroutine interpolant

subroutine pol2ij(in_r, in_t, out_i, out_j)
    use r64
    use math_const
    use main_var
    implicit none
    real(dp), intent(in)  :: in_r, in_t
    real(dp), intent(out) :: out_i, out_j
    out_i = in_r/r_range*n_r     + 1.0_dp;  
    out_j = in_t/(2.0_dp*pi)*n_t + 1.0_dp;
end subroutine pol2ij

subroutine rec2pol(in_x, in_y, out_r, out_t)
    use r64
    use math_const
    implicit none
    real(dp), intent(in)  :: in_x , in_y
    real(dp), intent(out) :: out_r, out_t
    out_r = sqrt(in_x*in_x + in_y*in_y) 
    if ( in_x == 0 ) then
        if ( in_y > 0 ) then
            out_t=pi*0.5_dp
            return
        else
            out_t=pi*1.5_dp;
            return
        end if
    end if
    if ( in_y == 0 ) then
        if ( in_x > 0 ) then
            out_t = 0.0_dp
            return
        else
            out_t = pi
            return
        end if
    end if
    if ( in_x > 0 .and. in_y > 0 ) then
        out_t = atan(in_y/in_x)
        return
    end if
    if ( in_x > 0 .and. in_y < 0 ) then
        out_t = 2.0_dp*pi + atan(in_y/in_x)
        return
    end if
    if ( in_x < 0 ) then
        out_t = pi + atan(in_y/in_x)
        return
    end if
end subroutine rec2pol

subroutine trans_ij(in_i, in_j, out_i, out_j)
    use r64
    use main_var
    use math_const
    integer, intent(in)  :: in_i, in_j
    integer, intent(out) :: out_i, out_j
    out_i = in_i
    out_j = in_j
    if ( in_i <= 0 ) then
        out_i = 2 - in_i
        out_j = in_j + n_t/2
    end if
    out_j = out_j - 1
    out_j = modulo(out_j, n_t)
    out_j = out_j + 1
end subroutine trans_ij

subroutine cal_gyr_standard
    use r64
    use math_const
    use main_var
    use thick_var
    implicit none
    integer i, j, i1
    real(dp) cen_r, cen_t, cen_x, cen_y, cir_r, cir_t, f_th_inter
    real(dp), allocatable :: x_np_th_rot(:), y_np_th_rot(:), &
    x_cir(:), y_cir(:)
    allocate(x_np_th_rot(n_p_th + 1))
    allocate(y_np_th_rot(n_p_th + 1))
    allocate(x_cir(n_p_th + 1))
    allocate(y_cir(n_p_th + 1))
    call init_th_var
    f_gyr_standard = 0.0_dp
    do i = 1, n_max + 1
        do j = 1, n_t + 1
            f_gyr_standard(i, j) = 0.0_dp
            cen_r = trans_r(i)
            cen_t = trans_t(j)
            cen_x = cen_r*cos(cen_t)
            cen_y = cen_r*sin(cen_t)
            x_np_th_rot = cos(cen_t)*x_np_th - sin(cen_t)*y_np_th
            y_np_th_rot = sin(cen_t)*x_np_th + cos(cen_t)*y_np_th
            x_cir = cen_x + x_np_th_rot
            y_cir = cen_y + y_np_th_rot
            do i1 = 1, n_p_th
                call rec2pol(x_cir(i1), y_cir(i1), cir_r, cir_t)
                call interpolant_th(cir_r, cir_t, f_th_inter)
                f_gyr_standard(i, j) = f_gyr_standard(i, j) + f_th_inter
            end do
            f_gyr_standard(i, j) = f_gyr_standard(i, j)/n_p_th
        end do
    end do
end subroutine cal_gyr_standard

subroutine interpolant_th(in_r, in_t, out_f)
    use r64
    use thick_var
    use math_const
    implicit none
    real(dp), intent(in)  :: in_r, in_t
    real(dp), intent(out) :: out_f
    real(dp) in_i, in_j, weight_i, weight_j
    integer i, j, i0, j0, ii, jj, iii, jjj, i1, j1, iw, jw
    call pol2ij_th(in_r, in_t, in_i, in_j)
    out_f = 0
    i0 = floor(in_i) - n_l_th
    j0 = floor(in_j) - n_l_th
    do i = 0, 2*n_l_th + 1
        do j = 0, 2*n_l_th + 1
            ii = i0 + i
            jj = j0 + j
            call trans_ij_th(ii, jj, iii, jjj)
            weight_i = 1.0_dp
            weight_j = 1.0_dp
            do i1 = 0, 2*n_l_th + 1
                if ( i1 == i ) cycle
                iw = i0 + i1
                weight_i = weight_i*(in_i - iw)/(ii - iw)
            end do
            do j1 = 0, 2*n_l_th + 1
                if ( j1 == j ) cycle
                jw = j0 + j1
                weight_j = weight_j*(in_j - jw)/(jj - jw)
            end do
            out_f = out_f + f_th(iii, jjj)*weight_i*weight_j
        end do
    end do
end subroutine interpolant_th

subroutine pol2ij_th(in_r, in_t, out_i, out_j)
    use r64
    use main_var
    use thick_var
    use math_const
    implicit none
    real(dp), intent(in)  :: in_r, in_t
    real(dp), intent(out) :: out_i, out_j
    out_i = in_r/r_range*n_r_th     + 1.0_dp
    out_j = in_t/(2.0_dp*pi)*n_t_th + 1.0_dp;
end subroutine pol2ij_th

subroutine trans_ij_th(in_i, in_j, out_i, out_j)
    use r64
    use math_const
    use thick_var
    implicit none
    integer, intent(in)  :: in_i, in_j
    integer, intent(out) :: out_i, out_j
    out_i = in_i
    out_j = in_j
    if ( in_i <= 0 ) then
        out_i = 2 - in_i
        out_j = in_j + n_t_th/2;
    end if
    out_j = out_j - 1
    out_j = modulo(out_j, n_t_th)
    out_j = out_j + 1
end subroutine trans_ij_th
