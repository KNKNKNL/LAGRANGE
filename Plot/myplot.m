%% self_plot
clear;clc;
f       = load('D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\f.txt');
x       = load('D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\x_plot.txt');
y       = load('D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\y_plot.txt');
f_gyr   = load('D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\f_gyr.txt');
err     = load('D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\err.txt');
err_rel = load('D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\err_rel.txt');
f_gyr_standard=load('D:\WHC_File\Study\research\GYR\Fortran\LAGRANGE\data\f_gyr_standard.txt');
figure(1)
set(figure(1),'position',[300,100,500,408], 'name', 'f');
pcolor(x, y, f);
shading interp;
colorbar
figure(2)
set(figure(2),'position',[400,100,500,408], 'name', 'f_gyr');
pcolor(x, y, f_gyr);
shading interp;
colorbar
figure(3)
set(figure(3),'position',[500,100,500,408], 'name', 'err_abs');
pcolor(x, y, err);
shading interp;
colorbar
figure(4)
set(figure(4),'position',[600,100,500,408], 'name', 'err_rel');
pcolor(x, y, err_rel);
shading interp;
colorbar
figure(5)
set(figure(5),'position',[700,100,500,408], 'name', 'f_gyr_standard');
pcolor(x, y, f_gyr_standard);
shading interp;
colorbar
%%
