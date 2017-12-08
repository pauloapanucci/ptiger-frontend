~/Projects/ptiger/gcc-install/bin/g++ -fPIC -shared ptigerlib.c -o libptiger.so
export DYLD_LIBRARY_PATH=../../ && export LD_LIBRARY_PATH=../../
gccptiger -L../../ -o z1.out z1.ptiger -lptiger && ./z1.out

export DYLD_LIBRARY_PATH=../../ && export LD_LIBRARY_PATH=../../
gccptiger -L../../ -o z.out z.ptiger -lptiger && ./z.out

gccptiger -L../../ -o mm.out mm.ptiger -lptiger && ./mm.out


gccptiger -L../../ -o cholesky.out cholesky.ptiger -lptiger && ./cholesky.out
