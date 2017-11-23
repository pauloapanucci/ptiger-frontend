~/Projects/ptiger-frontend/gcc-install/bin/g++ -fPIC -shared ptigerlib.c -o libptiger.so
export DYLD_LIBRARY_PATH=../../ && export LD_LIBRARY_PATH=../../
gccptiger -L../../ -o z1.out z1.ptiger -lptiger && ./z1.out
