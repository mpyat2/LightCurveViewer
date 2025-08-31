echo Initializing environment...
. /opt/intel/oneapi/setvars.sh

echo Building liblapack_min.so
gcc -fPIC -shared lapack_min.c -o liblapack_min.so \
    -I${MKLROOT}/include \
    -Wl,--start-group \
        ${MKLROOT}/lib/intel64/libmkl_intel_lp64.a \
        ${MKLROOT}/lib/intel64/libmkl_sequential.a \
        ${MKLROOT}/lib/intel64/libmkl_core.a \
    -Wl,--end-group \
    -lpthread -lm -ldl
