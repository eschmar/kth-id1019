#include "erl_nif.h"
#include <complex.h>

extern int depth(double cr, double ci, int m) {
    int i = 0;
    double complex c = cr + ci * I;
    double complex z = 0.0 + 0.0 * I;
    double abs = cabs(z);

    while (m != i && 2 >= abs) {
        i++;
        z = cpow(z, 2) + c;
        abs = cabs(z);
    }

    if (m == i) {
        return 0;
    }

    return i;
}

static ERL_NIF_TERM depth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int m;
    double cr, ci;

    if (!enif_get_double(env, argv[0], &cr)) {
        return enif_make_string(env, "3", ERL_NIF_LATIN1);
        return enif_make_badarg(env);
    }

    if (!enif_get_double(env, argv[1], &ci)) {
        return enif_make_string(env, "4", ERL_NIF_LATIN1);
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &m)) {
        return enif_make_badarg(env);
    }

    int result = depth(cr, ci, m);
    return enif_make_int(env, result);
}

static ErlNifFunc nif_funcs[] = {
    {"depth", 3, depth_nif}
};

ERL_NIF_INIT(complex_nif, nif_funcs, NULL, NULL, NULL, NULL)