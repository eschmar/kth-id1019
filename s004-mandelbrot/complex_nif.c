#include "erl_nif.h"
#include <complex.h>

extern int depth(int i, double zr, double zi, double cr, double ci, int m);

static ERL_NIF_TERM depth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i, m;
    double zr, zi, cr, ci;

    if (!enif_get_int(env, argv[0], &i) || !enif_get_int(env, argv[5], &m)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_double(env, argv[1], &zr)) {
        return enif_make_string(env, "1", ERL_NIF_LATIN1);
        return enif_make_badarg(env);
    }
    if (!enif_get_double(env, argv[2], &zi)) {
        return enif_make_string(env, "2", ERL_NIF_LATIN1);
        return enif_make_badarg(env);
    }
    if (!enif_get_double(env, argv[3], &cr)) {
        return enif_make_string(env, "3", ERL_NIF_LATIN1);
        return enif_make_badarg(env);
    }
    if (!enif_get_double(env, argv[4], &ci)) {
        return enif_make_string(env, "4", ERL_NIF_LATIN1);
        return enif_make_badarg(env);
    }

    double complex c = cr + ci * I;
    double complex z = zr + zi * I;
    double abs = cabs(z);

    while (m != i && 2 >= abs) {
        i++;
        z = cpow(z, 2) + c;
        abs = cabs(z);
    }

    if (m == i) {
        return enif_make_int(env, 0);
    }

    return enif_make_int(env, i);
}

static ErlNifFunc nif_funcs[] = {
    {"depth", 6, depth_nif}
};

ERL_NIF_INIT(complex_nif, nif_funcs, NULL, NULL, NULL, NULL)