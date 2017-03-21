# kth-id1019

<a target="_blank" href="https://github.com/eschmar/kth-id1019/raw/master/mandelbrot/img/fjord.jpg">
    <img src="https://github.com/eschmar/kth-id1019/raw/master/mandelbrot/img/fjord.jpg" alt="Mandelbrot" style="max-width:100%;">
</a>


## mandelbrot c compilation

```sh
gcc -std=c99 -fPIC -shared -o complex_nif.so complex_nif.c -I /usr/local/lib/erlang/erts-8.2/include/ -flat_namespace -undefined 
suppress
```

* Add the absolute path to the erlang `erl_nif.h`
* Add `-flat_namespace -undefined suppress` on macOS
