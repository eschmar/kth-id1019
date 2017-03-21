-module(complex).
-compile(export_all).

%
%   complex
%

new(R, Y) -> {R, Y}.

add({AR, AI}, {BR, BI}) -> {AR+BR, AI+BI}.

sqr({R, I}) -> {(R * R) - (I * I), 2 * R * I}.

abs({R, I}) -> math:sqrt(R * R + I * I).
