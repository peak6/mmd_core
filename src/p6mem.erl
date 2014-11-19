-module(p6mem).
-include("mem.hrl").

-compile([export_all]).

convert( N, bytes, bytes) -> N;
convert( N, bytes, words) -> N div ?WORD;
convert( N, bytes,    kb) -> N div ?KB;
convert( N, bytes,    mb) -> N div ?MB;
convert( N, bytes,    hb) -> N div ?GB;

convert( N, words, bytes) -> N * ?WORD;
convert( N, words, words) -> N;
convert( N, words,    kb) -> (N * ?WORD) div ?KB;
convert( N, words,    mb) -> (N * ?WORD) div ?MB;
convert( N, words,    gb) -> (N * ?WORD) div ?GB;

convert( N,    kb, bytes) -> N * ?KB;
convert( N,    kb, words) -> (N * ?KB) div ?WORD;
convert( N,    kb,    kb) -> N;
convert( N,    kb,    mb) -> N div ?KB_MB;
convert( N,    kb,    gb) -> N div ?KB_GB;

convert( N,    mb, bytes) -> N * ?MB;
convert( N,    mb, words) -> (N*?MB) div ?WORD;
convert( N,    mb,    kb) -> N * ?KB_MB;
convert( N,    mb,    mb) -> N;
convert( N,    mb,    gb) -> N div ?GB;

convert( N,    gb, bytes) -> N * ?GB;
convert( N,    gb, words) -> (N*?GB) div ?WORD;
convert( N,    gb,    kb) -> N * ?KB_GB;
convert( N,    gb,    mb) -> N * ?MB_GB;
convert( N,    gb,    gb) -> N.

