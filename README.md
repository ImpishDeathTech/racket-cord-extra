# racket-cord/extra

A modified version of the racket wrapper for the discord API for people who are lazy.

[Official Git](https://github.com/simmsb/racket-cord)

[Official Docs](https://docs.racket-lang.org/racket-cord/index.html)

# Design Notes
[Official Notes](https://github.com/simmsb/racket-cord#design-notes)

This library modification is focused on only a couple things and doing those things properly:
1. Providing some ease of use facilities for interracting with the api
2. Keeping the api definition at an available version for now, till they update it

There is nearly no caching of state in the origian client, so I decided to write some. 
With that, came along some other modifications, I guess.

The rationale can be found in this [commit message](https://github.com/simmsb/racket-cord/commit/64b8f1de97fccb01487571362e2b4bac749c3691)
And in the official git. So I decided to write some ease of use crap, mostly for myself.

# Sample Projects

* [Official Samples](https://github.com/simmsb/racket-cord#sample-projects)
