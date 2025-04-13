# Rhythm of Life

To run development:

Make sure you have [elm](https://guide.elm-lang.org/install/elm.html) installed.

Install client dependencies:

    pnpm i

Run the client:

    pnpm dev

Install server dependencies:

    opam install dream yojson lwt_ppx ppx_yojson_conv

Run the server:

    dune exec server
