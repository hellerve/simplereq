# simplereq

A zepto native extension that wraps the Haskell `wreq` package
to make simple http and https requests. It is not even nearly done,
but `get` and `delete` work.

## Installation

```
zeps install hellerve/simplereq
```

## Usage

```clojure
(load "SimpleReq")
(simplereq:request "a.4cdn.org/mu/1.json")
; will return the JSON representation of the first page of 4chan's mu board
```

<hr/>

Have fun!
