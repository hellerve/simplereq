# simplereq

A zepto native extension that wraps the Haskell `wreq` package
to make simple http and https requests. It is in alpha.

## Installation

```
zeps install hellerve/simplereq
```

## Usage

simplereq exposes the REST verbs and a generic verb function.
You can see it in action below:

```clojure
(load "simplereq/SimpleReq")
(simplereq:get "https://httpbin.org/get")
(simplereq:post "https://httpbin.org/post" "hi httpbin")
(simplereq:put "https://httpbin.org/put" "hi httpbin")
(simplereq:delete "https://httpbin.org/delete")
(simplereq:request "patch" "https://httpbin.org/patch" "hi httpbin")
```

All of these methods return hashmaps with the keys `:status`, `:headers` and
`:body`.

Sadly, you can not yet mess with the request headers or cookies.

<hr/>

Have fun!
