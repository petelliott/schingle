# schingle manual

## concepts

### paths

all requests are dispatched by HTTP method and path. handlers can be installed
for specific http methods, and for path patterns.

paths can use `:` identifiers: `/path/:var` matches `/path/a` and `/path/b`.

paths can use splats/wildcards (`*`): `/path/a*b` matches `/path/ab`,
`/path/acb`, and `/path/ac/cb`.

full regex matching is a future goal.

### handlers

as with guile's builtin web server, handlers are functions of the form
`(handler request body)`. schingle handlers can also take optional and keyword
arguments which correspond to the `*` and `:identifiers` respectively.

## (schingle schingle)

### routing functions

schingle exports the syntax:

```scheme
(GET|HEAD|POST|PUT|DELETE|TRACE /path/as/symbol handlerfn)
```

this will return a route-handler pair.

example:

```scheme
(GET /hello/:name
     (lambda* (request body #:key :name)
       (plain (format #f "Hello, ~a!" :name))))
```

occasionally you might want to generate routes, which is not possible with the
syntactic router. schingle also provides equivalent functions that takes path as a string argument.

```scheme
(GETs|HEADs|POSTs|PUTs|DELETEs|TRACEs "/path/as/string" handlerfn)
```

### (make-handler routes)

returns a guile web server compatible handler from a list of route-handler pairs

### (run-schingle routes #:optional (impl 'http) (open-params '()))

equivalent of `(run-server (make-handler routes) impl open-params)`

### (define-handlers name body* ...)

syntax that expands to `(define name (list body* ...))`
