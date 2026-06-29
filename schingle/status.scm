(define-module (schingle status)
  #:use-module (schingle content-type)
  #:export (bad-request *bad-request*
            unauthorized *unauthorized*
            forbidden *forbidden*
            not-found *not-found*
            method-not-allowed *method-not-allowed*
            not-acceptable *not-acceptable*
            request-timeout *request-timeout*
            conflict *conflict*
            gone *gone*
            im-a-teapot *im-a-teapot*
            unprocessable-entity *unprocessable-entity*
            too-many-requests *too-many-requests*
            internal-server-error *internal-server-error*
            not-implemented *not-implemented*
            bad-gateway *bad-gateway*
            service-unavailable *service-unavailable*
            gateway-timeout *gateway-timeout*))

(define-syntax status-generator
  (syntax-rules ()
    ((_ name parameter default-msg code)
     (begin
       (define parameter (make-parameter (lambda (msg)
                                           (plain (or msg default-msg) #:code code))))
       (define* (name #:optional (msg #f))
         ((parameter) msg))))))

(status-generator bad-request *bad-request* "400 Bad Request" 400)
(status-generator unauthorized *unauthorized* "401 Unauthorized" 401)
(status-generator forbidden *forbidden* "403 Forbidden" 403)
(status-generator not-found *not-found* "404 Not Found" 404)
(status-generator method-not-allowed *method-not-allowed* "405 Method Not Allowed" 405)
(status-generator not-acceptable *not-acceptable* "406 Not Acceptable" 406)
(status-generator request-timeout *request-timeout* "408 Request Timeout" 408)
(status-generator conflict *conflict* "409 Conflict" 409)
(status-generator gone *gone* "410 Gone" 410)
(status-generator im-a-teapot *im-a-teapot* "418 I'm a teapot" 418)
(status-generator unprocessable-entity *unprocessable-entity* "422 Unprocessable Entity" 422)
(status-generator too-many-requests *too-many-requests* "429 Too Many Requests" 429)
(status-generator internal-server-error *internal-server-error* "500 Internal Server Error" 500)
(status-generator not-implemented *not-implemented* "501 Not Implemented" 501)
(status-generator bad-gateway *bad-gateway* "502 Bad Gateway" 502)
(status-generator service-unavailable *service-unavailable* "503 Service Unavailable" 503)
(status-generator gateway-timeout *gateway-timeout* "504 Gateway Timeout" 504)
