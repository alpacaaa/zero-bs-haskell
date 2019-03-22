### Zero BS

Simple webserver library.

```haskell
import Zero.Server

helloHandler :: Handler
helloHandler
  = simpleHandler GET "/hello" (\req -> stringResponse "hello")

pingHandler :: Handler
pingHandler
  = simpleHandler GET "/ping" (\req -> stringResponse "pong")

main :: IO ()
main
  = startServer [ helloHandler, pingHandler ]
```


`curl localhost:7879/hello`  
hello


`curl localhost:7879/ping`  
pong
