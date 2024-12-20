To login in the TestAuthorise use _[your]_ username and password from Instagram and create `testinstagram.ini` file and copy the `ini` text below with your data:
``` INI
[Target]
;; It can specify other account Username or media post shortcode for testing
Username=natgeo
;; that is account url https://www.instagram.com/natgeo/
Media=BqRpCX2gfsq
;; that is media url  https://www.instagram.com/p/BqRpCX2gfsq/

[Session]
Username=YOUR_USERNAME
Password=YOUR_PASSWORD

;; HTTP proxies are currently supported only with synapse components
[Proxy]
Host=
Port=
Username=
Password=
;; You can instead above values to specify only one string value like this: host:password (or Username:Password@Host:Port) in Uri variable
Uri=
```
