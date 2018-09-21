This file contains some useful information for generating and working with RSA keys.

Generate a 1024 bit RSA private key.
```
openssl genrsa -out private.pem 1024
```

Generate an RSA public key from a private key.
```
openssl rsa -in private.pem -outform PEM -pubout -out public.pem
```

Show the key size of an RSA public key.
```
openssl rsa -inform PEM -pubin -in public.pem -text -noout
```
