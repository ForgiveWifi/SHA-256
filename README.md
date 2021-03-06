# SHA-256
This is a program written in Haskell that takes a string or a file and hashes it using SHA-256.  It is not optimized for large strings and files.  Instead, binary is represented as a [Bool] and all the conversions between hex, Int, and bytes are all functions in the src folder.

<a href="https://github.com/ForgiveWifi/SHA-256/blob/main/SHA256english.pdf">Secure Hash Algorithm 256<a> or SHA-256 is a cryptographic algorithm that can take arbitray data, which can have an arbitrary length, and puts it into a function that produces a 256 bit long value that is represented with a hexidecimal string.  It is algorithm that is used in Bitcoin mining and hash functions like SHA-256 are used everywhere in the cryptocurrency world.  Hash functions allow arbitrary data to be represented, without showing what data is actually contained.  

Here is how SHA-256 works:
1. Convert data to binary 
2. Padding message
3. Create message schedule with each block in message 
4. Compression on each word in each message block 
5. Convert final values after compression to hex

<img width="1226" alt="Screen Shot 2022-04-16 at 7 00 33 PM" src="https://user-images.githubusercontent.com/85458169/163735040-e3243fb4-9203-4041-8edd-4337cc3866b4.png">

# Setup 

Make sure cabal is installed 
```
$ git clone https://github.com/ForgiveWifi/SHA-256.git
$ cd SHA-256
$ cd code
$ cabal run
```

# Test 
```
$ 1
$ hello world
> b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9
$ Y 
$ 2
$ test.txt
> df22d9b0313a81cfbe8d38c2d0059ffa5a391d3b508439b656aafe50c533d4a0
```

# Resources

https://blog.boot.dev/cryptography/how-sha-2-works-step-by-step-sha-256/

https://www.youtube.com/watch?v=f9EbD6iY9zI
