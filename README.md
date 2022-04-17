# SHA-256
Secure Hash Algorithm 256 or SHA-256 is a cryptographic algorithm that can take arbitray data, which can have an arbitrary length, and puts it into a function that produces a 256 bit long value that is represented with a hexidecimal string.  It is algorithm that is used in Bitcoin mining and hash functions like SHA-256 are used everywhere in the cryptocurrency world.  Hash functions allow arbitrary data to be represented, without showing what data is actually contained.  

Here is how SHA-256 works:
1. Convert data to binary 
2. Padding message
3. Create message schedule with each block 
4. Compression on each word in each block
5. Convert final values after compression to hex 

Running code 
1. install cabal 
2. run "git clone https://github.com/ForgiveWifi/SHA-256.git"
3. run "cd SHA-256"
4. run "cd code" 
5. run "cabal run"



Resources
https://blog.boot.dev/cryptography/how-sha-2-works-step-by-step-sha-256/
https://www.youtube.com/watch?v=f9EbD6iY9zI