-- run the following in ghci..

import Sound.OSC.FD as O

-- gets a randomly allocated port, might be useful..
-- udp <- udpServer "127.0.0.1" 0
-- udpPort udp

udp <- udpServer "127.0.0.1" 6012

r <- openUDP  "127.0.0.1" 6011

sendMessage r $ Message "/code" [string "hello", string "sound \"bd sn\""]
m <- recvMessage udp
m

-- error..
sendMessage r $ Message "/code" [string "hello", string "sund \"bd sn\""]
m <- recvMessage udp
m

sendMessage r $ Message "/ping" []
m <- recvMessage udp
m

     
-- receive cps values 
sendMessage r $ Message "/cps" []
m <- recvMessage udp
m
