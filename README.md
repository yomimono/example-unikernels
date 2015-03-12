This unikernel currently only compiles in Unix mode because of its use of oUnit.  However, it uses the Mirage TCP/IP stack without requiring tap configuration.

This unikernel requires the `mirage-net-pcap` library.  To get it, 

```
opam pin add mirage-net-pcap https://github.com/yomimono/mirage-net-pcap
```
