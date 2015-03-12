This unikernel currently only compiles in Unix mode because of its use of oUnit.  However, it uses the Mirage TCP/IP stack without requiring tap configuration.

Currently, the tests will fail, as they correctly detect deficiencies in the current ARP implementation (as of tcpip 2.3.0).  This unikernel is meant for use in developing an adequate patch for these deficiencies.

This unikernel requires the `mirage-net-pcap` library.  To get it, 

```
opam pin add mirage-net-pcap https://github.com/yomimono/mirage-net-pcap
```
