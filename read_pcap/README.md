## Dependencies

This unikernel depends on [mirage-net-pcap](https://github.com/yomimono/mirage-net-pcap).  To install it:

```
opam pin add mirage-net-pcap https://github.com/yomimono/mirage-net-pcap.git
```

## Building

Build with:

```
mirage configure --unix
make
```

## Running

Have a [pcap](http://www.tcpdump.org) file handy.  `mir-read_pcap` takes an optional argument of which pcap file to read; if none is specified, it will look for a file called "packets.pcap" in your current working directory.

```
./mir-read_pcap -f packets.pcap
```
