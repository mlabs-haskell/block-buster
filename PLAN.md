# The Plan

- [ ] Chain follower: Add callback and implement poll functionality (with [unagi-chan bounded?](https://hackage.haskell.org/package/unagi-chan-0.4.1.4/docs/Control-Concurrent-Chan-Unagi-Bounded.html))
- [ ] Make template for producer of domain events; something like ```producer:: PollChainSyncClient -> (ChainEvent -> [DomainEvent]) -> IO Producer```
- [ ] Ability to restart chain sync client on connection failure