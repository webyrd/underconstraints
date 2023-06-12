Profile of 'test-restricted-interp-underconstraints-base-case-last.scm' with 1_000_000 ticks

Open 'profile.html' to see the results.

Chez profiler description:

https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:s130

To generate these files, I ran this command in the `faster-miniKanren` directory ** with the tests that don't use underconstraints commented out **:

```
Chez Scheme Version 9.5.8
Copyright 1984-2022 Cisco Systems, Inc.

> (parameterize ([compile-profile 'source]) (load "test-restricted-interp-underconstraints-base-case-last.scm"))
Testing "synthesize rember with one top-level general underconstraint base-case last"
*engine-completed-counter*: 152401
*engine-timedout-counter*: 13899
*immature-stream-counter*: 42008172
*fail-counter*: 10647
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 141754
(time (test "synthesize rember with one top-level general underconstraint base-case last" ...))
    15282 collections
    154.379322736s elapsed cpu time, including 10.086942018s collecting
    154.540912000s elapsed real time, including 10.132774000s collecting
    128226994400 bytes allocated, including 128061872832 bytes reclaimed
> (profile-dump-html) 
>
```