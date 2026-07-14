# gay://chat operationalization over Brassica Chat

This document turns Brassica Chat into a `gay://chat` shared-protention substrate.
It assumes the current Brassica architecture:

- local-first peer-to-peer room presences as a Goblins unum
- OCapN sturdyrefs and object capabilities for authority-bearing connections
- operation-based Byzantine-tolerant CRDT logs with HLC causal order
- time-partitioned chat logs
- zcap-style certificate capabilities that govern well-behaved interpretation
- append-only event history whose visible view is reinterpreted as certificates change

The goal is not merely chat. The goal is a capability-secure collective cognition room:

```text
retention -> impression -> protention -> feedback -> obstruction/experiment -> decision -> retention
```

## 1. URI discipline

`gay://chat` URIs identify cognitive objects and local petname bindings. They MUST NOT casually expose bearer sturdyrefs in human-shareable public text.

Recommended URI forms:

```text
gay://chat/room/<local-petname>
gay://chat/room/<root-signer-fingerprint>/<room-color>
gay://chat/event/<event-hash>
gay://chat/protention/<event-hash>
gay://chat/obstruction/<event-hash>
gay://chat/experiment/<event-hash>
gay://chat/decision/<event-hash>
gay://chat/invite/<local-petname-or-attenuated-cap-id>
```

A local resolver maps these names to actual OCapN sturdyrefs or local room actors.
The resolver is a petname table, not a global DNS replacement.

## 2. Brassica primitive mapping

| `gay://chat` concept | Brassica primitive |
|---|---|
| Shared memory / retention | partitioned append-only CRDT chat logs |
| Primal impression | `post` event with kind `observation` |
| Protention | structured `post` event with kind `protention` |
| Bidirectional feedback | `feedback` events referencing prior event IDs |
| Obstruction | explicit conflict/gluing-failure event |
| Experiment | event with hypothesis, protocol, horizon, result slot |
| Decision / ADR | event that closes protentions/obstructions and changes policy |
| Colored port | zcap predicate + event labels + room partitions |
| Capability wire | OCapN object capability / sturdyref |
| Local identity | ed25519 key + self-proposed name + petname binding |
| Consensus topos | derived view produced by replaying CRDT + certificates + event schema |

## 3. Event content schema

Brassica already allows `contents` to be arbitrary Scheme/Syrup data. `gay://chat` can start without changing the CRDT by placing structured forms in message contents.

Canonical form:

```scheme
(gay-event
  (version 1)
  (kind protention)
  (color (domain strategy) (phase protention) (role generator) (sensitivity internal))
  (refs ((retention <event-id>) (observation <event-id>) (obstruction <event-id>)))
  (body ((claim "Developer-led positioning will outperform compliance copy")
         (confidence 0.66)
         (horizon "30d")
         (falsifier "Compliance page wins qualified pipeline by >20%")))
  (feedback ((requested (contradiction evidence experiment-design))
             (due "2026-08-13"))))
```

Recommended kinds:

```text
observation
protention
feedback
obstruction
experiment
result
decision
retrospective
petname
port
grant
revoke
score
```

## 4. Operational invariants

### 4.1 No feedforward without feedback

Every `protention`, `experiment`, `decision`, and `grant` must name a reverse update path.

```text
proposal -> critique
prediction -> score
experiment -> result
decision -> retrospective
capability grant -> revocation path
```

### 4.2 No global broadcast by default

Events are routed by color. A client or bot subscribes to the colors it can interpret:

```text
(domain strategy phase protention) -> strategy synthesizer
(domain security role critic) -> security critic
(kind obstruction glue obstructed) -> coordinator
(kind result) -> telemetry scorer
```

### 4.3 Every disagreement becomes an obstruction

If two local models overlap and fail to glue, emit an `obstruction` event rather than burying disagreement in prose.

### 4.4 Every protention is judged

A protention eventually transitions to one of:

```text
fulfilled
frustrated
split
expired
abandoned
```

### 4.5 Authority and visibility are distinct

Brassica's README is explicit: soft blocks and zcaps govern the eventual user-visible view; object-capability revocation governs actual read/write reachability. UI must show the difference.

```text
soft-blocked != removed
hidden != unable-to-write
revoked-at-all-edges == removed from capability graph
```

## 5. Certificate/color extension

Current Brassica certificate predicates can express coarse permissions such as `allow-self` and operation filters. `gay://chat` needs payload-aware predicates.

Target predicate call shape:

```scheme
(predicate op author who payload context)
```

Where `payload` is the structured `gay-event` content and `context` can include room, partition, timestamp, and current policy view.

Useful predicate combinators:

```scheme
(when-op (post edit delete react) expr)
(when-kind (protention obstruction experiment decision) expr)
(when-domain (strategy security engineering market) expr)
(when-phase (retention impression protention action reafference) expr)
(when-sensitivity (public internal restricted) expr)
(allow-self)
(allow-role critic)
(allow-controller <public-key>)
(and expr ...)
(or expr ...)
(not expr)
```

This makes a zcap a colored port: it is simultaneously authorization, routing key, and semantic interface.

## 6. Minimal MVP without core CRDT changes

1. Encode structured `gay-event` lists as message contents.
2. Add UI renderers for event kinds and color tags.
3. Add a local petname resolver for room/user display.
4. Add bot presences as normal Brassica peers:
   - critic bot
   - synthesizer bot
   - telemetry scorer bot
   - obstruction coordinator bot
5. Add a replay/export process that derives:
   - `worldview/current.md`
   - `worldview/consensus-topos.json`
   - prediction scores
   - open obstruction list
6. Keep zcaps coarse initially, then add payload-aware predicates.

## 7. Bot protocol

Bots are not ambient readers. They are ordinary capability holders with attenuated room access.

Example bot responsibilities:

```text
critic bot:
  reads: protention, decision
  writes: feedback, obstruction

synthesis bot:
  reads: feedback, obstruction, result
  writes: decision draft, worldview update

telemetry bot:
  reads: protention, experiment, result
  writes: score, retrospective prompt

security bot:
  reads: grant, revoke, sensitivity restricted, obstruction security
  writes: feedback, soft-block recommendation, revocation checklist
```

## 8. Consensus topos derivation

A `gay://chat` client derives the current shared universe by replaying:

```text
CRDT events
+ certificate state
+ petname table
+ event schema validation
+ visibility interpretation
+ feedback/protention score updates
```

The derived view separates:

```text
strong beliefs
active protentions
open obstructions
model splits
experiments awaiting result
decisions retained
revocation/capability hazards
```

## 9. Brassica-specific development path

### Phase A: overlay protocol

No invasive CRDT changes. Add structured content helpers, UI renderers, and export.

### Phase B: petnames and UX truthfulness

Add petname display and explicit UI distinction between:

```text
self-proposed name
local petname
verified key fingerprint
soft-blocked state
actual object-cap revoked state
```

### Phase C: payload-aware zcaps

Extend certificate predicates so colors become enforceable interpretation capabilities.

### Phase D: persistence

Persist identity, room metadata, petnames, cert state, and CRDT partitions locally. Browser target likely uses IndexedDB; Guile target can use Goblins persistence or a local store.

### Phase E: content-addressed message bodies

Move message content out of the append-only CRDT into content-addressed mutable stores so illegal/sensitive content can be locally purged while retaining auditable references.

## 10. The final system law

`gay://chat` is not a message app. It is a capability-secure shared-protention machine:

```text
OCapN carries authority.
CRDT carries retention.
HLC carries temporal order.
Zcaps carry colored interpretation.
Petnames carry local identity.
Feedback carries learning.
Obstructions carry disagreement.
Consensus views carry the shared universe.
```
