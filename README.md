# Brassica Chat

![](https://files.spritely.institute/images/blog/brassica-chat-screenshot.png)

This is a prototype for a secure, local-first, peer-to-peer chat
application built with the
[Goblins](https://spritely.institute/goblins) distributed programming
framework.

This experiment was motivated by the rapidly changing legislative
environment for hosted digital services.  In contrast to centralized
or federated chat systems, *no one hosts* a Brassica Chat room as
there are no servers.  Another motivation was a search for simplicity;
to achieve secure local-first chat with fewer moving parts.  The
[capability
security](https://habitatchronicles.com/2017/05/what-are-capabilities/)
paradigm does the majority of the work to keep interactions safe
rather than complex cryptographic algorithms such as double-ratchet.

## Try

Get the dependencies by running `guix shell -m manifest.scm`.

Run `make world` to run the simulated chat world in your terminal.

To try the web interface, run `make server`.  Copy a sturdyref for
Alice, Bob, or Carol and visit http://localhost:8088.

Note that these worlds *do not* currently save their state.  This is
just a prototype and all chat rooms are ephemeral.

## Technical overview

![](https://files.spritely.institute/images/blog/brassica-chat-unum.png)

Chat rooms are implemented using operation-based
[CRDTs](https://crdt.tech) with causal broadcast messaging.  Each peer
participating in a chat room maintains a copy of the chat log, which
forms a directed acyclic graph similar to a Git repository.  Together,
these peers combine to form a single abstract chat room actor.  In
object capability jargon, chat rooms can be thought of as an
[unum](https://habitatchronicles.com/2019/08/the-unum-pattern/) where
each peer maintains a presence and messages are propagated via
broadcast.

![](https://files.spritely.institute/images/blog/brassica-chat-crdt-diagram.png)

Rather than using one giant CRDT for the entirety of a chat room's
history (which would create scaling issues for rooms with a lot of
history), the chat log is *partitioned* by time.  Each partition
covers some `period` number of seconds of real time and is represented
as its own distinct CRDT.  All peers must use the same `period` value
in order to converge properly.  This partitioning strategy allows each
peer to perform garbage collection on chunks as they see fit
*without coordinating with other peers*.  In practice, this ought to
keep the append-only logs for any individual partition quite small and
manageable; synchronizing the state of any given partition from
scratch shouldn't take very long.

Messages are sent between peers over the [OCapN](https://ocapn.org)
protocol, which handles the burden of secure message transport.  Chat
messages can be transported over any medium with an associated OCapN
netlayer, whether that be Tor, an E2EE relay, or something else.

The chat network is [Byzantine fault
tolerant](https://en.wikipedia.org/wiki/Byzantine_fault).  Any number
of Byzantine peers (run by our eternal adversary, Mallet) can be in
the network, but as long as Alice and Bob can directly connect to each
other, or indirectly connect through Carol, the well-behaved peers
will eventually converge to the correct state.  This is achieved
through content-addressing and signing of messages, much like Git, as
described in [Martin Kleppmann's "Making CRDTs Byzantine Fault
Tolerant"](https://dl.acm.org/doi/10.1145/3517209.3524042).  SHA-256
was chosen for the hash function and ed25519 for signatures.

The security implications of sharing a capability to a synchronized
data structure are worth analyzing in some detail.  If Alice, Bob, and
Carol are peers in the network, then sending Alice a message means
indirectly sending Bob and Carol messages, too.  Each presence of the
chat room is *co-equal* with all other presences!  As a consequence,
we cannot perform administration in a centralized manner like we could
if there was a single canonical chat room living on a single machine.

For example, if Mallet can propagate messages through Bob and Carol
(because Mallet holds a capability to both), then both Bob and Carol
must each revoke their respective capability for Mallet in order to
prevent Mallet from sending messages to the chat room.  While we could
also choose to propagate some information through a separate channel
saying that Mallet's messages should be ignored/hidden from users (and
we can, keep reading), it doesn't change the fundamental truth that
Mallet has write capabilities until such a time that all of the
previously granted capabilities have been revoked.  Even if Bob and
Carol revoke Mallet's capabilities, Alice could still hand Mallet
another capability.  [Preventing delegation is
impossible](http://erights.org/elib/capability/delegations.html) in
any access control model and the design of Brassica Chat reflects this
truth.

That said, having a capability to write to the chat log does not mean
that a peer has the privilege to do whatever they'd like to the shared
state of the room.  Certificate capabilities or "zcaps" (inspired by
[zcap-ld](https://w3c-ccg.github.io/zcap-spec/)) are used for a
limited (when compared to ocaps) form of access control for events
within the chat log CRDT.  The initiator of the chat room has the
privilege of being the root signer for all certificates used in the
chat room.  Privilege is delegated by the formation of certificate
chains that bottom out at the root certificate.  These certificate
chains *do not* prevent valid CRDT events for invalid chat room
operations (according to the privilege encoded in the certificate)
from being written to the chat event log because there is no central
peer from which to enforce such a policy and the certificate state is
also eventually consistent.  Instead, certificate capabilities specify
the rules by which well-behaved clients should *interpret* the events
that have occurred.

![](https://files.spritely.institute/images/blog/brassica-chat-zcap-diagram.png)

For example, Bob can commit an event that edits the contents of
Carol's post, but if the certificate Bob used for that operation does
not grant the capability to edit posts authored by Carol then that
edit will not be made user-visible (and the presence of such an event
in the log could be surfaced by the application as a means to hold Bob
accountable if this becomes a pattern).  When a client syncs a new
certificate, it must reinterpret the chat log with the current set of
policies in order to render the most correct view to the user
according to the information currently available.

Putting object capabilities and certificate capabilities together, we
can re-examine the case of Mallet in the chat room.  To remove Mallet,
a sufficiently privileged peer can first deploy a certificate
capability update to "soft block" and do some damage control with
regards to what users see of Mallet in their chat clients.  Meanwhile,
the group can work to revoke the relevant object capabilities in order
to prevent Mallet from reading/writing to the chat room entirely.  If
this proves to be socially divisive the group may permanently fork and
that's okay!  This also has UI/UX implications that are out of scope
for this experiment.  Without careful design, users could get the
mistaken impression that Mallet has been kicked out of the chat room
when, in fact, Mallet is only in the interim soft blocked state and
can still see what everyone is doing.

The overall security goal for this experiment is to prevent Mallet
from irreparably destroying the shared state of the chat room, to the
best of our ability, and additionally provide a means of holding
Mallet accountable for anti-social/malicious actions that the system
is technically incapable of preventing.  We try keep it simple: If
Alice has an unrevoked object capability to write messages to Bob's
chat log then she can write any message she would like to the chat
log.  Everything layered on top, like the certificates, is for the
sake of the users and admins to have some control over the *eventual
view* of that chat log in well-behaved clients.  Is this good enough?
We're in search of the right balance between eventual and strong
consistency.

Finally, note that this prototype is focused on exploring the core of
a minimally viable p2p chat built on capability security principles
without too many glaring security flaws (hopefully).  This is not
production software.  We did not concern ourselves with optimal
bandwidth, memory, nor disk space usage.

## gay://chat overlay

This fork adds an experimental `gay://chat` overlay for using Brassica
Chat as a capability-secure shared-protention substrate.  Structured
`gay-event` values can be posted into the existing CRDT log without
changing the event DAG format.  The overlay currently models:

* observations, protentions, feedback, obstructions, experiments,
  results, decisions, retrospectives, petnames, ports, grants,
  revocations, and scores;
* colored interfaces such as domain, phase, role, sensitivity, and
  gluing status;
* worldview export to `worldview/current.md` and
  `worldview/consensus-topos.json`.

Run the overlay-only smoke tests with:

```sh
./tests/run-gay-tests.sh
```

Those tests require only Guile and do not require a live OCapN relay.
The full Brassica chat runtime still requires the original Goblins/Fibers
runtime described above.

## Areas of further research/development

* Ergonomic UI/UX for the complexity introduced by decentralization
  and eventual consistency.  This prototype has no UI for managing
  certificate capability chains and the privilege encoded within them.

* Permanent deletion.  Right now, there's just a soft-delete flag on
  messages within the chat log due to the append-only, monotonic
  nature of CRDTs.  But what if Mallet were to post something illegal
  to the chat that should be permanently deleted?  Deleting from a
  CRDT is tricky and would require coordination amongst the peers,
  making it impractical.  Instead, the chat log CRDT should be
  modified such that message content is never stored within by using a
  content-addressed identifier that points to the actual message
  content in some mutable data store.  That way, clients are free to
  delete message content without coordination.

* Preventing new members from reading past messages.  This can be
  achieved with capability attenuation: Alice gives Bob a capability
  to read/write to chat log partitions >= a specific timestamp.  Note
  that since [delegation cannot be
  prevented](http://erights.org/elib/capability/delegations.html),
  Carol could give Bob a capability that provides access to the full
  chat history (just like Carol could forward Bob all of a Signal
  chat).  The partitioning strategy limits the granularity of
  attenuation, however.  If partitions are for 30 minutes chunks then
  Alice can attenuate Bob's access to messages newer than 12:00 or
  12:30 but not 12:45.

* Rotation of user identity keys.  We deliberately left this out to
  keep the scope of this experiment manageable.  This work is best
  left to prototypes of Spritely subproject "Navi".

* Multi-device support.  There is rudimentary support for this in the
  behind-the-scenes relay implementation but there's more work to be
  done.  A non-goal is to implement store-and-forward messaging, which
  would introduce significant cryptographic complexity such as key
  agreement and ratcheting.

* Distributed naming.  All names displayed in the UI are self-proposed
  names.  A [petname
  system](https://files.spritely.institute/papers/petnames.html)
  should be added.
