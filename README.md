# Brassica Chat

This is an experimental prototype for a p2p, eventually consistent,
"good enough" chat application built with
[Goblins](https://spritely.institute/goblins).

## Technical overview

Chat rooms are implemented using operation-based
[CRDTs](https://crdt.tech) with causal delivery order (concurrent
operations commute).  Each node participating in the chat maintains a
copy of the chat event log (a directed acyclic graph).  Together, the
nodes form a single abstract chat room actor.  In object capability
jargon, chat rooms can be thought of as an
[unum](https://habitatchronicles.com/2019/08/the-unum-pattern/) where
each node maintains a presence and messages are propagated via
broadcast.

Rather than using one giant CRDT for the entirety of a chat room's
history, the chat log is partitioned by time.  Each partition covers
some `period` number of seconds of real time and is represented as its
own distinct CRDT.  All nodes must use the same `period` value in
order to converge properly.  This partitioning strategy allows each
node to perform garbage collection on chunks as they see fit.  In
practice, this ought to keep the append-only logs for any individual
chunk quite small and manageable, and having to rebuild the state of
any given chunk from scratch shouldn't take very long.

Messages are sent between nodes over the [OCapN](https://ocapn.org)
protocol, which handles the burden of secure message transport.  Chat
messages can be transported over any medium with an associated OCapN
netlayer, whether that be Tor, an E2EE relay, or something else.

The security implications of sharing a capability to a chat room are
rather large.  If Alice, Bob, and Carol are nodes in the network, then
sending Alice a message means indirectly sending Bob and Carol
messages, too.  Each presence of the chat room is *co-equal* with all
other presences!  As a consequence, we cannot perform administration
in a centralized manner like we could if there was a single canonical
chat room living on a single machine.  Revocation, for example, is now
a communal effort.  If Mallet can propagate messages through Bob and
Carol (because Mallet holds a capability to both), then both Bob and
Carol must revoke their respective capabilities in order to prevent
Mallet from sending messages to the chat room in the future.  While we
could also choose to propagate some information through a separate
channel saying that Mallet's messages should be ignored/hidden from
users (and we can, keep reading), it doesn't change the fundamental
truth that Mallet has write capabilities until such a time that all of
the previously granted capabilities have been revoked.  Thus, the
formation of *complete* networks, where each node holds a capability
to message every other node, is *strongly discouraged* in this design.
How strongly/weakly connected a node is can be thought of as the
digital representation of how trusted the operator of that node is in
the real world social group.  The more strongly connected a node is,
the harder it becomes to remove that node later if the social dynamic
changes.  There is a tension between the risk imposed by strongly
connected peers and the desire for maximum availability of the chat
room.  The UI/UX implications of this are out of scope for this
experiment.

The chat network is [Byzantine fault
tolerant](https://en.wikipedia.org/wiki/Byzantine_fault).  Any number
of Byzantine nodes (run by our eternal adversary, Mallet) can be in
the network, but as long as Alice and Bob can directly connect to each
other, or indirectly connect through Carol, the well-behaved nodes
will eventually converge to the correct state.  This is achieved
through content-addressing and signing of messages, much like Git, as
described in [Martin Kleppmann's "Making CRDTs Byzantine Fault
Tolerant"](https://dl.acm.org/doi/10.1145/3517209.3524042).  SHA-256
was chosen for the hash function and ed25519 for signatures.

**(Note: this paragraph is still TODO)** Certificate capabilities are
used for a limited, presentation-only form of access control within
the chat log CRDTs.  The initiator of the chat room has the privilege
of being the root signer for all certificates used in the chat room.
These certificates *do not* prevent valid CRDT events for invalid chat
room operations (according to the certificate policy) from being
written to the chat event log because there is no central hub from
which to enforce such a policy.  Instead, they specify the rules by
which well-behaved clients should *interpret* the events that have
occurred.  For example, Bob can commit an event that edits the
contents of Carol's post, but if the certificate Bob used for that
operation does not grant the capability to edit posts authored by
Carol then that edit will not be made user-visible (and the presence
of such an event in the log could be surfaced by the application and
lead to Bob being removed from the group).  When a node becomes aware
of a new certificate, it must reinterpret the chat log with the
current set of policies in order to render the most correct view to
the user according to the information available on the local replica.

Putting object capabilities and certificate capabilities together, we
can re-examine the case of Mallet being in the chat room.  To remove
Mallet, an admin can first deploy a certificate capability update to
"soft block" and do some damage control with regards to what users see
of Mallet in their chat clients.  Meanwhile, the group can work to
revoke the relevant object capabilities in order to prevent Mallet
from reading/writing to the chat room entirely.  If this proves to be
socially divisive the group may permanently fork and that's okay!
This also has UI/UX implications that are out of scope for this
experiment.  Without careful design, users could get the mistaken
impression that Mallet has been kicked out of the chat room when, in
fact, Mallet is only in the interim soft blocked state and can still
see what everyone is doing.  If Mallet was granted admin access then
the only recourse in this architecture is to revoke all relevant
object capabilities and clean up the mess left behind.  Who watches
the watchmen?

The overall security goal for this experiment is to prevent Mallet
from irreparably destroying the shared state of the chat room, to the
best of our ability, and additionally provide a means of holding
Mallet accountable for anti-social/malicious actions that the system
is technically incapable of preventing.  One of the major anti-goals
of this experiment is to avoid the situation where we have encoded
significant access control like revocation into the CRDTs and have to
handle tricky situations such as two admins concurrently revoking each
other's access.  Instead, we keep it simple: If Alice has an unrevoked
object capability to write messages to Bob's chat log then she can
write any message she would like to the chat log.  Everything layered
on top, like the certificates, is for the sake of the users and admins
to have some control over the *eventual view* of that chat log in
well-behaved clients.  Is this good enough?  We're in search of the
right balance between eventual and strong consistency.

Finally, note that this prototype is focused on exploring the core of
a minimally viable p2p chat built on capability security principles
without too many glaring security flaws (hopefully).  This is not
production software.  We did not concern ourselves with optimal
bandwidth, memory, nor disk space usage.

## Areas of further research/development

* Ergonomic UI/UX for the complexity introduced by decentralization
  and eventual consistency.

* A mechanism for rewriting history.  If Mallet writes truly terrible
  content to the append-only chat log, it's stuck in there even if
  it's hidden from the user.  Layering some synchronization on top
  ought to fix this.  A version counter (or something similar) could
  be added to each chunk of history and peers could agree to roll to a
  new version where the event history has been rewritten to remove a
  subset of the original events.  This would be like how Git history
  is append-only but branch names are mutable pointers.

* Preventing new members from reading past messages.  This should be
  an option like it is in other secure chat programs.  A simple,
  perhaps naive, way to handle this with ocaps is to give the new user
  a capability that prevents reads/writes/syncs to partitions older
  than the time the capability was issued.  This introduces some
  complexity and potential confusion if another member of that chat
  grants the newcomer a capability to access the complete chat
  history.  If an ocap approach isn't sufficient then a solution
  involving additional cryptography would need to be used instead.

* Rotation of user identity keys.  We deliberately left this out to
  keep the scope of this experiment manageable.

## Try

Get the dependencies by running `guix shell -m manifest.scm`.

Run `make demo` to run the simulated chat test.
