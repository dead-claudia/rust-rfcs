- Feature Name: `io_splice`
- Start Date: 2023-01-09
- RFC PR: [rust-lang/rfcs#0000](https://github.com/rust-lang/rfcs/pull/0000)
- Rust Issue: [rust-lang/rust#0000](https://github.com/rust-lang/rust/issues/0000)

# Summary
[summary]: #summary

`std::io::copy` provides a way to do a complete copy, but blocks indefinitely until it's completed. This RFC proposes a splice trait to allow for partial copies and a way to specialize common pairs of them.

# Motivation
[motivation]: #motivation

`std::io::copy`, introduced in [RFC 517](https://rust-lang.github.io/rfcs/0517-io-os-reform.html), is very useful for quick and dirty copying from a readable to a writable. However, it does have some serious limitations:

- You can intercept cases where writes would fail, but signal interrupts are ignored. This is specifically bad when using file descriptor polling outside async runtimes, because it breaks signal handling.
- The current source code is hard-coded to optimize only for native file handles and (as of [RFC 2930](https://rust-lang.github.io/rfcs/2930-read-buf.html)) `BufRead`. It's not extensible to other types, and more importantly, userland types can't optimize for each other, either.
- As it loops indefinitely until completed, it cannot be used in async runtimes.

There's also a number of outstanding performance gaps:

- Copying from a `Vec<u8>` or `VecDeque<u8>` to a file or socket currently involves an intermediate copy, one that's very easily avoidable.
- `VecDeque<u8>` itself only tries to copy one half at a time, when in many cases it could just use a single vectored write. This would make it a lot more viable as a general byte buffer when paired with a `&[u8]` reader or `&mut [u8]` writer.

This RFC aims to both add the missing feature and fix the specialization issue by adding a way to "splice" from a readable to a writable.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The following methods will exist on `std::io::Read` and `std::io::Write`:

```rust
trait Splicer<'a, R, W> {
    /// Create a new splicer.
    fn new(reader: &'a mut R, writer: &'a mut W) -> io::Result<Self>;

    /// Copy some data from this `Read`er to a given `Write`r, and return the
    /// number of bytes copied. There may still be more bytes that may be
    /// copied in a later call, so you usually want to call this repeatedly.
    ///
    /// A return value of `Ok(0)` means there are no more bytes to read (or
    /// copy to the `Write`r).
    ///
    /// It should be functionally equivalent to this code:
    /// ```
    /// let mut buf = vec![0; MAX_SIZE];
    /// let bytes_read = self.read(&mut buf)?;
    /// dest.write(&buf[0..bytes_read])
    /// ```
    ///
    /// Implementations should consider using `splicer_from` where possible.
    /// In-memory `Write`rs will generally benefit, since you can just copy
    /// the bytes directly to its underlying buffer.
    fn splice(&mut self) -> io::Result<usize>;

    /// Get the underlying `Read`er from this splicer. Necessary to avoid
    /// overlapping mutable borrows.
    fn reader(&mut self) -> &'a mut R;

    /// Get the underlying `Write`r from this splicer. Necessary to avoid
    /// overlapping mutable borrows.
    fn writer(&mut self) -> &'a mut W;
}

trait Read {
    /// The type of the splicer struct to use. It's optional, and defaults to a
    /// splicer that simply reads and writes in each step.
    type Splicer<'a, W: io::Write>: Splicer<'a, Self, W> = DefaultSplicer<'a, Self, W>;

    /// Create a splicer to copy data in sections. This mainly exists to allow
    /// creating optimal splicers from `dyn io::Read`s.
    fn splicer_to<'a, W: io::Write>(
        &'a mut self,
        dest: &'a mut W
    ) -> io::Result<Self::Splicer<'a, W>> {
        Self::Splicer::new(self, dest)
    }

    /// Determines if this `Read`er has an efficient `splicer_to` implementation.
    ///
    /// If a `Read`er does not override the default `splicer_to` implementation,
    /// code using it may want to avoid the method altogether and use buffering
    /// or similar for higher performance.
    ///
    /// The default implementation returns `false`.
    fn can_efficiently_splice_to(&self) -> bool;
}

trait Write {
    /// Like `io::Read::Splicer`, but allows specializing in the other direction.
    type Splicer<'a, R: io::Read>: Splicer<'a, R, Self> = R::Splicer<'a>;

    /// Create a splicer to copy data in sections. This mainly exists to allow
    /// creating optimal splicers from `dyn io::Write`s.
    fn splicer_from<'a, R: io::Read>(
        &'a mut self,
        src: &'a mut R
    ) -> io::Result<Self::Splicer<'a, R>> {
        Self::Splicer::new(self, dest)
    }

    /// Determines if this `Write`r has an efficient `splicer_from` implementation.
    ///
    /// If a `Write`r does not override the default `splicer_from` implementation,
    /// code using it may want to avoid the method altogether and either write to
    /// it directly or try `src.splicer_to(&mut self)` instead.
    ///
    /// The default implementation returns `false`.
    fn can_efficiently_splice_from(&self) -> bool;
}
```

The function `std::io::splicer` sugars over all of this, trying `splicer_from` where optimized before falling back to `splicer_to` (which itself falls back to a simple read → write sequence). `std::io::copy` simply calls that splicer's `splice` method repeatedly until it either errors (for reason other than signal interrupt) or hits the end.

This can be used in a loop for, say, piping between sockets. Unlike `std::io::copy`, this accounts for blocking.

```rust
let splicer = std::io::splicer(&mut src_socket, &mut dest_socket);

loop {
    match splicer.splice() {
        Ok(0) => break,
        Ok(_) => continue,
        Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
        Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
            wait_for_ready([splicer.reader(), splicer.writer()])?;
            continue;
        },
        Err(e) => return Err(e),
    }
}
```

This can also be used in single-shot scenarios, like in async copies.

```rust
// Very high-level pseudocode.
pub async fn copy<R, W>(reader: &mut R, writer: &mut W) -> io::Result<()>
where
    R: io::Read + Unpin + ?Sized,
    W: io::Write + Unpin + ?Sized,
{
    let splicer = std::io::splicer(reader, writer);

    std::futures::poll_fn(move |cx| {
        match splicer.splice() {
            Err(e) if e.kind() == io::ErrorKind::Interrupted => {
                AsyncRuntime::check_signal();
                AsyncRuntime::wait_for_readable(splicer.reader(), cx);
                AsyncRuntime::wait_for_writable(splicer.writer(), cx);
                Poll::Pending
            }
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                AsyncRuntime::wait_for_readable(splicer.reader(), cx);
                AsyncRuntime::wait_for_writable(splicer.writer(), cx);
                Poll::Pending
            }
            result => Poll::Ready(result),
        }
    }).await
}
```

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

The following methods will be added to `Read` and `Write`:

- `Splicer::splice` to combine a read and write into a single operation, based on the reader. `Write::splicer_from` mirrors this from the writer, and normally just delegates to the reader's method.
- `Read::can_efficiently_splice_to` and `Write::can_efficiently_splice_from` allow introspection into whether `Read::splicer_to` and `Write::splicer_from` offer an optimized splicer, much like how `Read::is_read_vectored` and `Write::is_write_vectored` allow introspection into whether `Read::read_vectored` and `Write::write_vectored` are optimized.

Additionally, a couple structs will exist to expose what's currently offered for `std::io::copy`:

- `DefaultSplicer` to perform the default read → write loop. Ideally, it should use either a ring buffer or a simple state machine to ensure at most one read and at most one write is attempted each run.
- `OptimalSplicer` to allow `std::io::splicer` to delegate to whatever's determined to be the most optimal splicer for the given reader and writer.
  - If the writer's `writer.can_efficiently_splice_from()` returns `true`, prefer the writer's splicer (via `writer.splicer_from(reader)`).
  - If the writer's `writer.can_efficiently_splice_from()` returns `false`, fall back to the reader's splicer (via `reader.splicer_from(writer)`).

And of course, `std::io::splicer` will be added per above.

```rust
trait Splicer<'a, R, W> {
    fn new(reader: &'a mut R, writer: &'a mut W) -> io::Result<Self>;
    fn splice(&mut self) -> io::Result<usize>;
    fn reader(&mut self) -> &'a mut R;
    fn writer(&mut self) -> &'a mut W;
}

// Implementation elided for brevity.
pub struct DefaultSplicer<'a, R, W> { ... }

impl<'a, R: io::Read, W: io::Write> Splicer<'a, R, W> for DefaultSplicer<'a, R, W> {
    fn new<'a>(reader: &'a mut R, writer: &'a mut W) -> io::Result<Self> { ... }
    fn splice(&mut self) -> io::Result<usize> { ... }
    fn reader(&mut self) -> &'a mut R { ... }
    fn writer(&mut self) -> &'a mut W { ... }
}

// Implementation's pretty obvious, but elided for brevity.
pub struct OptimalSplicer<'a, R, W> { ... }

impl<'a, R: io::Read, W: io::Write> Splicer<'a, R, W> for OptimalSplicer<'a, R, W> {
    fn new<'a>(reader: &'a mut R, writer: &'a mut W) -> io::Result<Self> { ... }
    fn splice(&mut self) -> io::Result<usize> { ... }
    fn reader(&mut self) -> &'a mut R { ... }
    fn writer(&mut self) -> &'a mut W { ... }
}

trait Read {
    type Splicer<'a, W: io::Write>: Splicer<'a, R, Self> = DefaultSplicer<'a, R, Self>;

    fn splicer_to<'a, W: io::Write>(
        &'a mut self,
        dest: &'a mut W
    ) -> io::Result<Self::Splicer<'a, W>> {
        Self::Splicer::new(self, dest)
    }

    fn can_efficiently_splice_to(&self) -> bool {
        false
    }
}

trait Write {
    type Splicer<'a, R: io::Read>: Splicer<'a, R, Self> = R::Splicer<'a, Self>;

    fn splicer_from<'a, R: io::Read>(
        &'a mut self,
        src: &'a mut R
    ) -> io::Result<Self::Splicer<'a, R>> {
        Self::Splicer::new(self, dest)
    }

    fn can_efficiently_splice_from(&self) -> bool {
        false
    }
}
```

> Why does `OptimalSplicer` prefer `Write::splicer_from`? It's much easier for a writer to control its reader than a reader to control its writer, and it defaults to optimal read patterns in more cases.

There would be a number of native specializations here to improve performance:

- `File` and `TcpStream` would have `splicer_to` use the optimal syscalls for other descriptors. If `W::has_splice_into()` returns `true`, they'd have `splicer_to` call `dest.splice_into(|bufs| src.read_vectored(bufs))` instead.
  - This allows it to do the optimal thing for `Vec<u8>`, `VecDeque<u8>`, and related destinations without having to specially code for it.

- `Vec<u8>` would provide an appropriate `splicer_from` implementation exposing only a single `IoSliceMut`.

- `VecDeque<u8>` would make `splicer_to` invoke `dest.write_vectored(..)` on the writer with its separate head and tail slices. It'd also provide an appropriate `splicer_from` implementation targeting the appropriate head and tail sections.
  - The `splicer_from` implementation could avoid allocating if the overflow buffer isn't touched, speeding it up dramatically.
  - This would avoid intermediate copies, and it'd reduce the number of needed syscalls for copying `VecDeque<u8>`s into files by one, making them much more viable as temporary network buffers.
  - This would make `std::splicer::new(&mut vec_deque, mut vec/slice/etc)?.splice()?` and vice versa as efficient as copying the slices from `vec_deque.as_slices()` manually, but with a lot less code. If LLVM notices and merges the length write -> read pairs in the loop, it may also be able to remove the outer loop, making even `std::io::copy(&mut vec_deque, &mut vec/slice/etc)` zero-cost.

- `&[u8]` and `&mut [u8]` would provide appropriate `splicer_to` implementations to sidestep the ceremony and just directly read and write the data.

- `BufReader` would work similarly to `Vec`, using its internal buffer as applicable.

`std::io::copy` would be changed to simply repeatedly call `Splicer::splice` until either an error (other than the usual `ErrorKind::Interrupted`) is returned or `Ok(0)` is returned.

```rust
pub fn copy<R: ?Sized, W: ?Sized>(reader: &mut R, writer: &mut W) -> io::Result<u64>
where
    R: Read,
    W: Write,
{
    let splicer = OptimalSplicer::new(reader, writer);
    let len = 0;

    loop {
        match splicer.splice() {
            Ok(0) => return Ok(len),
            Ok(written) => len += written as u64,
            Err(e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        }
    }
}
```

Just for show, here's implementations for each of the relevant types and methods for `VecDeque<u8>`.

```rust
struct SpliceFromVecDeque<'a, W> {
    reader: &'a mut VecDeque<u8>,
    writer: &'a mut W,
}

impl<'a, W: io::Write> Splicer<'a, VecDeque<u8>, W> for SpliceFromVecDeque<'a, W> {
    fn new<'a>(reader: &'a mut VecDeque<u8>, writer: &'a mut W) -> io::Result<Self> {
        Ok(Self { reader, writer })
    }

    fn splice(&mut self) -> io::Result<usize> {
        let (head, tail) = self.reader.as_slices();

        let len = self.writer.write_vectored(&[
            IoSliceMut::new(&head),
            IoSliceMut::new(&tail),
        ])?;

        if len != 0 {
            self.reader.drain(..len);
        }

        Ok(len)
    }

    fn reader(&mut self) -> &'a mut VecDeque<u8> {
        self.reader
    }

    fn writer(&mut self) -> &'a mut W {
        self.writer
    }
}

impl io::Read for VecDeque<u8> {
    // ...

    type Splicer<'a, W: io::Write> = SpliceFromVecDeque<'a, W>;

    fn can_efficiently_splice_to(&self) -> bool {
        true
    }
}

struct SpliceToVecDeque<'a, R> {
    reader: &'a mut R,
    writer: &'a mut VecDeque<u8>,
}

impl<'a, R: io::Read> Splicer<'a, R, VecDeque<u8>> for SpliceToVecDeque<'a, R> {
    fn new<'a>(reader: &'a mut R, writer: &'a mut VecDeque<u8>) -> io::Result<Self> {
        Ok(Self { reader, writer })
    }

    fn splice(&mut self) -> io::Result<usize> {
        // The idea here is to read into the spare capacity first, and then grow
        // if needed. This avoids a copy in case the spare capacity provides enough
        // space to read off everything.
        //
        // `VecDeque<u8>`'s `read` implementation should be updated to do the same
        // here.

        // Note: `spare_capacity_mut` isn't actually a real method. This is just
        // pseudocode for getting the extra capacity not included in the vector.
        // Likewise, `set_len` as used later on doesn't actually exist, either.
        let (spare1, spare2) = self.writer.spare_capacity_mut();

        let mut extra = [MaybeUninit::uninit(); DEFAULT_BUF_SIZE];

        let mut bufs = io::ReadBufs::uninit(&mut [
            MaybeUninitIoSliceMut::uninit(spare1),
            MaybeUninitIoSliceMut::uninit(spare2),
            MaybeUninitIoSliceMut::uninit(&mut extra),
        ]);

        let len = self.reader.read_buf_vectored(&mut bufs)?;

        let (slices, _) = bufs.filled();

        unsafe {
            if !slices.is_empty() {
                self.writer.set_len(self.writer.capacity().min(self.writer.len() + len));
                if let Some(last) = slices.last() {
                    self.writer.reserve(last.len());
                    self.writer.extend(&last);
                }
            }
        }

        Ok(len)
    }

    fn reader(&mut self) -> &'a mut R {
        self.reader
    }

    fn writer(&mut self) -> &'a mut VecDeque<u8> {
        self.writer
    }
}

impl io::Write for VecDeque<u8> {
    // ...

    type Splicer<'a, R: io::Read> = SpliceToVecDeque<'a, R>;

    fn can_efficiently_splice_from(&self) -> bool {
        true
    }
}
```

# Drawbacks
[drawbacks]: #drawbacks

**Code motion.** It changes `std::io::copy` substantially, and the existing optimizations will need separately adapted. That entire area will need moderately restructured to accommodate this RFC as written (though it'll most likely be straightforward). Also, it'll require some changes in other areas, especially with `VecDeque`, in order to tap into the potential performance gains there, and so it'll in the end be a rather decent-sized task.

**Increased surface area.** It moderately increases the API surface area and introduces a very complex feature. This is of course one of the usual drawbacks, though this not being a trivial feature certainly makes it a bigger drawback than usual.

**Potential for low adoption.** Library authors might not implement this optimization, even if it could benefit them greatly to do so. It's already uncommon to see `write_vectored` implemented in the wild for userland `Write` types, and even some built-in types like `VecDeque<u8>` don't implement that method even though they could, so it's not unreasonable to expect low adoption for this as well. The type is also a bit boilerplatey and complex to implement, so that may also result in some people not implementing it even if they could.

**Niche benefit.** Much of the benefit of this RFC exists for users using file descriptor polling, and most of these users use frameworks like Tokio and async-std. Frameworks like those implement their own `Read` and `Write` traits, and even `File` structs. This reduces how many people would actually benefit from this in practice.

# Rationale
[rationale]: #rationale

A number of constraints have to be satisfied for this to be viable.

**Efficiently backwards-compatible.** This goes without saying.

**Compatible with trait objects.** As simple as it may seem to go with a trait mirroring `(Read, Write)` tuples, that just doesn't work, as trait objects are pervasive in I/O code. Solutions likewise must not depend on monomorphization or [specialization](https://rust-lang.github.io/rfcs/1210-impl-specialization.html). (And yes, I've looked into it. It would simplify this RFC a *lot*.)

The current implementation of `std::io::copy` relies on specialization. This precludes any of its optimizations from working in the face of `dyn Read`s.

**Performs the minimal number of syscalls per "splice" attempt.** The inability to directly invoke syscalls individually has been asked about a couple times already:

- [#60689: sendfile syscall](https://github.com/rust-lang/rust/issues/60689)
- [URLO: Towards a more perfect RustIO (comment)](https://users.rust-lang.org/t/towards-a-more-perfect-rustio/18570/4)

There are also a number of performance guarantees that rely on the number of syscalls (and thus, related context switches) being low.

**Avoids unnecessary copies.** The reason `read_vectored` exists is to allow efficiently reading into multiple buffers. This is very helpful when writing to and from memory (and is technically optimal in terms of context switches), but this only works for memory to and from descriptors and such. Other APIs like Linux's `splice` and `sendfile` must be used when neither the source nor the destination are in-memory.

**Efficient with in-memory readers and writers.** In-memory readers and writers are very common, as they handle two exceptionally common cases: in-memory buffering and generated output. Both, especially post-monomorphization, need to be fast, and it's very helpful to have them written out in the most efficient way possible.

**Forward-compatible with a future `no_std` variant.** This shouldn't need a redesign to be moved to a future hypothetical `core::io::*` set of traits.

## Alternatives
[alternatives]: #alternatives

I've already considered a number of possible alternatives. There's enough possibilities that, chances are, I've almost certainly not considered *all* of them. I tried to document everything I've considered here, but I've probably also missed a thing or two as well, so feel free to ask!

**Do nothing.** Yes, doing nothing is an option. All of the thousands of words and hundreds of lines of code within this RFC could in fact just be following one massive red herring. Performance-conscious users already implement 99% of this manually, either by combining APIs or by invoking libc (or safe wrappers of it) directly, so clearly, the status quo *is* tenable for enough people that this isn't a *critical* feature.

**Use `splice_to`/`splice_from` methods.** The idea is, instead of a splicer type/trait, simple `splice_to`/`splice_from` methods could exist. It'd be much simpler to implement, and was what I initially drafted most of this up with. There's an elephant in the room that complicates things, though: `std::fs::File`. Some descriptors for `std::fs::File` require some syscalls, and some descriptor types require others, and so one can't just use one syscall to rule them all. There's two ways to address this, neither of which are pretty and only one of which is even doable:

1. Check the descriptor's type on every splice attempt. This obviously flies in the face of the constraint of performing the minimal number of syscalls per "splice" attempt, so it's not really an option.
2. Add a field to `std::fs::File` for storing lazily-loaded metadata. This comes at a risk of slowing down passing owned file references, since they're stored in a single 32-bit object. This *could* be avoided on 64-bit platforms by using a custom representation not unlike what's been done for `std::io::Error`, but that then complicates the definition and its use in various other ways (and requires a manual `Drop` implementation). Also, I'm not sure how many people transmute between `i32`/`u32` and `std::fs::File` - this would need a Crater run to check for any potential compatibility problems.

Of this, 2 *is* an option seriously worth considering, and I *almost* reverted the change to a separate "splicer" object because of it. I just decided against because there wasn't much to be gained by doing it, and I wasn't sure about the compatibility or potential memory usage concerns.

**Just add new methods for everything.** This would carry even higher complexity than either this current proposal or the `splicer_to` method above, though, and it doesn't solve the extensibility problem for `std::io::copy`.

**Figure out a way to dispatch based on both the reader and the writer.** Unfortunately, Rust doesn't offer any way to represent multiple dispatch like that directly, and guarding it would be extremely difficult as well. Also, it'd probably result in some major breaking changes to `std::io::copy` that'd be complete non-starters. It *could* be done in a very hacky way by using `(R, W) where R: io::Read, W: io::Write` tuples, but 1. it's clearly extremely hacky (and error-prone) and 2. you'd need even more hacks a là [Castaway](https://crates.io/crates/castaway) to know when to fall back. Plus, in any case, it's not really compatible with trait objects (`dyn io::Read` or `dyn io::Write`), so absent even *more* hacks to fill that gap (and I'm not sure how that'd even be done), it's just not viable.

And no, I didn't skip past this out of lack of trying. Aside from the problem of trait objects, it's *technically* doable. Just far from ideal. (I didn't actually try to fully spec it out, though. I knew it could be done using [specialization](https://rust-lang.github.io/rfcs/1210-impl-specialization.html), and just decided it wasn't worth the effort.)

# Prior art
[prior-art]: #prior-art

Operating system syscalls for zero-copy copies normally take one of a couple things:

- source handle/FD, destination handle/FD, byte count
- source handle/FD, optional source offset, destination handle/FD, optional destination offset, byte count

C libraries also sometimes expose similar, though they vary wildly.

The idea is `std::io::copy` and the like (with their broad prior art) is the common sugar, and this is just what those normally use under the hood to accelerate themselves.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

**Naming?** The original `splice_to` name was admittedly just a shot in the dark for a decent name, and `splicer_to` is just derived from that. My initial idea, back when I was envisioning this as just a method, was just `splice`. Both are inspired by Linux's `splice` pipe syscall, but of course this is pretty easy to bikeshed.

`send` is another good option, since one could view it as directing data to be sent from a source to a destination.

**How exactly should the secondary dispatch work?** In practice, these syscalls are generally defined in terms of source + destination pairs, and so understanding how the second level of dispatch would work would be critical to implementing this.

**How much is performance actually improved?**

FD to and from FD calls probably won't change much since they're already optimized for by `std::io::copy`. In-memory to and from FD calls will be a different story, though.

- For `Vec<u8>`, I expect a mild speed-up versus `std::io::copy` for larger reads/writes and a wash for smaller reads/writes. Performance should be consistent the whole time since each "splice" corresponds to a single `write` syscall.
- For `VecDeque<u8>`, I expect it to be a near wash for smaller reads/writes (as they're likely to be contiguous), but a significant speed-up for highly-utilized deques that are commonly not contiguous. I also expect much more consistent performance over the existing `std::io::copy` algorithm due to there only being a single vectored write instead of potentially two separate non-vectored writes.

**Is it okay to add fields to `std::fs::File` and friends?** This is the main question holding back the `splice_to`/`splice_from` methods alternative. If it's workable, that alternative is much more attractive as it's a much smaller feature to add.

# Future possibilities
[future-possibilities]: #future-possibilities

**`impl<E: iter::Extend<u8>> io::Write for E`.** The idea is to generalize what's already done for `Vec<u8>` so it applies to everything else that implements it, including `VecDeque<u8>` where it's most useful. Such an implementation is pretty simple, with most of it just being the splicer proposed in this RFC.

```rust
impl<E: iter::Extend<u8>> io::Write for E {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.extend(buf);
        Ok(buf.len())
    }

    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        let len = bufs.iter().map(|b| b.len()).sum();
        self.extend_reserve(len);
        for buf in bufs {
            self.extend(buf);
        }
        Ok(len)
    }

    fn is_write_vectored(&self) -> bool {
        true
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.extend(buf);
        Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
```

If you squint hard enough, it looks *very* similar to the `io::Write` implementations for both `Vec<u8>` and `VecDeque<u8>`. Unfortunately, both of those very types in question have buffers that can be read into directly, and that can't be separately optimized for without specialization. (Or, in other words, this in practice is blocked on the [specialization RFC](https://rust-lang.github.io/rfcs/1210-impl-specialization.html) stabilizing.)

**`impl<I: iter::Iterator<u8>> io::Read for I`.** See above - it's effectively the inverse of it. Once monomorphized, a lot of readers should end up optimized into memory copies in LLVM, and this combined with the above would allow `std::io::copy` to be generalized even to those as well. Could be useful to avoid `Vec`s when writing out data - I've already come across some of these personally. Once specialization comes out, `iter::Chain<u8>` and friends could get specialized splicers as well, to mitigate the perf impact there (and allow better use of their internal state machines).

This would have to be a `default impl` just to avoid breaking people, and `VecDeque` itself needs to keep its read specialization, so specialization is probably a blocker here.

**Sized splicing.** All relevant syscalls here accept a maximum length to splice. It's easy to imagine how splicing X bytes would be useful in more advanced cases. However, it's niche enough that I'm hesitant to get behind it.

**Read and write offsets.** Most relevant syscalls here accept optional read and write offsets, and stuff like `pread` and `pwrite` already have broad cross-platform support in some form or another. I intentionally omitted these because Rust's standard library doesn't support this even for simple reads and writes.

**Async read/write/seek traits.** There's already multiple userland async frameworks and runtimes (ex: Tokio, async-std) that have their own traits for this, and features like this `splicer_to` will inevitably have to be supported by them. If this were internalized in Rust somehow with the hooks needed for them to do their needed polling, it'd avoid the drawback regarding low user adoption, since Rust would be the one making sure it's as fast as pragmatically possible.

Separately, it'd also make the language a lot easier to learn and navigate for `async` users, since `std::fs::File` and friends could just implement that trait as desired. Admittedly, I even found it annoying myself having to bounce back and forth so much between Rust's docs and Tokio's docs while also having to deal with the minor (and subtle) inconsistencies between the two.
