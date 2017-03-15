# Monad Transformer
## Problem
In most enterprise applications, we have many db calls. At work we have precise protocol for our queries :
- all write queries must return a `Future[Either[E, s]]`
- all read queries must return a `Future[Option[S]]` or `Future[List[S]]`.

Since some action involve more than one write queries and that we often don't want to trigger a second query if the first failed, instead we'd like to chain them, that is doing one after another. We want to chain them, but only if the `Future` is successful and the `Either` is right. For exemple :

```scala
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

type Error = String

def query1: Future[Either[Error, Int]] = Future.successful(Right(42))
def query2: Future[Either[Error, Int]] = Future.successful(Right(42))
def query3: Future[Either[Error, Int]] = Future.successful(Right(42))

query1 flatMap {
  case Right(_) => query2 flatMap {
    case Right(_) => query3
    case Left(err) => Future.successful(Left(err))
  }
  case Left(err) => Future.successful(Left(err))
}
```

It works, but it is verbose. To prevent such case of nested flatMap/map, scala provide for comprehension, but we can't use it here. In the next example, our for comprehension flatMap to the next line if the future is successful, be we want it to flatmap only if the future is successful and its result is an `Either.Right` :

```scala
def queryError: Future[Either[Error, Int]] = Future.successful(Left("Error happened"))

for {
  _ <- query1
  _ <- queryError
  _ <- query2
} yield 42
```

It return a successful `Future` while `queryError` returned a `Left`, that's not what we expect ! That's where we need a new abstraction called Monad Transformer.

## Solution
### Composing `Future` of `Either`
In order to use for comprehension, we need one structure which handle the functions `map` and `flatMap`. Let's call it `FutureEitherT` for "future of either Transformer". This structure take a 'Future[Either[E, S]]' and flatMap/map with another `FutureEitherT` only if the future is successful and the either is right.

```scala
case class FutureEitherT[E, S](value: Future[Either[E, S]]) {
  def map[S2](f: S => S2): FutureEitherT[E, S2] = FutureEitherT(value.map {
    case Left(err) => Left(err)
    case Right(s) => Right(f(s))
  })

  def flatMap[S2](f: S => FutureEitherT[E, S2]): FutureEitherT[E, S2] = FutureEitherT(value.flatMap {
    case Left(err) => Future.successful(Left(err))
    case Right(s) => f(s).value
  })
}
```

Let' test it :

```scala
for {
  _ <- FutureEitherT(query1)
  _ <- FutureEitherT(queryError)
  result <- FutureEitherT(query2)
} yield result
```

We recieved `FutureEitherT[Error, Int] = FutureEitherT(Future(Success(Left(Error happened))))` which is what we were expecting ! But what if we want to chain, not Future, but `Option` of `Either` ? Or any custom type which contain value when that value is an `Either` ? Let's abstract out `FutureEitherT` so that we could replace `Future` by any other type.

### Composing monads
Not every type is eligible for the `Either` composition chain. First, it must contain value of type either, then we have to implement the `map` and `flatMap` function for it. This looks like the definition of a monad.

#### Monad definition
A monad is a type constructor (functor) which associate `flatMap` and `unit` (`pure` in cats) to the type (type `F`) :

```scala
trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
}
```

There is other definitions but this is not the point of this article.

#### The `EitherT` monad transformer
We are trying to abstract our `FutureEitherT` for any type which is associated with a monad. First let's write our future monad :

```scala
implicit val futureMonad = new Monad[Future] {
  def unit[A](a: => A): Future[A] = Future.successful(a)
  def flatMap[A, B](ma: Future[A])(f: A => Future[B]): Future[B] = ma flatMap f
}
```

And then our `EitherT` structure which must implement `map` and `flatMap` so that we can use for comprehension with them :

```scala
case class EitherT[F[_], E, S](value: F[Either[E, S]])(implicit monad: Monad[F]) {
  def map[S2](f: S => S2): EitherT[F, E, S2] = EitherT(monad.map(value) {
    case Right(s) => Right(f(s))
    case Left(err) => Left(err)
  })

  def flatMap[S2](f: S => EitherT[F, E, S2]): EitherT[F, E, S2] = EitherT(monad.flatMap(value) {
    case Right(s) => f(s).value
    case Left(err) => monad.unit(Left(err))
  })
}
```

Let's test it :

```scala
val future = (for {
  _ <- EitherT(query1)
  _ <- EitherT(queryError)
  _ <- EitherT(query2)
} yield ()).value
```

Our result is : `EitherT[Future, Error, Unit] = EitherT(Future(Success(Left(Error happened))))`. This is what was expected.

#### The `OptionT` Monad transformer
We could do the same for monadic type which contain `Option` values :

```scala
case class OptionT[F[_], S](value: F[Option[S]])(implicit monad: Monad[F]) {
  def flatMap[S2](f: S => OptionT[F, S2]): OptionT[F, S2] = OptionT(monad.flatMap(value){
    case Some(s) => f(s).value
    case None => monad.unit(None)
  })

  def map[S2](f: S => S2): OptionT[F, S2] = OptionT(monad.map(value) {
    case Some(s) => Some(f(s))
    case None => None
  })
}
```

### Synthax sugars
This is great but there is still some useless verobsity that deserved to be cleared. For instance, if I have a query which return a future of raw value (for read queries for instace), and I want to chain it with write queries, I must write this :

```scala
def readQuery: Future[String] = Future.successful("Something extracted from DB")
for {
  _ <- EitherT(query1)
  _ <- EitherT[Future, Error, String](readQuery.map(Right.apply))
} yield ()
```

We can shorten this using the [pimp my library](https://coderwall.com/p/k_1jzw/scala-s-pimp-my-library-pattern-example) pattern :

```scala
implicit class FutureEitherToEitherT[E, S](f: Future[Either[E, S]]) {
  def toEitherT: EitherT[Future, E, S] = EitherT(f)
}
implicit class FutureOptionToEitherT[S](f: Future[Option[S]]) {
  def optToEitherT[E](err: E): EitherT[Future, E, S] = EitherT(f.map(_.toRight(err)))
}
implicit class FutureToEitherT[S](f: Future[S]) {
  def valToEitherT[E]: EitherT[Future, E, S] = EitherT[Future, E, S](f.map(Right.apply))
}
```

Now we can write :

```scala
for {
  _ <- query1.toEitherT
  _ <- readQueryOption.optToEitherT("Oups")
  _ <- readQuery.valToEitherT
} yield ()
```

## Conclusion
This is an article which explore the [cats EitherT](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/data/EitherT.scala) class. We use it work and it was very usefull and I recommand using it.
