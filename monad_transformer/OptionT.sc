trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}


case class OptionT[F[_], S](value: F[Option[S]]) {
  def flatMap[S2](f: S => OptionT[F, S2])(implicit monad: Monad[F]): OptionT[F, S2] = OptionT(monad.flatMap(value){
    case Some(s) => f(s).value
    case None => monad.unit(None)
  })
}

def optionTMonad[F] = new Monad[({type l[S] = OptionT[F, S]})#l] {
  def unit[A](a: => A): OptionT[F[A], A] = OptionT(Some(a))
  def flatMap[A,B](ma: OptionT[A])(f: A => OptionT[B]): OptionT[B] = ma.flatMap(f)
}

def optionTMonad[F[_]](implicit monad: Monad[F]) = new Monad[({type l[S] = OptionT[F, S]})#l] {
  def unit[A](a: => A): OptionT[F, A] = OptionT(monad.unit(Option(a)))
  def flatMap[A,B](ma: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = ma.flatMap(f)
}

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

trait EitherContainer[E, S] {
  def map[S2](f: S => S2): EitherContainer[E, S2]
  def flatMap[S2](f: S => EitherContainer[E, S2]): EitherContainer[E, S2]
}

implicit case class FutureEitherContainer[E, S](future: Future[Either[E, S]]) extends EitherContainer[E, S] {
  def map[S2](f: S => S2): FutureEitherT[E, S2] = FutureEitherT(value.map {
    case Left(err) => Left(err)
    case Right(s) => Right(f(s))
  })

  def flatMap[S2](f: S => FutureEitherT[E, S2]): FutureEitherT[E, S2] = FutureEitherT(value.flatMap {
    case Left(err) => Future.successful(Left(err))
    case Right(s) => f(s).value
  })
}
